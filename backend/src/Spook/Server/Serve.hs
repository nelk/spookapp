{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spook.Server.Serve
    ( startApp
    , app
    , SiteContext(..)
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status (status404)
import Data.Generics.Product (the)
import qualified Network.Wai.Application.Static as Wai
import qualified WaiAppStatic.Types as Wai
import Network.Socket
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust)
import Numeric (showHex)
import Data.Word (Word8, Word16)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import System.Random (randomIO)
import qualified Data.ByteString.Lazy as BsL
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base64.URL as Bs64Url
import Servant
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Data.Monoid ((<>))
import Control.Monad (replicateM, forM_, void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Applicative (liftA)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ask, runReaderT, ReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Lens

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, runMigration, SqlBackend)
import qualified Database.Esqueleto as Es
import Data.Pool (Pool)

import Spook.Common.Model
import Spook.Common.Api (FullApi, Api, AccessControlAllowOriginHeader)
import Spook.Server.Model
import Spook.Server.Data (eventTime)
import Options.Generic as OG


data Params = Params
  { dbHost :: Maybe Text
  , dbPort :: Maybe Int
  , dbUser :: Maybe Text
  , dbPassword :: Maybe Text
  , dbDatabase :: Maybe Text
  , dbPoolSize :: Maybe Int
  , dbSsl :: Bool
  , sitePort :: Maybe Int
  , serveIndexDirectory :: Maybe FilePath
  , serveStaticDirectory :: Maybe FilePath
  , allowCrossOrigin :: Bool
  }
  deriving (Show, Generic)

instance OG.ParseRecord Params

dbConnectInfo :: Params -> ConnectionString
dbConnectInfo Params{..} = Text.encodeUtf8 $ Text.unwords $
  [ "host=" <> fromMaybe "localhost" dbHost
  , "port=" <> (Text.pack $ show $ fromMaybe (5432 :: Word16) (fromIntegral <$> dbPort))
  , "user=" <> fromMaybe "spookapp" dbUser
  , "password=" <> fromMaybe "password" dbPassword
  , "dbname=" <> fromMaybe "spook" dbDatabase
  ] ++ ["sslmode=require" | dbSsl]

data SiteContext = SiteContext
  { siteDbPool :: Pool SqlBackend
  , siteServeIndexDirectory :: Maybe FilePath
  , siteServeStaticDirectory :: Maybe FilePath
  , siteAllowCrossOrigin :: Bool
  } deriving Generic

-- TODO: Lookup servant threading model - how do threads interact with StateT? Make random part of that.

newtype App a = App { runApp :: ReaderT SiteContext Handler a }
  deriving (Functor, Applicative, Monad, MonadReader SiteContext, MonadError ServantErr, MonadIO, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either ServantErr a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . runApp)
  restoreM = App . restoreM

runSql :: Es.SqlPersistT App a -> App a
runSql sql = do
  pool <- view (the @"siteDbPool")
  Es.runSqlPool sql pool

startApp :: IO ()
startApp = do
  params <- OG.getRecord "Spook.app Web Server"
  dbPool <- runStderrLoggingT $ createPostgresqlPool (dbConnectInfo params) (fromMaybe 1 $ dbPoolSize params)
  flip Es.runSqlPool dbPool $ do
    runMigration migrateAll
    insertTestData
  let port = fromMaybe 8080 $ sitePort params
      context = SiteContext
        { siteDbPool = dbPool
        , siteServeIndexDirectory = serveIndexDirectory params
        , siteServeStaticDirectory = serveStaticDirectory params
        , siteAllowCrossOrigin = allowCrossOrigin params
        }

  putStrLn $ "Running on port " ++ show port
  run port $ app context

testData :: [SavedSpook]
testData =
  [ SavedSpook
      { savedSpookToken = "abc12401"
      , savedSpookParent = Nothing
      , savedSpookVisits = 1
      , savedSpookIp = Nothing
      , savedSpookReferrer = Nothing
      , savedSpookChildSpookCount = 0
      , savedSpookVideoUrl = "https://www.youtube.com/embed/oJqc4vByZCc?autoplay=1&rel=0&amp;controls=0&amp;showinfo=0"
      }
  ]

insertTestData :: MonadIO m => Es.SqlPersistT m ()
insertTestData = forM_ testData $ \savedSpook -> do
  maybeDbSpook :: Maybe (Es.Entity SavedSpook) <- Es.getBy $ UniqueToken $ savedSpookToken savedSpook
  when (isNothing maybeDbSpook) $ void $ Es.insert savedSpook

app :: SiteContext -> Application
app context =
 let policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
     corsMiddleware
       | siteAllowCrossOrigin context =
           cors (const $ Just policy) . provideOptions (Proxy :: Proxy Api)
       | otherwise = id
 in logStdoutDev
    $ corsMiddleware
    $ serve (Proxy :: Proxy FullApi)
    $ server context

convertApp :: SiteContext -> App :~> Handler
convertApp cfg = NT (flip runReaderT cfg . runApp)

server :: SiteContext -> Server FullApi
server context = enter (convertApp context) ((getSpookHandler :<|> newSpookHandler) {- :<|> indexHandler -} ) :<|> rawHandler context

getAddrIpAsText :: Maybe Text -> SockAddr -> Maybe Text
getAddrIpAsText (Just realIpHeader) _ | not (Text.null realIpHeader) = Just realIpHeader
getAddrIpAsText _ (SockAddrInet _ ipv4) =
  let (p1, p2, p3, p4) = hostAddressToTuple ipv4
  in Just $ Text.intercalate "." $ Text.pack . show <$> [p1, p2, p3, p4]
getAddrIpAsText _ (SockAddrInet6 _ _ ipv6 _) =
  let (p1, p2, p3, p4, p5, p6, p7, p8) = hostAddress6ToTuple ipv6
      list = [p1, p2, p3, p4, p5, p6, p7, p8]
  in Just $ Text.intercalate ":" $ Text.pack . flip showHex "" <$> list
getAddrIpAsText _ _ = Nothing

-- |Create a base64url-encoded random 128-bit string.
generateToken :: App Text
generateToken = do
  bs :: Bs.ByteString <- liftIO $ liftA Bs.pack $ replicateM 16 (randomIO :: IO Word8)
  let bs64 = Bs64Url.encode bs
  return $ Text.decodeUtf8 bs64

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay _ = Nothing

-- TODO: Store cookie. Can rewatch with it only.
-- TODO: Can only generate new ones once, store if done, return stored ones if same ip, cookie.

getSpookByTokenOrErr :: ServantErr -> Token -> App (Es.Entity SavedSpook)
getSpookByTokenOrErr err token = do
  visitorMay <- getSpookByToken token
  case visitorMay of
    Nothing -> throwError err
    Just spook -> return spook

getSpookByToken :: Token -> App (Maybe (Es.Entity SavedSpook))
getSpookByToken (Token token) = do
  currentTime <- liftIO getCurrentTime
  spookEnts <- runSql $ Es.select $ Es.from $ \savedSpook -> do
    Es.where_ $ savedSpook Es.^. SavedSpookToken Es.==. Es.val token
    return savedSpook
  return $ headMay spookEnts

getSpookHandler :: Token -> App (Either SpookFailure SpookData)
getSpookHandler token = do
  maybeSpook <- getSpookByToken token

  -- Check if already redeemed.
  -- TODO

  result <- case maybeSpook of
    Nothing -> return $ Left SpookDoesNotExist
    Just (Es.entityVal -> spook) ->
      return $ Right $ SpookData
        { videoUrl = LinkUrl $ savedSpookVideoUrl spook
        , token = token
        , numSpooked = savedSpookChildSpookCount spook
        }
  liftIO $ putStrLn $ "getSpookHandler\n" <> show token <> "\n" <> show result
  return result
  -- return (addHeader "*" result :: Headers '[AccessControlAllowOriginHeader] (Either SpookFailure SpookData))

newSpookHandler :: Maybe Text -> Maybe Text -> SockAddr -> Token -> App (Either SpookFailure [Token])
newSpookHandler referrerHeader realIpHeader sockAddr token = do
  maybeSpook <- getSpookByToken token
  result <- case maybeSpook of
    Nothing -> return $ Left SpookDoesNotExist
    Just spook ->
      return $ Right []
  liftIO $ putStrLn $ "newSpookHandler\n" <> show token <> "\n" <> show result
  return result
  -- return (addHeader "*" result :: Headers '[AccessControlAllowOriginHeader] (Either SpookFailure [Token]))

-- |Landing page for retrieving spook - always serve index.
indexHandler :: Text -> App BsL.ByteString
indexHandler _ = do
  maybeIndexDir <- view (the @"siteServeIndexDirectory")
  case maybeIndexDir of
    Just dir -> liftIO $ BsL.readFile $ dir <> "/index.html"
    _ -> throwError err404

-- |For either static files, or js and other files. Disambiguate by checking for /static/ prefix.
rawHandler :: SiteContext -> Server Raw
rawHandler context =
  let
    rawHandler' req respond
      | "/static/" `Bs.isPrefixOf` rawPathInfo req && isJust (siteServeStaticDirectory context) =
        Wai.staticApp (Wai.defaultWebAppSettings $ fromJust $ siteServeStaticDirectory context) req respond
      | isJust (siteServeIndexDirectory context) =
        Wai.staticApp ((Wai.defaultWebAppSettings $ fromJust $ siteServeIndexDirectory context) { Wai.ssIndices = [Wai.unsafeToPiece "index.html"] }) req respond
    rawHandler' _ respond = respond $ responseLBS status404
                            [ ("Content-Type", "text/plain")
                            ] "File not found"
  in Tagged rawHandler'


