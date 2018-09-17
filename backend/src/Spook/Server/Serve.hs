{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spook.Server.Serve
    ( startApp
    , app
    , SiteContext(..)
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Application.Static as Wai
import qualified WaiAppStatic.Types as Wai
import Network.Socket
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import Data.Maybe (isNothing, fromMaybe)
import Numeric (showHex)
import Data.Word (Word8, Word16)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import System.Random (randomIO)
import qualified Data.ByteString.Lazy as BsL
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base64.URL as Bs64Url
import Servant
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

import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, runMigration, SqlBackend)
import qualified Database.Esqueleto as Es
import Data.Pool (Pool)

import Spook.Common.Model
import Spook.Common.Api (FullApi, Api, ImplicitApi)
import Spook.Server.Model
import Spook.Server.Data (eventTime)
import Options.Generic as OG


tokenLifetime :: NominalDiffTime
tokenLifetime = 24 * 60 * 60 -- 24 hours.

data Params = Params
  { dbHost :: Maybe Text
  , dbPort :: Maybe Int
  , dbUser :: Maybe Text
  , dbPassword :: Maybe Text
  , dbDatabase :: Maybe Text
  , dbPoolSize :: Maybe Int
  , dbSsl :: Maybe Bool
  , sitePort :: Maybe Int
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
  ] ++ ["sslmode=require" | fromMaybe False dbSsl]

data SiteContext = SiteContext
  { _siteDbPool :: Pool SqlBackend
  }
makeLenses ''SiteContext

-- TODO: Lookup servant threading model - how do threads interact with StateT? Make random part of that

newtype App a = App { runApp :: ReaderT SiteContext Handler a }
  deriving (Functor, Applicative, Monad, MonadReader SiteContext, MonadError ServantErr, MonadIO, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either ServantErr a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . runApp)
  restoreM = App . restoreM

runSql :: Es.SqlPersistT App a -> App a
runSql sql = do
  pool <- (^. siteDbPool) <$> ask
  Es.runSqlPool sql pool

startApp :: IO ()
startApp = do
  params <- OG.getRecord "Spook.app Web Server"
  dbPool <- runStderrLoggingT $ createPostgresqlPool (dbConnectInfo params) (fromMaybe 1 $ dbPoolSize params)
  flip Es.runSqlPool dbPool $ do
    runMigration migrateAll
    insertTestData
  let port = fromMaybe 8080 $ sitePort params
  putStrLn $ "Running on port " ++ show port
  run port $ app $ SiteContext dbPool

testData :: [SavedSpook]
testData =
  [ SavedSpook
      { savedSpookToken = "abc12399"
      , savedSpookParent = Nothing
      , savedSpookVisits = 1
      , savedSpookIp = Nothing
      , savedSpookReferrer = Nothing
      , savedSpookChildSpookCount = 0
      , savedSpookVideoUrl = "https://www.youtube.com/watch?v=oJqc4vByZCc&t=0s&index=6&list=FL2o7N6B96f48pE0yX01iY0A"
      }
  ]

insertTestData :: MonadIO m => Es.SqlPersistT m ()
insertTestData = forM_ testData $ \savedSpook -> do
  maybeDbSpook :: Maybe (Es.Entity SavedSpook) <- Es.getBy $ UniqueToken $ savedSpookToken savedSpook
  when (isNothing maybeDbSpook) $ void $ Es.insert savedSpook

app :: SiteContext -> Application
app context = (serve (Proxy :: Proxy Api) $ server context)
-- app context = (serve (Proxy :: Proxy FullApi) $ server context)

convertApp :: SiteContext -> App :~> Handler
convertApp cfg = NT (flip runReaderT cfg . runApp)

server :: SiteContext -> Server Api
server context = enter (convertApp context) (getSpookHandler :<|> newSpookHandler)

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

-- TODO: Store ip, cookie. Can rewatch with it only.
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
  case maybeSpook of
    Nothing -> return $ Left SpookDoesNotExist
    Just (Es.entityVal -> spook) ->
      return $ Right $ SpookData
        { videoUrl = LinkUrl $ savedSpookVideoUrl spook
        , token = token
        , numSpooked = savedSpookChildSpookCount spook
        }

newSpookHandler :: Maybe Text -> Maybe Text -> SockAddr -> Token -> App (Either SpookFailure [Token])
newSpookHandler referrerHeader realIpHeader sockAddr token = do
  maybeSpook <- getSpookByToken token
  case maybeSpook of
    Nothing -> return $ Left SpookDoesNotExist
    Just spook ->
      return $ Right []

indexHandler :: [Text] -> App BsL.ByteString
indexHandler _ = liftIO $ BsL.readFile "public/index.html"

-- |For static files that also do not require a visitor token.
rawHandler :: Server Raw
rawHandler = serveDirectoryWith $ (Wai.defaultWebAppSettings "public/") { Wai.ssIndices = [Wai.unsafeToPiece "index.html"] }

