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
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Generics.Product (the)
import qualified Network.Wai.Application.Static as Wai
import qualified WaiAppStatic.Types as Wai
import Network.Socket
import Data.Default (def)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import Data.Maybe (isNothing, fromMaybe, fromJust, isJust, isNothing, mapMaybe)
import Numeric (showHex)
import Data.Word (Word8, Word16)
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime, diffUTCTime)
import System.Random (randomIO, randomRIO)
import qualified Data.ByteString.Lazy as BsL
import qualified Data.ByteString.Builder as BsL
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base64.URL as Bs64Url
import Servant
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import qualified Network.Wai.Middleware.Prometheus as Prom (prometheus)
import qualified Prometheus as Prom
import Data.Monoid ((<>))
import Control.Monad (replicateM, forM_, void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Applicative (liftA)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ask, runReaderT, ReaderT, lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Lens
import qualified Web.Cookie as Cookie
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO, threadDelay)

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, runMigration, SqlBackend)
import qualified Database.Esqueleto as Es
import qualified Database.Persist as Persist
import Data.Pool (Pool)

import Spook.Common.Model
import Spook.Common.Api (FullApi, ServerApi, AccessControlAllowOriginHeader, SetCookieHeader)
import Spook.Server.Model
import Spook.Server.Data (eventTime)
import qualified Spook.Server.Youtube as YT
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
  , secureCookie :: Bool
  , youtubeKey :: Text
  , youtubeSearchDelay :: Maybe Double
  , enablePrometheus :: Bool
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

data Stats m = Stats
  { incClaimToken :: m ()
  , incCreateToken :: Int -> m ()
  , recDbLatency :: Text -> Double -> m ()
  } deriving Generic

data SiteContext = SiteContext
  { siteDbPool :: Pool SqlBackend
  , siteServeIndexDirectory :: Maybe FilePath
  , siteServeStaticDirectory :: Maybe FilePath
  , siteAllowCrossOrigin :: Bool
  , siteSecureCookie :: Bool
  -- TODO: Split off into separate type class.
  , siteRequestVidMVar :: MVar (MVar (Either SpookFailure SpookVid)) -- To request, create new mvar, put that mvar into this value, then block on your mvar.
  , siteHttpManager :: Manager
  , siteYoutubeKey :: Text
  , siteYoutubeSearchDelay :: NominalDiffTime
  , sitePrometheus :: Bool
  , siteStats :: Stats IO
  } deriving Generic

-- TODO: Lookup servant threading model - how do threads interact with StateT? Make random part of that.

newtype App a = App { runApp :: ReaderT SiteContext Handler a }
  deriving (Functor, Applicative, Monad, MonadReader SiteContext, MonadError ServantErr, MonadIO, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either ServantErr a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . runApp)
  restoreM = App . restoreM

runSql :: Text -> Es.SqlPersistT App a -> App a
runSql queryName sql = do
  context <- ask
  runSql' context queryName sql

runSql' :: forall m a. (MonadIO m, MonadBaseControl IO m) => SiteContext -> Text -> Es.SqlPersistT m a -> m a
runSql' context queryName sql = do
  beforeTime <- liftIO getCurrentTime
  result <- Es.runSqlPool sql (siteDbPool context)
  afterTime <- liftIO getCurrentTime
  let latency :: Double = realToFrac $ beforeTime `diffUTCTime` afterTime
  liftIO $ recDbLatency (siteStats context) queryName latency
  return result

startApp :: IO ()
startApp = do
  params <- OG.getRecord "Spook.app Web Server"
  dbPool <- runStderrLoggingT $ createPostgresqlPool (dbConnectInfo params) (fromMaybe 1 $ dbPoolSize params)
  flip Es.runSqlPool dbPool $ do
    runMigration migrateAll
    insertTestData
  httpManager <- newManager tlsManagerSettings
  requestVidMVar <- MVar.newEmptyMVar
  let prometheusOn = enablePrometheus params

  stats <- if prometheusOn
    then do
      claimTokenCounter <- Prom.registerIO $ Prom.counter (Prom.Info "claim_token" "Number of tokens claimed")
      createTokenCounter <- Prom.registerIO $ Prom.counter (Prom.Info "create_token" "Number of tokens created")
      dbLatencyHistogram <- Prom.registerIO
        $ Prom.vector ("query_name" :: String)
        $ Prom.histogram (Prom.Info "db_latency" "Latency of db queries") Prom.defaultBuckets
      return Stats
        { incClaimToken = liftIO $ Prom.incCounter claimTokenCounter
        , incCreateToken = \i -> void $ liftIO $ Prom.addCounter (fromIntegral i) createTokenCounter
        , recDbLatency = \query_name latency -> liftIO $ Prom.withLabel (Text.unpack query_name) (Prom.observe latency) dbLatencyHistogram 
        }
    else return Stats
          { incClaimToken = return ()
          , incCreateToken = const $ return ()
          , recDbLatency = const $ const $ return ()
          }

  let port = fromMaybe 8080 $ sitePort params
      context = SiteContext
        { siteDbPool = dbPool
        , siteRequestVidMVar = requestVidMVar
        , siteHttpManager = httpManager
        , siteServeIndexDirectory = serveIndexDirectory params
        , siteServeStaticDirectory = serveStaticDirectory params
        , siteAllowCrossOrigin = allowCrossOrigin params
        , siteSecureCookie = secureCookie params
        , siteYoutubeKey = youtubeKey params
        , siteYoutubeSearchDelay = realToFrac $ fromMaybe 2.0 $ youtubeSearchDelay params
        , sitePrometheus = prometheusOn
        , siteStats = stats
        }

  _ <- forkIO $ spookFetcherWorker context

  putStrLn $ "Running on port " ++ show port
  run port $ app context

insertTestData :: MonadIO m => Es.SqlPersistT m ()
insertTestData = do {- forM_ testData $ \savedSpook -> do
  maybeDbSpook :: Maybe (Es.Entity SavedSpook) <- Es.getBy $ UniqueToken $ savedSpookToken savedSpook
  when (isNothing maybeDbSpook) $ void $ Es.insert savedSpook
  -}
  let visitorId = "nelk"
      vidId = "oJqc4vByZCc"
  _ <- Es.upsert (Visitor {visitorVisitorId = visitorId}) []
  _ <- Es.upsert (SpookVid {spookVidVidId = vidId }) []
  _ <- Es.upsert (SavedSpook
      { savedSpookToken = unToken magicToken
      , savedSpookParentSpook = Nothing
      , savedSpookVisits = 0
      , savedSpookIp = Nothing
      , savedSpookReferrer = Nothing
      , savedSpookChildSpookCount = 0
      , savedSpookCreator = visitorId
      , savedSpookClaimer = Nothing
      , savedSpookVidId = vidId
      }) [] -- [ SavedSpookClaimer Persist.=. Nothing, SavedSpookVisits Persist.=. 0, SavedSpookChildSpookCount Persist.=. 0 ]
  return ()

createVideoUrl :: Text -> Text
createVideoUrl id' = "https://www.youtube.com/embed/" <> id' <> "?autoplay=1&rel=0&controls=0&showinfo=0&ecver=2"


app :: SiteContext -> Application
app context =
 let policy = simpleCorsResourcePolicy { corsOrigins = Just (["http://localhost:8080", "http://localhost:3003"], True), corsRequestHeaders = ["Content-Type"] }
     corsMiddleware
       | siteAllowCrossOrigin context =
           cors (const $ Just policy) . provideOptions (Proxy :: Proxy ServerApi)
       | otherwise = id
     prometheusMiddleware
       | sitePrometheus context = Prom.prometheus def
       | otherwise = id
 in logStdoutDev
    $ prometheusMiddleware
    $ corsMiddleware
    $ serve (Proxy :: Proxy FullApi)
    $ server context

convertApp :: SiteContext -> App :~> Handler
convertApp cfg = NT (flip runReaderT cfg . runApp)

-- Note: Can't put headers ahead of path or compiler hangs.
server :: SiteContext -> Server FullApi
server context = enter (convertApp context) ((getSpookHandler :<|> newSpookHandler ) {- :<|> indexHandler -} ) :<|> rawHandler context

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
generateToken :: App Token
generateToken = do
  bs :: Bs.ByteString <- liftIO $ liftA Bs.pack $ replicateM 18 (randomIO :: IO Word8)
  let bs64 = Bs64Url.encode bs
  return $ Token $ Text.decodeUtf8 bs64

headMay :: [a] -> Maybe a
headMay (a:_) = Just a
headMay _ = Nothing

getVisitor :: VisitorId -> App (Maybe Visitor)
getVisitor visitorId = runSql "get_visitor" $ Es.get visitorId

getSpookByToken :: Token -> App (Maybe SavedSpook)
getSpookByToken (Token token) =
  runSql "get_spook" $ Es.get $ SavedSpookKey token
{-
  spookEnts <- runSql $ Es.select $ Es.from $ \savedSpook -> do
    Es.where_ $ savedSpook Es.^. SavedSpookToken Es.==. Es.val token
    return savedSpook
  return $ headMay spookEnts
  -}

cookieKey :: Bs.ByteString
cookieKey = "vis"

addCookie :: Cookie.SetCookie -> r -> Headers '[SetCookieHeader] r
addCookie c = addHeader (Text.decodeUtf8 $ BsL.toStrict $ BsL.toLazyByteString $ Cookie.renderSetCookie c)

cookieToVisitor :: Maybe Text -> App (Maybe Visitor)
cookieToVisitor maybeCookie = runMaybeT $ do
  rawCookie :: Text <- MaybeT $ pure maybeCookie
  let cookies = Cookie.parseCookies $ Text.encodeUtf8 rawCookie
  visId <- MaybeT $ pure $ lookup cookieKey cookies
  MaybeT $ getVisitor $ VisitorKey $ Text.decodeUtf8 visId

getSpookHandler :: Maybe Text -> Maybe Text -> Maybe Text -> SockAddr -> Token -> App (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
getSpookHandler maybeCookie referrerHeader realIpHeader sockAddr token = do
  maybeVisitor <- cookieToVisitor maybeCookie
  maybeSpook <- getSpookByToken token
  setSecureCookie <- view (the @"siteSecureCookie")

  _ <- runSql "insert_visit" $ Es.insert $ Visit
    { visitIp = getAddrIpAsText realIpHeader sockAddr
    , visitReferrer = referrerHeader
    , visitVisitorId = visitorVisitorId <$> maybeVisitor
    }

  let handleSpookRevisit :: SavedSpook -> App (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
      handleSpookRevisit spook = do
        runSql "update_saved_spook_revisit" $ Es.update $ \s -> do
          Es.set s [ SavedSpookVisits Es.+=. Es.val 1 ]
          Es.where_ (s Es.^. SavedSpookToken Es.==. Es.val (savedSpookToken spook))
        return $ noHeader $ Right $ SpookData
          { videoUrl = LinkUrl $ createVideoUrl $ savedSpookVidId spook
          , token = token
          , numSpooked = savedSpookChildSpookCount spook
          }

      handle :: Maybe SavedSpook -> Maybe Visitor -> App (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
      handle Nothing _ = return $ noHeader $ Left SpookDoesNotExist

      -- Magic root spook.
      handle (Just spook) _ | savedSpookToken spook == unToken magicToken = handleSpookRevisit spook

      -- New visitor claiming spook.
      handle (Just spook) Nothing | isNothing (savedSpookClaimer spook) = do
        -- TODO: Increment IP.
        visitorId <- generateToken
        runSql "insert_visitor_claim_saved_spook" $ do
          _ <- Es.insert $ Visitor { visitorVisitorId = unToken visitorId }
          Es.update $ \s -> do
            Es.set s [ SavedSpookClaimer Es.=. Es.val (Just $ unToken visitorId), SavedSpookVisits Es.+=. Es.val 1 ]
            Es.where_ (s Es.^. SavedSpookToken Es.==. Es.val (savedSpookToken spook))
        let cookie = def { Cookie.setCookieName = cookieKey
                         , Cookie.setCookieValue = Text.encodeUtf8 $ unToken visitorId
                         , Cookie.setCookieMaxAge = Just $ fromIntegral (60 * 24 * 60 * 60 :: Int) -- 60 days in seconds.
                         , Cookie.setCookieHttpOnly = True
                         , Cookie.setCookieSecure = setSecureCookie
                         }
        stats <- view (the @"siteStats")
        liftIO $ incClaimToken stats
        return $ addCookie cookie $ Right $ SpookData
          { videoUrl = LinkUrl $ createVideoUrl $ savedSpookVidId spook
          , token = token
          , numSpooked = savedSpookChildSpookCount spook
          }

      -- Returning visitor viewing their spook.
      handle (Just spook) (Just visitor) | savedSpookClaimer spook == Just (visitorVisitorId visitor) = handleSpookRevisit spook

      -- Someone else claimed this spook already.
      handle (Just spook) (Just visitor)
        | isJust (savedSpookClaimer spook)
        && savedSpookClaimer spook /= Just (visitorVisitorId visitor) =
          return $ noHeader $ Left SpookAlreadyClaimed

      -- Visitor already claimed another spook.
      handle _ _ = return $ noHeader $ Left VisitorAlreadyClaimedSpook

  result <- handle maybeSpook maybeVisitor
  liftIO $ putStrLn $ "getSpookHandler\ncookie: " <> show maybeCookie <> "\ntoken: " <> show token <> "\nresult: " <> show (getResponse result)
  return result
  -- return (addHeader "*" result :: Headers '[AccessControlAllowOriginHeader] (Either SpookFailure SpookData))


newSpookHandler :: Maybe Text -> Maybe Text -> Maybe Text -> SockAddr -> Token -> App (Either SpookFailure [Token])
newSpookHandler maybeCookie referrerHeader realIpHeader sockAddr token = do
  maybeSpook <- getSpookByToken token
  maybeVisitor <- cookieToVisitor maybeCookie

  _ <- runSql "insert_visit" $ Es.insert $ Visit
    { visitIp = getAddrIpAsText realIpHeader sockAddr
    , visitReferrer = referrerHeader
    , visitVisitorId = visitorVisitorId <$> maybeVisitor
    }

  let handle :: Maybe SavedSpook -> Maybe Visitor -> App (Either SpookFailure [Token])
      handle (Just spook) (Just visitor)
        | (savedSpookToken spook == unToken magicToken)
          || (savedSpookClaimer spook == Just (visitorVisitorId visitor)
            && (savedSpookChildSpookCount spook == 0 || savedSpookToken spook == unToken magicToken)) = do
          newVidsEither :: Either SpookFailure [SpookVid] <- requestNewVids
          case newVidsEither of
            Left e -> return $ Left e
            Right newVids -> do
              newTokens <- replicateM (length newVids) generateToken
              result <- runSql "insert_saved_spooks" $ do
                forM_ (zip newVids newTokens) $ \(spookVid, token') -> Es.insert $ SavedSpook
                  { savedSpookToken = unToken token'
                  , savedSpookParentSpook = Just $ savedSpookToken spook
                  , savedSpookVisits = 0
                  , savedSpookIp = Nothing
                  , savedSpookReferrer = Nothing
                  , savedSpookChildSpookCount = 0
                  , savedSpookCreator = visitorVisitorId visitor
                  , savedSpookClaimer = Nothing
                  , savedSpookVidId = spookVidVidId spookVid
                  }
                Es.update $ \s -> do
                  Es.set s [ SavedSpookChildSpookCount Es.+=. Es.val (length newVids) ]
                  Es.where_ (s Es.^. SavedSpookToken Es.==. Es.val (savedSpookToken spook))
                return $ Right newTokens
              stats <- view (the @"siteStats")
              liftIO $ incCreateToken stats $ length newVids
              return result

      handle (Just spook) (Just visitor)
        | savedSpookClaimer spook == Just (visitorVisitorId visitor)
        && savedSpookChildSpookCount spook > 0 = do
          alreadyCreatedChildSpooks :: [Es.Entity SavedSpook] <- runSql "select_already_created_saved_spooks" $ Es.select $ Es.from $ \savedSpook -> do
            Es.where_ $ savedSpook Es.^. SavedSpookParentSpook Es.==. Es.just (Es.val $ savedSpookToken spook)
            return savedSpook
          return $ Right $ (Token . savedSpookToken . Es.entityVal) <$> alreadyCreatedChildSpooks

      handle _ _ = return $ Left SpookDoesNotExist

  result <- handle maybeSpook maybeVisitor
  liftIO $ putStrLn $ "newSpookHandler\n" <> show token <> "\n" <> show result
  return result
      -- return (addHeader "*" result :: Headers '[AccessControlAllowOriginHeader] (Either SpookFailure [Token]))

-- |Landing page for retrieving spook - always serve index.
{-
indexHandler :: Text -> App BsL.ByteString
indexHandler _ = do
  maybeIndexDir <- view (the @"siteServeIndexDirectory")
  case maybeIndexDir of
    Just dir -> liftIO $ BsL.readFile $ dir <> "/index.html"
    _ -> throwError err404
-}

-- |For either static files, or js and other files. Disambiguate by checking for /static/ prefix.
rawHandler :: SiteContext -> Server Raw
rawHandler context =
  let
    rawHandler' req respond
      | "/static/" `Bs.isPrefixOf` rawPathInfo req && isJust (siteServeStaticDirectory context) =
        let wrappedResponder = respond -- respond . mapResponseHeaders (("Access-Control-Allow-Origin", "http://localhost:3003"):) -- Seems to already add this, don't double-add. Needed for font.
        in Wai.staticApp (Wai.defaultWebAppSettings $ fromJust $ siteServeStaticDirectory context) req wrappedResponder
      | isJust (siteServeIndexDirectory context) =
        Wai.staticApp ((Wai.defaultWebAppSettings $ fromJust $ siteServeIndexDirectory context) { Wai.ssIndices = [Wai.unsafeToPiece "index.html"] }) req respond
    rawHandler' _ respond = respond $ responseLBS status404
                            [ ("Content-Type", "text/plain")
                            ] "File not found"
  in Tagged rawHandler'

ytPageSize :: Int
ytPageSize = 30

ytBufferThreshold :: Int
ytBufferThreshold = 10

requestNewVids :: App (Either SpookFailure [SpookVid])
requestNewVids = do
  requestVidMVar <- view $ the @"siteRequestVidMVar"
  eitherResults <- liftIO $ do
    numTokens <- randomRIO (1, 2)
    replicateM numTokens $ do
      responseMVar <- MVar.newEmptyMVar
      MVar.putMVar requestVidMVar responseMVar
      MVar.takeMVar responseMVar
  return $ sequence eitherResults

spookFetcherWorker :: SiteContext -> IO ()
spookFetcherWorker context = do
  let requestVidMVar = siteRequestVidMVar context
      searchDelay = siteYoutubeSearchDelay context

  unusedSpookVids :: [Es.Entity SpookVid] <- runSql' context "get_unused_spook_vids" $ Es.select $ Es.from $ \(spookVid `Es.LeftOuterJoin` savedSpook) -> do
    Es.on $ Es.just (spookVid Es.^. SpookVidVidId) Es.==. savedSpook Es.?. SavedSpookVidId
    Es.where_ $ Es.isNothing (savedSpook Es.?. SavedSpookId) -- Note: Note the same as Es.==. Es.nothing
    return spookVid

  (headMay -> firstSpookVidPage :: Maybe (Es.Entity SpookVidPage)) <-
    runSql' context "get_spook_vid_page" $ Es.select $ Es.from $ \spookVidPage -> do
      Es.limit 1
      return spookVidPage

  let loop :: [SpookVid] -> Maybe (Es.Entity SpookVidPage) -> Maybe UTCTime -> Maybe SpookFailure -> IO ()
      loop spookVids spookVidPage lastSearchTime lastSearchError = do
        maybeResponseMVar <- MVar.tryTakeMVar requestVidMVar

        let pageToken = (spookVidPageNextPage . Es.entityVal) <$> spookVidPage
        time <- getCurrentTime

        let doSearch = do
              response <- YT.searchYoutube (siteHttpManager context) $ YT.YoutubeRequest
                { YT.key = siteYoutubeKey context
                , YT.q = "spooky meme"
                , YT.maxResults = ytPageSize
                , YT.part = "id" -- "snippet"
                , YT.pageToken = pageToken
                , YT.resourceType = Just "video"
                }
              putStrLn $ "Response from YT search: " <> show response
              case response of
                Left _ -> loop spookVids spookVidPage (Just time) (Just SpookTemporaryFailure)
                Right searchResult -> do
                  let newSpookVids = mapMaybe (\r -> case r of
                                                  YT.YoutubeVideoId videoId -> Just $ SpookVid videoId
                                                  _ -> Nothing) $ YT.resources searchResult
                      spookVidPage' = (SpookVidPage $ YT.nextPageToken searchResult)
                  spookVidPageId <- runSql' context "insert_spook_vids" $ do
                    -- TODO: Upsert to allow ignoring duplicates.
                    forM_ newSpookVids $ Es.insert
                    case spookVidPage of
                      Just pageEnt -> do
                        Es.update $ \p -> do
                          Es.set p [ SpookVidPageNextPage Es.=. Es.val (YT.nextPageToken searchResult) ]
                          Es.where_ (p Es.^. SpookVidPageId Es.==. Es.val (Es.entityKey pageEnt))
                        return $ Es.entityKey pageEnt
                      Nothing -> Es.insert spookVidPage'

                  loop (spookVids <> newSpookVids) (Just $ Es.Entity spookVidPageId spookVidPage') (Just time) Nothing

        case (spookVids, maybeResponseMVar, lastSearchError) of
          -- Out of vids, search delay long enough, do new search.
          ([], _, _) | maybe True (\t -> time > searchDelay `addUTCTime` t) lastSearchTime -> doSearch
          -- No requests, below threshold, search.
          (vs, Nothing, _) | length vs < ytBufferThreshold -> doSearch
          -- No requests, above threshold, just wait a while.
          (_, Nothing, _) -> do
            threadDelay 500000 -- micro seconds
            loop spookVids spookVidPage lastSearchTime lastSearchError
          -- Out of vids, no error last time, do search anyway (this case shouldn't happen).
          ([], Just _, Nothing) -> doSearch
          -- Out of vids, errored last time and haven't waited for search delay. Reply error for now.
          ([], Just responseMVar, Just err) -> do
            MVar.putMVar responseMVar $ Left err
            loop spookVids spookVidPage lastSearchTime lastSearchError
          -- Have a vid.
          (spookVid:moreSpookVids, Just responseMVar, _) -> do
            MVar.putMVar responseMVar $ Right spookVid
            loop moreSpookVids spookVidPage lastSearchTime lastSearchError

  loop (Es.entityVal <$> reverse unusedSpookVids) firstSpookVidPage Nothing Nothing


