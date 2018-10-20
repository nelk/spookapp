{-# LANGUAGE RecursiveDo #-}

module Spook.App.Main
    ( mainish
    ) where

import Data.Default (def)
import GHC.Generics (Generic)
import qualified Data.Time as Time
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Monoid ((<>))
import qualified Reflex.Dom as R
import qualified Reflex.Dom.Main
import qualified Reflex.Dom.Widget.Input as R
import qualified Servant.Reflex as R
import qualified Reflex.Material.Basic as RM
import qualified Reflex.Material.Card as RM
import qualified Reflex.Material.Button as RM
import qualified Reflex.Tags as RT
import Servant.API
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Data.Generics.Product (the)
import Control.Applicative (liftA, liftA2)
import Control.Monad (void, forM, forM_)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, runReaderT)
import Data.Either (isLeft)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Monad (runJSaddle)
import qualified Language.Javascript.JSaddle   as Js
import qualified JSDOM as Dom (currentWindow, currentWindowUnchecked)
import qualified JSDOM.Types as Dom
import qualified JSDOM.Document as Document
import qualified JSDOM.Window as Window
import qualified JSDOM.Location as Location
import qualified JSDOM.EventM as EventM (newListener, addListener)
import qualified JSDOM.Event as Event (initEvent)
import qualified JSDOM.WindowEventHandlers as WindowEventHandlers (hashChange)
import qualified JSDOM.GlobalEventHandlers as GlobalEventHandlers (click)
import qualified JSDOM.EventTarget as EventTarget
import qualified JSDOM.ClipboardEvent as ClipboardEvent
import qualified JSDOM.HTMLTextAreaElement as HTMLTextAreaElement
import qualified JSDOM.HTMLButtonElement as HTMLButtonElement

import Paths_frontend (getDataFileName)
import Spook.Common.Model
import Spook.Common.Api
import Spook.App.Analytics (analyticsScript, analyticsSetUserId)
import Spook.App.Common (attrHideIf)
import qualified Spook.App.Style as S
import Spook.App.Runner (customRun)
import qualified Spook.App.Fluent as F
import qualified Spook.App.BootstrapCss as Bootstrap
import qualified Spook.App.Assets as Assets


-- stylesheet :: R.MonadWidget t m => Text -> m ()
-- stylesheet s = R.elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

-- embedStyle :: R.MonadWidget t m => Text -> m ()
-- embedStyle = R.el "style" . R.text


mainish :: IO ()
mainish = do
  customRun $ Reflex.Dom.Main.mainWidgetWithHead htmlHead app

htmlHead :: forall t m. R.MonadWidget t m => m ()
htmlHead = do
    R.el "title" $ R.text "Spook!"
    RM.mobile_
    -- TODO: Copy and serve from nginx? Host locally for dev too?
    {-
    RM.styles_ $ RM.Style
      { RM.styleIcons = ["https://fonts.googleapis.com/icon?family=Material+Icons"]
      , RM.styleFonts = []
      , RM.styleCss = ["http://unpkg.com/material-components-web@latest/dist/material-components-web.css"]
      }
    -}

    -- R.elAttr "meta" (Map.fromList [("name", "viewport"), ("content", "width=device-width, initial-scale=1.0")]) $ return ()
    basePath <- computeApiPath
    forM_ ["shortcut icon", "icon"] $ \rel ->
      R.elAttr "link" (Map.fromList [("rel", rel), ("href", basePath <> "static/favicon.ico"), ("type", "image/icon")]) $ return ()
    -- mapM_ stylesheet Assets.bootstrapCssPaths
    -- stylesheet "https://fonts.googleapis.com/css?family=Alex+Brush"
    -- stylesheet "https://fonts.googleapis.com/css?family=Merriweather+Sans:300,400"
    -- stylesheet "https://fonts.googleapis.com/css?family=Nova+Mono"
    RM.stylesheet_ "https://fonts.googleapis.com/icon?family=Material+Icons"

    RM.style_ S.rawCss
    RM.stylesheet_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"

    RM.script_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.js"
    RM.scriptDo_ analyticsScript

type RpcArg t a = R.Dynamic t (Either Text a)
type RpcRes t m a = R.Event t ()
                 -> m (R.Event t (R.ReqResult () a))
type GetSpookRpc t m = RpcArg t Token
                    -> RpcRes t m (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
type NewSpookRpc t m = RpcArg t Token
                    -> RpcRes t m (Either SpookFailure [Token])

class HasGetSpookRpc env t m where
  getSpookRpc :: Lens' env (GetSpookRpc t m)

class HasNewSpookRpc env t m where
  newSpookRpc :: Lens' env (NewSpookRpc t m)

class HasBasePath env where
  getApiPath :: Lens' env Text
  getAppPath :: Lens' env Text

type App env reflexM = ReaderT env reflexM

data Env t (reflexM :: * -> *) = Env
  { envGetSpookRpc :: GetSpookRpc t (App (Env t reflexM) reflexM)
  , envNewSpookRpc :: NewSpookRpc t (App (Env t reflexM) reflexM)
  , envApiPath :: Text
  , envAppPath :: Text
  } deriving Generic

instance HasGetSpookRpc (Env t reflexM) t (App (Env t reflexM) reflexM) where
  getSpookRpc = the @"envGetSpookRpc"

instance HasNewSpookRpc (Env t reflexM) t (App (Env t reflexM) reflexM) where
  newSpookRpc = the @"envNewSpookRpc"

instance HasBasePath (Env t reflexM) where
  getApiPath = the @"envApiPath"
  getAppPath = the @"envAppPath"

app :: forall t m. R.MonadWidget t m => m ()
app = do
  maybeLocation <- getLocation
  apiPath <- computeApiPath
  let tweakRequest = R.ClientOptions $ \r -> return $ r & R.withCredentials .~ True
      ((getSpookRpc :: GetSpookRpc t m) :<|> (newSpookRpc :: NewSpookRpc t m))
        = R.clientWithOpts (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) (R.constDyn $ R.BasePath apiPath) tweakRequest
      env = Env
        { envGetSpookRpc = (\a b -> lift $ getSpookRpc a b)
        , envNewSpookRpc = (\a b -> lift $ newSpookRpc a b)
        , envApiPath = apiPath
        , envAppPath = maybe "https://spook.app" (\(protocol, host) -> protocol <> "//" <> host) maybeLocation
        }
      unwrapReaderEnv w = runReaderT w env
  let lp :: App (Env t m) m () = landingPage
  unwrapReaderEnv lp

landingPage :: forall t m env.
             ( R.MonadWidget t m
             , MonadReader env m
             , HasGetSpookRpc env t m
             , HasNewSpookRpc env t m
             , HasBasePath env
             ) => m ()
landingPage = do
  postBuildE <- R.getPostBuild
  tokenDyn <- getToken

  let doGetSpookE = R.leftmost [() <$ R.updated tokenDyn, postBuildE]

  getSpookRpc' :: GetSpookRpc t m <- view getSpookRpc
  getSpookResultE <- getSpookRpc' tokenDyn doGetSpookE

  _ <- R.widgetHold (R.text "Loading") $ R.ffor getSpookResultE $ \case
    (R.ResponseSuccess _ (getResponse -> Right spookData) _) -> void $ spookWidget spookData
    (R.ResponseSuccess _ (getResponse -> Left spookFailure) _) -> void $ badTokenWidget spookFailure
    (R.ResponseFailure _ _ _) -> badTokenWidget SpookTemporaryFailure
    (R.RequestFailure _ _) -> noTokenWidget
  return ()

badTokenText :: SpookFailure -> Text
badTokenText SpookAlreadyClaimed = "This spook has already been claimed"
badTokenText SpookDoesNotExist = "This spook does not exist"
badTokenText SpookTemporaryFailure = "We had a temporary issue. Try reloading the page"

badTokenWidget :: forall t m env.
                ( R.MonadWidget t m
                , MonadReader env m
                , HasBasePath env
                ) => SpookFailure
                  -> m ()
badTokenWidget failure = do
  basePath <- view getApiPath
  F.img (F.srcAttr, basePath <> "static/dootdoot.gif" :: Text) $ return ()
  R.text $ badTokenText failure

noTokenWidget :: forall t m. R.DomBuilder t m => m ()
noTokenWidget = R.text "Welcome! You'll need to receive a unique spook from someone else."

getLocation :: forall m.
          ( Dom.MonadJSM m
          ) => m (Maybe (Text, Text))
getLocation = runMaybeT $ do
  window <- MaybeT Dom.currentWindow
  document <- lift $ Window.getDocument window
  location <- MaybeT $ Document.getLocation document
  protocol <- Location.getProtocol location
  host <- Location.getHost location
  return (protocol, host)

computeApiPath :: forall m.
             ( Dom.MonadJSM m
             ) => m Text
computeApiPath = do
  maybeLoc <- getLocation
  -- For local development, always fetch on :8080, even if we're served from jsaddle-warp server.
  return $ case maybeLoc of
    Just (_, h) | "localhost" `Text.isPrefixOf` h || "127.0.0.1" `Text.isPrefixOf` h -> "http://localhost:8080/"
    otherwise -> "/"

getToken :: forall t m.
          ( R.MonadWidget t m
          ) => m (R.Dynamic t (Either Text Token))
getToken = do
  Just window <- Dom.currentWindow
  (newTokenMaybeE, triggerTokenMaybe) <- R.newTriggerEvent

  let
    getTokenMaybe :: Dom.JSM (Maybe Token)
    getTokenMaybe = runMaybeT $ do
      document <- lift $ Window.getDocument window
      location <- MaybeT $ Document.getLocation document
      -- Drop "#" prefix.
      (Token . Text.drop 1) <$> Location.getHash location
  jsContextRef <- Dom.askJSM
  listener <- Dom.liftJSM $ EventM.newListener $ do
    maybeToken <- Dom.runJSM getTokenMaybe jsContextRef
    liftIO $ triggerTokenMaybe maybeToken

  Dom.liftJSM $ EventM.addListener window WindowEventHandlers.hashChange listener False
  startToken <- Dom.liftJSM getTokenMaybe
  maybeTokenE <- R.holdDyn startToken newTokenMaybeE
  return $ maybe (Left "No token in path") Right <$> maybeTokenE

data SpookWidgetState = SpookWidgetInitial | SpookWidgetNewTokens [Token] | SpookWidgetFailure SpookFailure

spookWidget :: forall t m env.
             ( R.MonadWidget t m
             , MonadReader env m
             , HasNewSpookRpc env t m
             , HasBasePath env
             ) => SpookData
               -> m ()
spookWidget spookData = do
  newSpookRpc' :: NewSpookRpc t m <- view newSpookRpc

  F.div [RM.mdcCard_, S.clzSpookCard] $ do
    rec
      newSpooksResultE <- newSpookRpc' (R.constDyn $ Right $ token spookData) getNewSpooksE

      let widgetStateE = R.ffor newSpooksResultE $ \case
            (R.ResponseSuccess _ (Right newSpookTokens) _) -> SpookWidgetNewTokens newSpookTokens
            (R.ResponseSuccess _ (Left spookFailure) _) -> SpookWidgetFailure spookFailure
            (R.ResponseFailure _ _ _) -> SpookWidgetFailure SpookTemporaryFailure
            (R.RequestFailure _ _) -> SpookWidgetInitial

      widgetStateDyn <- R.holdDyn SpookWidgetInitial widgetStateE

      F.section RM.mdcCardPrimary_ $ do
        F.h1 RM.mdcCardTitleLarge_ $
          R.text "You've Been Spooked!"
        embedYoutube $ spookData ^. the @"videoUrl"
      getNewSpooksE <- F.section [RM.mdcCardActions_, S.clzNewSpookList] $ do
        es <- R.dyn $ R.ffor widgetStateDyn $ \case
          SpookWidgetInitial -> RM.mdButton def $ R.text "Spook Others"
          SpookWidgetFailure e -> R.text (badTokenText e) >> return R.never
          SpookWidgetNewTokens tokens -> do
            appPath <- view getAppPath
            forM_ tokens $ \(Token tok) -> do
              F.div S.clzTokenWrapper $ do
                let link = appPath <> "#" <> tok
                textArea <- R.textArea $ R.TextAreaConfig link R.never $ R.constDyn (Map.fromList [("readonly", ""), ("rows", "1")])
                (buttonEl, _) <- RT.button' $ R.text "Copy"
                jsContextRef <- Dom.askJSM
                copyListener <- Dom.liftJSM $ EventM.newListener $ do
                  HTMLTextAreaElement.select $ R._textArea_element textArea
                  window <- Dom.currentWindowUnchecked
                  document <- Window.getDocument window
                  Document.execCommand document ("copy" :: Text) False (Nothing @Text)
                  return ()
                Just buttonHtmlEl <- Dom.castTo HTMLButtonElement.HTMLButtonElement $ R._el_element buttonEl
                Dom.liftJSM $ EventM.addListener buttonHtmlEl GlobalEventHandlers.click copyListener True
                return ()
            return $ R.never
        R.switchPromptly R.never es
    return ()


embedYoutube :: forall t m.
              ( R.DomBuilder t m
              , R.PostBuild t m
              ) => LinkUrl
                -> m ()
embedYoutube (LinkUrl url) = do
  let attrs :: F.Attrs t = mconcat
        [ F.toAttrs (F.widthAttr, "1000" :: Text)
        , F.toAttrs (F.heightAttr, "500" :: Text)
        , F.toAttrs (F.srcAttr, url)
        , F.toAttrs (F.frameborderAttr, "0" :: Text)
        , F.toAttrs (F.allowAttr, "autoplay;encrypted-media" :: Text)
        ]
  F.iframe attrs (return ())


{-
  postBuildE <- R.getPostBuild
  delayedPostBuildE <- R.delay 0.5 postBuildE

  rec
    photoSplash tokenDyn siteDataDyn

    rootEl <- F.div' S.clzRoot $ do
      mainSiteWidget tokenDyn siteDataDyn rsvpRpc

    let tokenResponseToRpcInput :: Maybe Token -> Either Text Token
        tokenResponseToRpcInput Nothing = Left "Do not have a valid token"
        tokenResponseToRpcInput (Just token) = Right token

    getSiteDataResponseE <- siteDataRpc
      (tokenResponseToRpcInput <$> tokenDyn)
      (R.constDyn $ Right "")
      (void $ R.updated tokenDyn)

    let successfulGetSiteDataE :: R.Event t SiteData =
          R.fmapMaybe R.reqSuccess getSiteDataResponseE
    siteDataDyn :: R.Dynamic t (Maybe SiteData) <-
      R.holdDyn Nothing $ Just <$> successfulGetSiteDataE

  R.performEvent_ $ R.ffor
    (R.fmapMaybe id $ R.tagPromptlyDyn tokenDyn successfulGetSiteDataE)
    $ \(Token tok) -> Storage.storageSet tokenStorageKey $ Text.unpack tok

  R.performEvent_ $ R.ffor
    (R.fmapMaybe id $ R.updated siteDataDyn)
    $ \siteData -> analyticsSetUserId $ Text.unpack $ siteData ^. dataVisitorExternalId
    -}


logo :: R.MonadWidget t m => m ()
logo = F.h1 S.clzLogo $ R.text "Spook"

{-

mainSiteWidget :: forall t m. R.MonadWidget t m
               => R.Dynamic t (Maybe Token)
               -> R.Dynamic t (Maybe SiteData)
               -> RsvpRpc t m -> m ()
mainSiteWidget tokenDyn siteDataDyn rsvpRpc = do
  let makeNoEvent :: m (R.Event t (Loc -> Loc))
      makeNoEvent = return R.never
      siteDataMayE = R.attachPromptlyDyn tokenDyn (R.updated siteDataDyn)
  rec
    let liftedRsvpRpc = ((lift <$>) <$>) <$> rsvpRpc -- Lift MonadWidget into ReaderT.
        toWidget (Nothing, _) = makeNoEvent
        toWidget (_, Nothing) = makeNoEvent
        toWidget (Just token, Just siteData) = runReaderT (mainSiteWidget' locDyn liftedRsvpRpc) (token, siteData)
    -- TODO: Remove navigation logs in production.
    locDyn :: R.Dynamic t Loc <- {-R.traceDynWith (("Navigate: " <>) . toUrl) <$>-} routeSite changeLocE
    changeLocE <- R.switchPromptlyDyn <$> R.widgetHold makeNoEvent (toWidget <$> siteDataMayE)

  -- Scroll to top of page when changing page.
  -- TODO: Scroll to element for subparts of same page.
  jsContextRef <- Dom.askJSM
  R.performEvent_ $ R.ffor (R.updated locDyn) $ const $ liftIO $ do
    runJSaddle jsContextRef $ do
      Just window <- Dom.currentWindow
      Window.scrollTo window 0 0

type NavEvent t = R.Event t (Loc -> Loc)
type StdWidget t m = R.Dynamic t Loc -> m (NavEvent t)

mainSiteWidget' :: forall t m. (R.MonadWidget t m, MonadReader (Token, SiteData) m)
                => R.Dynamic t Loc
                -> RsvpRpc t m
                -> m (R.Event t (Loc -> Loc))
mainSiteWidget' locDyn rsvpRpc =
  F.div () $ do
    tabNavE <- tabBar locDyn
    navEs <- F.div S.clzPagePanelContainer $ do
      let wrapHidingPagePanel :: R.Dynamic t Bool -> StdWidget t m -> m (NavEvent t)
          wrapHidingPagePanel showWidget widget =
            F.div (S.clzPagePanel, (S.clzSelected, showWidget), (S.clzUnselected, not <$> showWidget)) $ widget locDyn
          noNav :: (R.Dynamic t Loc -> m ()) -> StdWidget t m
          noNav = (liftA (const R.never) <$>)
      sequence
        [ wrapHidingPagePanel (isMainPage . (^. locRoute) <$> locDyn) mainPage
        , wrapHidingPagePanel ((== RPhotos) . (^. locRoute) <$> locDyn) $ noNav photosPage
        , wrapHidingPagePanel ((== RWeddingParty) . (^. locRoute) <$> locDyn) $ noNav weddingPartyPage
        , wrapHidingPagePanel ((== RDetails) . (^. locRoute) <$> locDyn) $ noNav detailsPage
        , wrapHidingPagePanel ((== RRsvp) . (^. locRoute) <$> locDyn) $ noNav (`rsvpPage` rsvpRpc)
        ]
    return $ R.leftmost $ tabNavE:navEs

mainPage :: forall t m. (R.MonadWidget t m, MonadReader (Token, SiteData) m)
         => R.Dynamic t Loc
         -> m (R.Event t (Loc -> Loc))
mainPage _ = do
  (_, siteData) <- ask
  F.div [S.clzMainMessageContainer, Bootstrap.colSm12] $
    F.div [S.clzMainMessage, S.clzCard] $ do
      F.h1 S.clzMainMessageHeader $ R.text $ siteData ^. dataMarriageMessage
      forM_ (siteData ^? dataSchedule . _head . scheduleStepDateTime . _Just :: Maybe Time.UTCTime) countdownWidget
      -- , F.toAttrs (F.hrefAttr, "javascript:void(0);" :: Text)
      rsvpClick <- F.div S.clzRsvpNowButton $ R.button "RSVP Now"
      return $ const (locRoute .~ RRsvp) <$> rsvpClick

countdownWidget :: (R.MonadWidget t m, MonadIO m) => Time.UTCTime -> m ()
countdownWidget targetTime = do
  startTime <- liftIO Time.getCurrentTime
  tickE <- R.tickLossy 1 startTime
  let widg currentTime =
        let totalSeconds :: Int = max 0 $ ceiling $ Time.diffUTCTime targetTime currentTime
            (days, secondsModDays) = divMod totalSeconds (24*60*60)
            (hours, secondsModHours) = divMod secondsModDays (60*60)
            (minutes, seconds) = divMod secondsModHours 60
            fmt i | i < 10 = "0" ++ show i
                  | otherwise = show i
        in F.div S.clzCountdown $ R.text $ Text.pack $ mconcat [show days, " days, ", fmt hours, ":", fmt minutes, ":", fmt seconds]
  void $ R.widgetHold (widg startTime) $ widg . R._tickInfo_lastUTC <$> tickE
  -}

