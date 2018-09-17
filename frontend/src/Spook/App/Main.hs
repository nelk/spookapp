{-# LANGUAGE RecursiveDo #-}

module Spook.App.Main
    ( app
    ) where

import qualified Data.Time as Time
import Data.Proxy (Proxy(..))
import Data.Monoid ((<>))
import qualified Reflex.Dom as R
import qualified Servant.Reflex as R
import Servant.API
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Control.Applicative (liftA, liftA2)
import Control.Monad (void, forM, forM_)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Monad (runJSaddle)
import qualified JSDOM.Types as Dom
import qualified JSDOM.Document as Document
import qualified JSDOM.Window as Window

import Spook.Common.Model
import Spook.Common.Api
import Spook.App.Analytics (analyticsScript, analyticsSetUserId)
import Spook.App.Common (attrHideIf)
import qualified Spook.App.Style as S
import qualified Spook.App.Fluent as F
import qualified Spook.App.BootstrapCss as Bootstrap
import qualified Spook.App.Assets as Assets


stylesheet :: R.MonadWidget t m => Text -> m ()
stylesheet s = R.elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

embedStyle :: R.MonadWidget t m => Text -> m ()
embedStyle = R.el "style" . R.text


app :: IO ()
app = do
    --Dom.runWebGUI $ \webView -> R.withWebViewSingleton webView $ \webViewSing -> do
  -- TODO: Don't do irrefutable matches.
  -- Just doc <- fmap (Dom.castTo Dom.HTMLDocument) <$> Dom.webViewGetDomDocument webView
  -- Just (body :: Dom.HTMLElement) <- Document.getBody doc
  -- Just headElement <- fmap Dom.castToHTMLElement <$> Document.getHead doc

  let head :: forall t m. R.MonadWidget t m => m ()
      head = do
  -- R.attachWidget headElement webViewSing $ do
        R.elAttr "meta" (Map.fromList [("name", "viewport"), ("content", "width=device-width, initial-scale=1.0")]) $ return ()
        forM_ ["shortcut icon", "icon"] $ \rel ->
          R.elAttr "link" (Map.fromList [("rel", rel), ("href", "/favicon.ico"), ("type", "image/icon")]) $ return ()
        mapM_ stylesheet Assets.bootstrapCssPaths
        stylesheet "https://fonts.googleapis.com/css?family=Alex+Brush"
        stylesheet "https://fonts.googleapis.com/css?family=Merriweather+Sans:300,400"
        stylesheet "https://fonts.googleapis.com/css?family=Nova+Mono"
        stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

        embedStyle S.rawCss
        R.el "script" $ R.text analyticsScript

  -- R.attachWidget body webViewSing app'
  R.mainWidgetWithHead head app'

{-
type GetTokenRpc t m = R.Dynamic t (Either Text PassPhrase) -> R.Dynamic t (Either Text Text) -> R.Event t () -> m (R.Event t (R.ReqResult () Token))
type SiteDataRpc t m = R.Dynamic t (Either Text Token) -> R.Dynamic t (Either Text Text) -> R.Event t () -> m (R.Event t (R.ReqResult () SiteData))
-}

app' :: forall t m. R.MonadWidget t m => m ()
app' = do
  R.text' "Hello"
  let basePath = "http://192.168.0.142:8080/"  --"/"
  -- let ((tokenRpc :: TokenRpc t m) :<|> (siteDataRpc :: SiteDataRpc t m) :<|> (rsvpRpc :: RsvpRpc t m)) = R.client (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) (R.constDyn $ R.BasePath basePath)

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

  return ()

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

