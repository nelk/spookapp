{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module Spook.App.Main
    ( mainish
    ) where

import Data.Default (def)
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Monoid ((<>))
import qualified Reflex.Dom as R
import qualified Reflex.Dom.Main
import qualified Servant.Reflex as R
import qualified Reflex.Material.Basic as RM
import qualified Reflex.Material.Card as RM
import qualified Reflex.Material.Button as RM
import Servant.API
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Data.Generics.Product (the)
import Control.Monad (void, forM_)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle   as Js
import qualified JSDOM as Dom (currentWindow, currentWindowUnchecked)
import qualified JSDOM.Types as Dom (Element(..), MonadJSM, JSM, askJSM, liftJSM, runJSM, unsafeCastTo)

-- Horrible hack - figure out why it's needed.
#if defined(ghcjs_HOST_OS)
import GHCJS.DOM.Types (Element(..), HTMLTextAreaElement(..))
#endif

import qualified JSDOM.Document as Document
import qualified JSDOM.Window as Window
import qualified JSDOM.Location as Location
import qualified JSDOM.EventM as EventM (newListener, addListener)
import qualified JSDOM.WindowEventHandlers as WindowEventHandlers (hashChange)
import qualified JSDOM.GlobalEventHandlers as GlobalEventHandlers (click)
import qualified JSDOM.HTMLTextAreaElement as HTMLTextAreaElement
import qualified JSDOM.HTMLButtonElement as HTMLButtonElement

import Spook.Common.Model
import Spook.Common.Api
import Spook.App.Analytics (analyticsScript, analyticsPageView, analyticsEvent)
import qualified Spook.App.Style as S
import Spook.App.Runner (customRun)
import qualified Spook.App.Fluent as F



mainish :: IO ()
mainish = do
  customRun $ Reflex.Dom.Main.mainWidgetWithHead htmlHead app

htmlHead :: forall t m. R.MonadWidget t m => m ()
htmlHead = do
    R.el "title" $ R.text "Spook!"
    RM.mobile_

    apiPath <- computeApiPath
    forM_ ["shortcut icon", "icon"] $ \rel ->
      R.elAttr "link" (Map.fromList [("rel", rel), ("href", apiPath <> "static/favicon.ico"), ("type", "image/icon")]) $ return ()

    -- RM.stylesheet_ "https://fonts.googleapis.com/icon?family=Material+Icons"

    RM.style_ $ S.rawCss apiPath
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
  analyticsPageView "main"
  maybeLocation <- getLocation
  apiPath <- computeApiPath
  let tweakRequest = R.ClientOptions $ \r -> return $ r & R.withCredentials .~ True
      ((getSpookRpc' :: GetSpookRpc t m) :<|> (newSpookRpc' :: NewSpookRpc t m))
        = R.clientWithOpts (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) (R.constDyn $ R.BasePath apiPath) tweakRequest
      env = Env
        { envGetSpookRpc = (\a b -> lift $ getSpookRpc' a b)
        , envNewSpookRpc = (\a b -> lift $ newSpookRpc' a b)
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

  F.div [RM.mdcCard_, S.mdcThemeBackground, S.clzSpookCard] $ do
    F.section [RM.mdcCardPrimary_, S.clzSpookSection] $
      void $ R.widgetHold (R.text "Loading") $ R.ffor getSpookResultE $ \case
        (R.ResponseSuccess _ (getResponse -> Right spookData) _) -> do
          analyticsEvent "token" "get_success" "" Nothing
          void $ spookWidget spookData
        (R.ResponseSuccess _ (getResponse -> Left spookFailure) _) -> do
          analyticsEvent "token" "get_failure" (Text.pack $ show spookFailure) Nothing
          void $ badTokenWidget spookFailure
        (R.ResponseFailure _ e _) -> do
          analyticsEvent "token" "get_failure" e Nothing
          badTokenWidget SpookTemporaryFailure
        _ -> noTokenWidget
  return ()

badTokenText :: SpookFailure -> Text
badTokenText SpookAlreadyClaimed = "This spook has already been claimed"
badTokenText VisitorAlreadyClaimedSpook = "You've already claimed a spook - send this one to a friend!"
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
  F.h1 [RM.mdcCardTitleLarge_, S.mdcThemePrimary, S.clzSpookMessage] $
    R.text $ badTokenText failure
  let attrs :: F.Attrs t = mconcat
        [ F.toAttrs (F.srcAttr, basePath <> "static/dootdoot.gif" :: Text)
        , F.toAttrs S.clzSpookImage
        ]
  F.img attrs $ return ()

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
    _ -> "/"

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

  rec
    newSpooksResultE <- newSpookRpc' (R.constDyn $ Right $ token spookData) getNewSpooksE

    widgetStateDyn <- R.widgetHold (return SpookWidgetInitial) $ R.ffor newSpooksResultE $ \case
      (R.ResponseSuccess _ (Right newSpookTokens) _) -> do
        analyticsEvent "token" "new_success" "" $ Just $ length newSpookTokens
        return $ SpookWidgetNewTokens newSpookTokens
      (R.ResponseSuccess _ (Left spookFailure) _) -> do
        analyticsEvent "token" "new_failure" (Text.pack $ show spookFailure) Nothing
        return $ SpookWidgetFailure spookFailure
      (R.ResponseFailure _ e _) -> do
        analyticsEvent "token" "new_failure" e Nothing
        return $ SpookWidgetFailure SpookTemporaryFailure
      (R.RequestFailure _ _) -> return SpookWidgetInitial

    F.h1 [RM.mdcCardTitleLarge_, S.mdcThemePrimary, S.clzSpookTitle] $
      R.text "You've Been Spooked!"
    embedYoutube $ spookData ^. the @"videoUrl"
    es <- R.dyn $ R.ffor widgetStateDyn $ \case
      SpookWidgetInitial -> do
        F.div [S.clzSpookButton, S.clzBigButton] $ do
          (_, e) <- RM.mdButton def $ R.text "Spook Others"
          return e
      SpookWidgetFailure e -> R.text (badTokenText e) >> return R.never
      SpookWidgetNewTokens tokens -> do
        appPath <- view getAppPath
        F.h2 [S.clzSpookMessage, S.clzSpookH2, S.mdcThemePrimary] $ R.text "Send out these new Spooks"
        forM_ tokens $ \(Token tok) -> do
          F.div S.clzTokenWrapper $ do
            let link = appPath <> "#" <> tok
            textArea <- R.textArea $ R.TextAreaConfig link R.never $ R.constDyn (Map.fromList [("readonly", ""), ("rows", "1"), ("class", F.unCssClass S.mdcThemePrimary <> " " <> F.unCssClass S.mdcThemeBackground)])
            buttonHtmlEl <- F.div S.clzSpookButton $ do
              (buttonEl, _) <- RM.mdButton def $ R.text "Copy"
              Dom.unsafeCastTo HTMLButtonElement.HTMLButtonElement $ R._element_raw buttonEl
            copyListener <- Dom.liftJSM $ EventM.newListener $ do
              textAreaHtmlEl <- Dom.unsafeCastTo HTMLTextAreaElement.HTMLTextAreaElement $ R._textArea_element textArea
              HTMLTextAreaElement.select textAreaHtmlEl
              window <- Dom.currentWindowUnchecked
              document <- Window.getDocument window
              _ <- Document.execCommand document ("copy" :: Text) False (Nothing @Text)
              return ()
            Dom.liftJSM $ EventM.addListener buttonHtmlEl GlobalEventHandlers.click copyListener True
            return ()
        return $ R.never
    getNewSpooksE <- R.switchHoldPromptly R.never es
  return ()


embedYoutube :: forall t m.
              ( R.DomBuilder t m
              , R.PostBuild t m
              ) => LinkUrl
                -> m ()
embedYoutube (LinkUrl url) = do
  let attrs :: F.Attrs t = mconcat
        [ F.toAttrs (F.widthAttr, "560" :: Text)
        , F.toAttrs (F.heightAttr, "349" :: Text)
        , F.toAttrs (F.srcAttr, url)
        , F.toAttrs (F.frameborderAttr, "0" :: Text)
        , F.toAttrs (F.allowAttr, "autoplay;encrypted-media" :: Text)
        ]
  F.div S.clzSpookVid $ F.iframe attrs (return ())

