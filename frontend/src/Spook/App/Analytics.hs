module Spook.App.Analytics
  ( analyticsScript
  , analyticsPageView
  , analyticsEvent
  , analyticsSetUserId
  ) where

import           Control.Monad               (void)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           JSDOM.Types                 (JSString, JSVal, MonadJSM,
                                              liftJSM, toJSVal)
import           Language.Javascript.JSaddle (jsg2, jsg3, jsg5, jsgf)


ga :: JSString
ga = "ga"

js_ga_2 :: MonadJSM m => Text -> Text -> m ()
js_ga_2 a b = void $ liftJSM $ jsg2 ga a b

js_ga_3 :: MonadJSM m => Text -> Text -> Text -> m ()
js_ga_3 a b c = void $ liftJSM $ jsg3 ga a b c

js_ga_5 :: MonadJSM m => Text -> Text -> Text -> Text -> Text -> m ()
js_ga_5 a b c d e = void $ liftJSM $ jsg5 ga a b c d e

js_ga_6 :: MonadJSM m => JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> m ()
js_ga_6 a b c d e f = void $ liftJSM $ jsgf ga [a, b, c, d, e, f]

analyticsPageView :: MonadJSM m => Text -> m ()
analyticsPageView page = do
  js_ga_3 "set" "page" page
  js_ga_2 "send" "pageview"

analyticsSetUserId :: MonadJSM m => Text -> m ()
analyticsSetUserId userId = js_ga_3 "set" "userId" userId

-- ga('send', 'event', [eventCategory], [eventAction], [eventLabel], [eventValue]);
analyticsEvent :: MonadJSM m => Text -> Text -> Text -> Maybe Int -> m ()
analyticsEvent category action label Nothing = js_ga_5 "send" "event" category action label
analyticsEvent category action label (Just value) = do
  send' <- liftJSM $ toJSVal ("send" :: Text)
  event' <- liftJSM $ toJSVal ("event" :: Text)
  category' <- liftJSM $ toJSVal category
  action' <- liftJSM $ toJSVal action
  label' <- liftJSM $ toJSVal label
  value' <- liftJSM $ toJSVal value
  js_ga_6 send' event' category' action' label' value'


trackingId :: Text.Text
trackingId = "UA-72402075-3"

analyticsScript :: Text.Text
analyticsScript = Text.unlines
  [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
  , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
  , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
  , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
  , "ga('create', '"<>trackingId<>"', 'auto');"
  ]

