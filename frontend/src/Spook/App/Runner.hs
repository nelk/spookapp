{-# LANGUAGE CPP #-}

-- Derived from jsaddle-warp:
-- https://hackage.haskell.org/package/jsaddle-warp-0.9.5.0/docs/src/Language-Javascript-JSaddle-Warp.html
--
-- Purpose is to add access control allow origin header so that we can serve the warp frontend
-- against a backend on another local port for development.
-- UPDATE: Not needed anymore - preflight cors option checks all handled on server, no need to serve static files with this header.

module Spook.App.Runner (
  customRun
) where

import           Language.Javascript.JSaddle.Types (JSM)
import qualified Reflex.Dom                        as R (run)

customRun :: JSM () -> IO ()
customRun = R.run

{-

#if defined(ghcjs_HOST_OS)

import qualified Language.Javascript.JSaddle.Warp  as JW

customRun = JW.run

#else

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Types as H (status403, status200)
import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)
import qualified Network.Wai as W
       (Application, Request, Response, ResponseReceived, responseLBS, requestMethod, pathInfo)
import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.WebSockets (jsaddleJs, jsaddleOr)
import Language.Javascript.JSaddle.Run.Files (indexHtml)

customRun :: JSM () -> IO ()
customRun jsm = do
  -- port <- maybe 3003 read <$> lookupEnv "JSADDLE_WARP_PORT"
  let port = 3003 :: Int
  putStrLn $ "Running jsaddle-warp server on port " <> show port
  run' port jsm

jsaddleApp' :: W.Application
jsaddleApp' = jsaddleAppWithJs' $ jsaddleJs False

jsaddleAppWithJs' :: ByteString -> W.Application
jsaddleAppWithJs' js req sendResponse =
  jsaddleAppWithJsOr' js
    (\_ _ -> sendResponse $ W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
    req sendResponse

jsaddleAppWithJsOr' :: ByteString -> W.Application -> W.Application
jsaddleAppWithJsOr' js otherApp req sendResponse =
  fromMaybe (otherApp req sendResponse)
    (jsaddleAppPartialWithJs' js req sendResponse)

indexResponse' :: W.Response
indexResponse' = W.responseLBS H.status200 [("Content-Type", "text/html"), ("Access-Control-Allow-Origin", "*")] indexHtml

jsaddleAppPartialWithJs' :: ByteString -> W.Request -> (W.Response -> IO W.ResponseReceived) -> Maybe (IO W.ResponseReceived)
jsaddleAppPartialWithJs' js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse indexResponse'
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

run' :: Int -> JSM () -> IO ()
run' port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp'
#endif

-}

