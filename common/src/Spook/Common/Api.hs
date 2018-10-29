module Spook.Common.Api
  ( Api
  , ServerApi
  , FullApi
  , AccessControlAllowOriginHeader
  , SetCookieHeader
  ) where


import Servant.API
import Data.Text
import qualified Data.ByteString as Bs

import Spook.Common.Model

type RealIpHeader = Header "X-Real-Ip" Text
type RefererHeader = Header "Referer" Text
type SetCookieHeader = Header "Set-Cookie" Text
type CookieHeader = Header "Cookie" Text
type AccessControlAllowOriginHeader = Header "Access-Control-Allow-Origin" Bs.ByteString

type Response a = Post '[JSON] a

type GetSpookInner = ReqBody '[JSON] Token :> Response (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
type GetSpookApi inner = "api" :> "getSpook" :> inner
type NewSpookInner = ReqBody '[JSON] Token :> Response (Either SpookFailure [Token])
type NewSpookApi inner = "api" :> "newSpook" :> inner

type WithServerHeaders innerApi = CookieHeader :> RefererHeader :> RealIpHeader :> RemoteHost :> innerApi

type Api = GetSpookApi GetSpookInner
      :<|> NewSpookApi NewSpookInner
      -- :<|> "log" :> ReqBody '[JSON] StatLog :> RealIpHeader :> RemoteHost :> Post '[JSON] ()

type ServerApi = GetSpookApi (WithServerHeaders GetSpookInner)
            :<|> NewSpookApi (WithServerHeaders NewSpookInner)

-- type ImplicitApi = "s" :> Capture "tokenAttempt" Text :> Get '[Html] Bs.ByteString

type FullApi = ServerApi :<|> Raw
-- type FullApi = (Api :<|> ImplicitApi) :<|> Raw

