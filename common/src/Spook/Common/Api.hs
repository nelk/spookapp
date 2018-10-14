module Spook.Common.Api
  ( Api
--   , ImplicitApi
  , FullApi
  , AccessControlAllowOriginHeader
  , SetCookieHeader
  ) where


import Servant.API
import Data.Text
import qualified Data.ByteString.Lazy as BsL
import qualified Data.ByteString as Bs

import Spook.Common.Model
import Spook.Common.Servant

type RealIpHeader = Header "X-Real-Ip" Text
type RefererHeader = Header "Referer" Text
type SetCookieHeader = Header "Set-Cookie" Text
type CookieHeader = Header "Cookie" Text
type AccessControlAllowOriginHeader = Header "Access-Control-Allow-Origin" Bs.ByteString

type Response a = Post '[JSON] a

type Api = "api" :> "getSpook" :> CookieHeader :> ReqBody '[JSON] Token :> Response (Headers '[SetCookieHeader] (Either SpookFailure SpookData))
      :<|> "api" :> "newSpook" :> CookieHeader :> RefererHeader :> RealIpHeader :> RemoteHost :> ReqBody '[JSON] Token :> Response (Either SpookFailure [Token])
      -- :<|> "log" :> ReqBody '[JSON] StatLog :> RealIpHeader :> RemoteHost :> Post '[JSON] ()

-- type ImplicitApi = "s" :> Capture "tokenAttempt" Text :> Get '[Html] Bs.ByteString

type FullApi = Api :<|> Raw
-- type FullApi = (Api :<|> ImplicitApi) :<|> Raw

