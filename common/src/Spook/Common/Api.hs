module Spook.Common.Api
  ( Api
--   , ImplicitApi
  , FullApi
  , AccessControlAllowOriginHeader
  ) where


import Servant.API
import Data.Text
import Data.ByteString.Lazy

import Spook.Common.Model
import Spook.Common.Servant

type RealIpHeader = Header "X-Real-Ip" Text
type RefererHeader = Header "Referer" Text
type AccessControlAllowOriginHeader = Header "Access-Control-Allow-Origin" Text

type Response a = Post '[JSON] a

type Api = "api" :> "getSpook" :> ReqBody '[JSON] Token :> Response (Either SpookFailure SpookData)
      :<|> "api" :> "newSpook" :> RefererHeader :> RealIpHeader :> RemoteHost :> ReqBody '[JSON] Token :> Response (Either SpookFailure [Token])
      -- :<|> "log" :> ReqBody '[JSON] StatLog :> RealIpHeader :> RemoteHost :> Post '[JSON] ()

-- type ImplicitApi = "s" :> Capture "tokenAttempt" Text :> Get '[Html] ByteString

type FullApi = Api :<|> Raw
-- type FullApi = (Api :<|> ImplicitApi) :<|> Raw

