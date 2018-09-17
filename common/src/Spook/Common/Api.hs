module Spook.Common.Api
  ( Api
  , ImplicitApi
  , FullApi
  ) where


import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Data.Text
import Data.ByteString.Lazy

import Spook.Common.Model
import Spook.Common.Servant

type RealIpHeader = Header "X-Real-Ip" Text
type RefererHeader = Header "Referer" Text

type Api = "api" :> "getSpook" :> ReqBody '[JSON] Token :> Post '[JSON] (Either SpookFailure SpookData)
      :<|> "api" :> "newSpook" :> RefererHeader :> RealIpHeader :> RemoteHost :> ReqBody '[JSON] Token :> Post '[JSON] (Either SpookFailure [Token])
      -- :<|> "log" :> ReqBody '[JSON] StatLog :> RealIpHeader :> RemoteHost :> Post '[JSON] ()

type ImplicitApi = CaptureAll "path" Text :> RefererHeader :> RealIpHeader :> RemoteHost :> Get '[Html] ByteString

type FullApi = (Api :<|> ImplicitApi) :<|> Raw

