module Spook.Common.Servant where

import Servant.API
import Network.HTTP.Media ((//), (/:))
import Data.ByteString.Lazy

data Jpeg

instance Accept Jpeg where
  contentType _ = "image" // "jpeg"

instance MimeRender Jpeg ByteString where
  mimeRender _ b = b

instance MimeUnrender Jpeg ByteString where
  mimeUnrender _ = Right

data Html

instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html ByteString where
  mimeRender _ b = b

instance MimeUnrender Html ByteString where
  mimeUnrender _ = Right
