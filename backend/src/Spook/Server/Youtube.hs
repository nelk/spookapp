module Spook.Server.Youtube where

import Servant.API
import Servant.Client (client, ClientM, runClientM, ServantError, BaseUrl(..), Scheme(Https), ClientEnv(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types
import Control.Monad (mzero)
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (Manager)

{-
{
  "kind": "youtube#searchListResponse",
  "etag": etag,
  "nextPageToken": string,
  "prevPageToken": string,
  "regionCode": string,
  "pageInfo": {
    "totalResults": integer,
    "resultsPerPage": integer
  },
  "items": [
    search Resource
  ]
}
-}
data YoutubeSearchResponse = YoutubeSearchResponse
  { resources :: [YoutubeResource]
  , nextPageToken :: Text
  }
  deriving (Generic, Show)

instance FromJSON YoutubeSearchResponse where
  parseJSON (Object o) =
    YoutubeSearchResponse <$> o .: "items"
                          <*> o .: "nextPageToken"
  parseJSON _ = mzero


{-
   {
  "kind": "youtube#searchResult",
  "etag": etag,
  "id": {
    "kind": string,
    "videoId": string,
    "channelId": string,
    "playlistId": string
  },
  "snippet": {
    "publishedAt": datetime,
    "channelId": string,
    "title": string,
    "description": string,
    "thumbnails": {
      (key): {
        "url": string,
        "width": unsigned integer,
        "height": unsigned integer
      }
    },
    "channelTitle": string,
    "liveBroadcastContent": string
  }
}
-}
data YoutubeResource = YoutubeResource
  { videoId :: Text
  }
  deriving (Generic, Show)

instance FromJSON YoutubeResource where
  parseJSON (Object o) =
    YoutubeResource <$> ((o .: "id") >>= (.: "videoId"))
  parseJSON _ = mzero

data YoutubeRequest = YoutubeRequest
  { key :: Text
  , q :: Text
  , maxResults :: Int
  , part :: Text
  , pageToken :: Maybe Text
  }
  deriving (Generic, Show)

type YoutubeApi = "youtube" :> "v3" :> "search"
               :> QueryParam "key" Text
               :> QueryParam "q" Text
               :> QueryParam "maxResults" Int
               :> QueryParam "part" Text
               :> QueryParam "pageToken" Text
               :> Get '[JSON] YoutubeSearchResponse

youtubeClient :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM YoutubeSearchResponse
youtubeClient = client (Proxy :: Proxy YoutubeApi)

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "www.googleapis.com" 443 ""

searchYoutube :: Manager -> YoutubeRequest -> IO (Either ServantError YoutubeSearchResponse)
searchYoutube m r = runClientM (youtubeClient (Just $ key r) (Just $ q r) (Just $ maxResults r) (Just $ part r) (pageToken r)) (ClientEnv m baseUrl)

