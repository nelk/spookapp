module Spook.Common.Model where

import           Data.Aeson   (FromJSON (..), ToJSON (..))
import           Data.Text
import qualified Data.Time    as Time
import           GHC.Generics (Generic)

newtype Token = Token {unToken :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data LinkUrl = LinkUrl Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SpookData = SpookData
  { videoUrl   :: LinkUrl
  , token      :: Token
  , numSpooked :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SpookFailure = VisitorAlreadyClaimedSpook | SpookAlreadyClaimed | SpookDoesNotExist | SpookTemporaryFailure
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data StatLog = LogTimeSpent Time.UTCTime
  deriving (Eq, Show, Generic)

magicToken :: Token
magicToken = Token "magictoken23740923"

