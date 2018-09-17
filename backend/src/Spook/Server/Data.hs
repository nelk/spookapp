module Spook.Server.Data where

import           Control.Lens
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import qualified Data.Time.Calendar   as Calendar
import           Spook.Common.Model
import           Spook.Server.KeyPerm (encryptBackendKey)
import           Spook.Server.Model

pst :: Time.TimeZone
pst = Time.TimeZone (-8*60) False "PST"

eventTime :: Time.UTCTime
eventTime = Time.localTimeToUTC pst $ Time.LocalTime (Calendar.fromGregorian 2018 10 31) (Time.TimeOfDay 0 0 0)

