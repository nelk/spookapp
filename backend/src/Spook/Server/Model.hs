{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spook.Server.Model where

import           Data.Text
import           Data.Time           (UTCTime)
import           Database.Persist.TH
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  SavedSpook
    token Text
    UniqueToken token
    visits Int
    parent Int Maybe
    ip Text Maybe
    referrer Text Maybe
    childSpookCount Int
    videoUrl Text
    createTime UTCTime MigrationOnly default=now()
    lastModifiedTime UTCTime MigrationOnly default=now()
    deriving Generic Eq Show
|]


    -- Foreign SavedSpook fkparent parent
