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
    visits Int
    parentSpook Text Maybe
    ip Text Maybe
    referrer Text Maybe
    childSpookCount Int
    vidId Text
    creator Text
    claimer Text Maybe
    createTime UTCTime MigrationOnly default=now()
    lastModifiedTime UTCTime MigrationOnly default=now()
    Primary token
    Foreign SavedSpook fkparent parentSpook
    Foreign Visitor fkcreator creator
    Foreign Visitor fkclaimer claimer
    Foreign SpookVid fkvid vidId
    deriving Generic Eq Show

  SpookVidPage
    nextPage Text
    deriving Generic Eq Show

  SpookVid
    vidId Text
    Primary vidId
    deriving Generic Eq Show

  Visitor
    visitorId Text
    Primary visitorId
    deriving Generic Eq Show

  VisitedIp
    visitedIp Text
    visitedCount Int
    redeemedSavedSpooks Int
    Primary visitedIp
    deriving Generic Eq Show
|]
