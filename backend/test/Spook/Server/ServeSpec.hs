{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spook.Server.ServeSpec where

import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (runNoLoggingT, runStderrLoggingT)
import           Control.Monad.Reader     (ReaderT (..), ask, runReaderT)
import           Control.Monad.State      (StateT)
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.String              (IsString, fromString)
import qualified Data.Text.Lazy           as Text
import qualified Data.Text.Lazy.Encoding  as Text
import           Data.Time                (NominalDiffTime, UTCTime (..),
                                           addUTCTime, getCurrentTime)
import           Data.Time.Calendar       (Day (..))
import qualified Database.Esqueleto       as Es
import qualified Database.Persist         as P
import           Database.Persist.Sqlite
import qualified Network.HTTP.Types       as Network
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Wai hiding (withApplication)
import qualified Network.Wai.Test         as Wai hiding (request)
import           Test.Hspec
import           Test.Hspec.Core.Spec
import qualified Test.Hspec.Wai           as Wai
import qualified Test.Hspec.Wai.Internal  as Wai (withApplication)

import qualified Spook.Common.Model       as CM
import qualified Spook.Server.Model       as SM
import           Spook.Server.Serve       (SiteContext (..), app)

createPool :: IO Es.ConnectionPool
createPool = runStderrLoggingT $ createSqlitePool ":memory:" 1

mkApplication :: Es.ConnectionPool -> Wai.Application
mkApplication dbPool = app $ SiteContext dbPool

setupDb :: Es.ConnectionPool -> IO ()
setupDb dbPool = flip runSqlPool dbPool $ do
  runMigration SM.migrateAll
  insertTestData
  return ()

insertTestData = do
{-
  P.insert SM.Visitor { SM._visitorFullNames = ["Tony Tester"], SM._visitorPassPhrase = "secret", SM._visitorMessage = "message" }
  allowedVisitorId <- P.insert SM.Visitor { SM._visitorFullNames = ["Fiona Finicky", "Fillip Finicky"], SM._visitorPassPhrase = "secret2", SM._visitorMessage = "message2" }
  expiredVisitorId <- P.insert SM.Visitor { SM._visitorFullNames = ["Billy Bob"], SM._visitorPassPhrase = "secret3", SM._visitorMessage = "message3" }
  currentTime <- liftIO getCurrentTime
  let localIp = Just "0.0.0.0"
  P.insert SM.SavedToken { SM._savedTokenVisitor = allowedVisitorId, SM._savedTokenToken = "-good-token-", SM._savedTokenExpiry = addUTCTime (100 * 60) currentTime, SM._savedTokenIp = localIp }
  P.insert SM.SavedToken { SM._savedTokenVisitor = expiredVisitorId, SM._savedTokenToken = "-expired-token-", SM._savedTokenExpiry = currentTime, SM._savedTokenIp = localIp }
  P.insert SM.SavedToken { SM._savedTokenVisitor = expiredVisitorId, SM._savedTokenToken = "-wrongip-token-", SM._savedTokenExpiry = currentTime, SM._savedTokenIp = Just "1.2.3.4" }
  -}
  return ()

renderJson :: (Aeson.ToJSON i, IsString o) => i -> o
renderJson = fromString . Text.unpack . Text.decodeUtf8 . Aeson.encode . Aeson.toJSON

type S a = Wai.WaiSession a

post :: Aeson.ToJSON i => BS.ByteString -> i -> S Wai.SResponse
post path = -- ReaderT . const .
  Wai.request Network.methodPost path [("Content-Type", "application/json")] . renderJson

getResult :: Aeson.FromJSON a => Wai.SResponse -> S a
getResult response = maybe (fail "Could not parse response.") return $ Aeson.decode $ Wai.simpleBody response

shouldRespondWith :: S Wai.SResponse -> Wai.ResponseMatcher -> S ()
shouldRespondWith = Wai.shouldRespondWith

shouldRespondWithJson :: Aeson.ToJSON res => S Wai.SResponse -> res -> S ()
shouldRespondWithJson response expectedResponse = shouldRespondWith response (renderJson expectedResponse)

withConnection :: ActionWith Es.ConnectionPool -> (Es.ConnectionPool -> IO ()) -> ActionWith Es.ConnectionPool
withConnection action f connection = do
  f connection
  action connection

withApp ::  (Es.ConnectionPool -> S a) -> Es.ConnectionPool -> IO a
withApp mkS dbPool = Wai.withApplication (mkApplication dbPool) $ mkS dbPool

withApp_ ::  S a -> Es.ConnectionPool -> IO a
withApp_ s dbPool = Wai.withApplication (mkApplication dbPool) s

spec :: Spec
spec = return ()
{-
  beforeAll (createPool >>= \dbPool -> setupDb dbPool >> return dbPool) $ {- aroundWith (withConnection setupDb) $ -} describe "KlenLi Server" $ do
  describe "token route" $ do
    it "Allows getting a token from a correct passphrase" $ withApp_ $
      post "/token" (CM.PassPhrase "secret") `shouldRespondWith` 200
    it "Fails getting a token for an incorrect passphrase" $ withApp_ $
      post "/token" (CM.PassPhrase "secret-bad") `shouldRespondWith` 400
  describe "get data route" $ do
    it "Responds with data for good unexpired token" $ withApp_ $ do
      siteData :: CM.SiteData <- post "/sitedata" (CM.Token "-good-token-") >>= getResult
      liftIO $ do
        siteData ^. CM.dataVisitorFullNames `shouldBe` ["Fiona Finicky", "Fillip Finicky"]
        siteData ^. CM.dataVisitorMessage `shouldBe` "message2"
    it "Fails if token isn't found" $ withApp_ $
      post "/sitedata" (CM.Token "-nonexistent-token-") `shouldRespondWith` 400
    it "Fails if token expired" $ withApp_ $
      post "/sitedata" (CM.Token "-expired-token-") `shouldRespondWith` 400
    it "Fails if used with another ip" $ withApp_ $
      post "/sitedata" (CM.Token "-wrongip-token-") `shouldRespondWith` 400

  -- let getToken :: Wai.WaiSession CM.Token = post "/token" (CM.PassPhrase "secrets") >>= getResult
  describe "rsvp route" $
    it "Writes rsvps to db" $ withApp $ \dbPool -> do
      post
        "/rsvp"
        ( CM.Token "-good-token-"
        , [ CM.Rsvp "Bogus Bob" "bogus.bob@gmail.com" True "Message1" CM.MealPreferenceMeat "Good food only" True Nothing
          , CM.Rsvp "Legit Leroy" "legit.leroy@gmail.com" True "Message2" CM.MealPreferenceVegetarian "Veg" False (Just "Jojo")
          ]
        ) `shouldRespondWith` 200
      dbLookupResults :: [(Es.Value (Es.Key SM.Visitor), Es.Value (Es.Key SM.SavedToken), Es.Entity SM.RsvpData)] <- liftIO $ flip runSqlPool dbPool $
        Es.select $ Es.from $ \(visitor `Es.InnerJoin` savedToken `Es.InnerJoin` rsvp) -> do
          Es.on $ visitor Es.^. SM.VisitorId Es.==. rsvp Es.^. SM.RsvpDataVisitor
          Es.on $ rsvp Es.^.SM.RsvpDataSavedToken Es.==. savedToken Es.^. SM.SavedTokenId
          Es.where_ $ visitor Es.^. SM.VisitorPassPhrase Es.==. Es.val "secret2"
          return (visitor Es.^. SM.VisitorId, savedToken Es.^. SM.SavedTokenId, rsvp)
      let visitorId = Es.unValue . view _1 $ head dbLookupResults
          savedTokenId = Es.unValue . view _2 $ head dbLookupResults
          rsvps = Es.entityVal . view _3 <$> dbLookupResults
      liftIO $ (scrubDates <$> rsvps) `shouldMatchList`
        [ SM.RsvpData visitorId savedTokenId "Bogus Bob" "bogus.bob@gmail.com" True "Message1" CM.MealPreferenceMeat "Good food only" Nothing zeroTime True
        , SM.RsvpData visitorId savedTokenId "Legit Leroy" "legit.leroy@gmail.com" True "Message2" CM.MealPreferenceVegetarian "Veg" (Just "Jojo") zeroTime False
        ]
-}

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) 0

-- scrubDates :: SM.RsvpData -> SM.RsvpData
-- scrubDates rsvp = rsvp { SM._rsvpDataSubmitTime = zeroTime }
