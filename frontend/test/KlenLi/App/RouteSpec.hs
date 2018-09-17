{-# LANGUAGE ScopedTypeVariables #-}

module KlenLi.App.RouteSpec where

import Test.Hspec
import KlenLi.App.Route
-- import Test.QuickCheck (Arbitrary, arbitrary, shrink, property, arbitraryBoundedEnum, genericShrink)

-- TODO.
{-
instance Arbitrary SubrouteMain where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Route where
  shrink = genericShrink

instance Arbitrary Loc where
  shrink = genericShrink
-}

mainLoc :: Loc
mainLoc = Loc
      { _locRoute = RMain RMainTop
      , _locBrideBioExpanded = False
      , _locGroomBioExpanded = False
      , _locMapSelectedNode = False
      }
mainUrl :: String
mainUrl = "/app/main/#"

mainLocWithFlag :: Loc
mainLocWithFlag = Loc
      { _locRoute = RMain RMainTop
      , _locBrideBioExpanded = True
      , _locGroomBioExpanded = False
      , _locMapSelectedNode = True
      }
mainUrlWithFlag :: String
mainUrlWithFlag = "/app/main/#br=t,map=t"

detailsLoc :: Loc
detailsLoc = Loc
      { _locRoute = RDetails
      , _locBrideBioExpanded = False
      , _locGroomBioExpanded = False
      , _locMapSelectedNode = False
      }
detailsUrl :: String
detailsUrl = "/app/details#"

rsvpLoc :: Loc
rsvpLoc = Loc
      { _locRoute = RRsvp
      , _locBrideBioExpanded = False
      , _locGroomBioExpanded = False
      , _locMapSelectedNode = False
      }
rsvpUrl :: String
rsvpUrl = "/app/rsvp#"

spec :: Spec
spec = describe "RouteFormatter" $ do
  -- it "All routes come back the same through url formatting" $
    -- property $ \(loc :: Loc) -> fromUrl (toUrl loc) `shouldBe` Just loc
  it "Formats main route" $
    toUrl mainLoc `shouldBe` mainUrl
  it "Parses main route" $
    fromUrl mainUrl `shouldBe` Just mainLoc
  it "Formats main route with flags" $
    toUrl mainLocWithFlag `shouldBe` mainUrlWithFlag
  it "Parses main route with flags" $
    fromUrl mainUrlWithFlag `shouldBe` Just mainLocWithFlag
  it "Formats details route" $
    toUrl detailsLoc `shouldBe` detailsUrl
  it "Parses details route" $
    fromUrl detailsUrl `shouldBe` Just detailsLoc
  it "Formats rsvp route" $
    toUrl rsvpLoc `shouldBe` rsvpUrl
  it "Parses rsvp route" $
    fromUrl rsvpUrl `shouldBe` Just rsvpLoc
  it "Parses partial fragments" $ do
    fromUrl "/app/main/" `shouldBe` Just mainLoc
    fromUrl "/app/main/#" `shouldBe` Just mainLoc
