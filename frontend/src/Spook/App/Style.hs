module Spook.App.Style where

import qualified Clay
import qualified Clay.Stylesheet as Clay (key)
import qualified Clay.Selector as Clay (Refinement(..), filterFromText)
import qualified Clay.Media as ClayMedia
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.String (IsString)
import Control.Monad (forM_)
import Data.Monoid ((<>))

import qualified Spook.App.Fluent as F

rawCss :: Text -> Text
rawCss serverPath = LazyText.toStrict $ Clay.render $ css serverPath

-- TODO: Quasiquoter for generating all of the below from list of class names.

-- Third-party styles:
clzMaterialIcons :: F.CssClass
clzMaterialIcons = "material-icons"
------

mdcThemePrimary :: F.CssClass
mdcThemePrimary = "mdc-theme--primary"

mdcThemeBackground :: F.CssClass
mdcThemeBackground = "mdc-theme--background"

clzSpookCard :: F.CssClass
clzSpookCard = "spook-card"

clzSpookSection :: F.CssClass
clzSpookSection = "spook-section"

clzSpookTitle :: F.CssClass
clzSpookTitle = "spook-title"

clzSpookMessage :: F.CssClass
clzSpookMessage = "spook-message"

clzTokenWrapper :: F.CssClass
clzTokenWrapper = "token-wrapper"

clzSpookVid :: F.CssClass
clzSpookVid = "spook-vid"

clzSpookButton :: F.CssClass
clzSpookButton = "spook-button"

clzSpookImage :: F.CssClass
clzSpookImage = "spook-image"

clzBigButton :: F.CssClass
clzBigButton = "big-button"

clzSpookH2 :: F.CssClass
clzSpookH2 = "spook-h2"

clzNewSpookList :: F.CssClass
clzNewSpookList = "new-spook-list"

clzDisplayNone :: F.CssClass
clzDisplayNone = "display-none"

objectFit :: Clay.BackgroundSize -> Clay.Css
objectFit = Clay.key "object-fit"

byClass :: F.CssClass -> Clay.Refinement
byClass = Clay.byClass . F.unCssClass

clz :: F.CssClass -> Clay.Selector
clz = (Clay.star Clay.#) . byClass

fontScareArms :: Clay.Css
fontScareArms = Clay.fontFamily ["Scare Arms"] [Clay.sansSerif]

animBlink :: IsString s => s
animBlink = "blink"

fullWidth :: Clay.Css
fullWidth = Clay.width $ Clay.pct 100

fullHeight :: Clay.Css
fullHeight = Clay.height $ Clay.pct 100

zeroOpacity :: Clay.Css
zeroOpacity = Clay.opacity 0

fullOpacity :: Clay.Css
fullOpacity = Clay.opacity 1

zIndexNeg1 :: Clay.Css
zIndexNeg1 = Clay.zIndex $ -1

flex1 :: Clay.Css
flex1 = do
  Clay.flexGrow 1
  Clay.flexShrink 1
  Clay.flexBasis $ Clay.pct 0

noSelect :: Clay.Css
noSelect = Clay.userSelect Clay.none

uniformBorderRadius :: Clay.Size a -> Clay.Css
uniformBorderRadius r = Clay.borderRadius r r r r

-- |Width splitpoint between xs and sm in bootstrap.
desktopWidth :: Clay.Size Clay.LengthUnit
desktopWidth = Clay.px 768

desktop :: Clay.Css -> Clay.Css
desktop = Clay.query Clay.all [ClayMedia.minWidth desktopWidth]

-- TODO: Might need portrait vs landscape orientation.
mobile :: Clay.Css -> Clay.Css
mobile = Clay.query ClayMedia.screen [ClayMedia.maxWidth desktopWidth]

css :: Text -> Clay.Css
css serverPath = do
  Clay.keyframes animBlink
    [ (0, fullOpacity)
    , (50, zeroOpacity)
    , (100, fullOpacity)
    ]

  forM_ [Clay.html, Clay.body] (Clay.? do
    Clay.width $ Clay.pct 100
    Clay.height $ Clay.pct 100
    Clay.backgroundColor $ Clay.rgba 0 0 0 0
    )

  Clay.body Clay.? do
    "--mdc-theme-primary" Clay.-: "#e09100"
    "--mdc-theme-background" Clay.-: "#060300bf"
    Clay.backgroundColor Clay.black

  Clay.fontFace $ do
    Clay.fontFamily ["Scare Arms"] []
    Clay.fontFaceSrc [Clay.FontFaceSrcUrl (serverPath <> "static/scare_arms.otf") $ Just Clay.OpenType]

  -- TODO - Delete all unused parts.
  clz clzSpookCard Clay.? do
    fontScareArms
    let m = Clay.px 30 in Clay.padding m m m m
    Clay.display Clay.flex
    Clay.alignItems Clay.center

    clz clzSpookSection Clay.? do
      Clay.display Clay.flex
      Clay.flexDirection Clay.column
      Clay.width $ Clay.pct 90

    clz clzSpookMessage Clay.? do
      Clay.fontWeight $ Clay.weight 400

    clz clzSpookTitle Clay.? do
      -- let m = Clay.px 30 in Clay.margin m m m m
      Clay.animation animBlink (Clay.ms 1000) Clay.stepStart (Clay.ms 0) Clay.infinite Clay.normal Clay.forwards
      Clay.fontWeight $ Clay.weight 400
      Clay.fontSize $ Clay.em 2.5

    clz clzSpookVid Clay.? do
      Clay.marginTop $ Clay.px 20
      Clay.marginBottom $ Clay.px 30
      Clay.position Clay.relative
      Clay.paddingBottom $ Clay.pct 56.25 -- 16:9
      Clay.paddingTop $ Clay.px 25
      Clay.height $ Clay.px 0

      Clay.iframe Clay.? do
        Clay.position Clay.absolute
        Clay.top $ Clay.px 0
        Clay.left $ Clay.px 0
        fullWidth
        fullHeight

    clz clzTokenWrapper Clay.? do
      fullWidth
      Clay.height $ Clay.px 32
      Clay.display Clay.flex
      Clay.alignItems Clay.center

    Clay.textarea Clay.? do
      "resize" Clay.-: "none"
      Clay.width $ Clay.pct 50
      Clay.marginRight $ Clay.px 30
      Clay.height $ Clay.px 18

    clz clzNewSpookList Clay.? do
      Clay.flexDirection Clay.column
      Clay.alignItems Clay.flexStart

    clz clzSpookButton Clay.? do
      Clay.button Clay.? do
        fontScareArms
    clz clzBigButton Clay.? do
      Clay.button Clay.? do
        Clay.fontSize $ Clay.em 1.8
        Clay.height $ Clay.px 55
        Clay.border Clay.solid (Clay.px 1) Clay.white

    clz clzSpookH2 Clay.? do
      Clay.fontSize $ Clay.em 1.8

    clz clzSpookImage Clay.? do
      fullWidth


-- TODO: Contribute to Clay.
pseudoPlaceholders :: [Clay.Refinement]
pseudoPlaceholders = Clay.filterFromText . (<> "placeholder") <$> placeholderBrowsers
  where placeholderBrowsers :: [Text] =
          [ "::-webkit-input-"
          , "::-moz-"
          , ":-ms-input-"
          , ":-moz-"
          ]

