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

clzSpookVid :: F.CssClass
clzSpookVid = "spook-vid"

clzSpookButton :: F.CssClass
clzSpookButton = "spook-button"

clzBigButton :: F.CssClass
clzBigButton = "big-button"

clzNewSpookList :: F.CssClass
clzNewSpookList = "new-spook-list"

clzTokenWrapper :: F.CssClass
clzTokenWrapper = "token-wrapper"

clzDisplayNone :: F.CssClass
clzDisplayNone = "display-none"

clzNull :: F.CssClass
clzNull = "null"

clzGlowyInput :: F.CssClass
clzGlowyInput = "glowy-input"

clzRoot :: F.CssClass
clzRoot = "root"

clzTab :: F.CssClass
clzTab = "tab"

clzTabContainer :: F.CssClass
clzTabContainer = "tab-container"

clzLogo :: F.CssClass
clzLogo = "logo"

clzPagePanelContainer :: F.CssClass
clzPagePanelContainer = "page-panel-container"

clzPagePanel :: F.CssClass
clzPagePanel = "page-panel"

clzMainMessage :: F.CssClass
clzMainMessage = "main-message"

clzMainMessageContainer :: F.CssClass
clzMainMessageContainer = "main-message-container"

clzMainMessageHeader :: F.CssClass
clzMainMessageHeader = "main-message-header"

clzSelected :: F.CssClass
clzSelected = "selected"

clzUnselected :: F.CssClass
clzUnselected = "unselected"

clzPlainPage :: F.CssClass
clzPlainPage = "plain-page"

clzPageNavPadding :: F.CssClass
clzPageNavPadding = "page-nav-padding"

clzPageBottomPadding :: F.CssClass
clzPageBottomPadding = "page-bottom-padding"

clzCard :: F.CssClass
clzCard = "card"

clzError :: F.CssClass
clzError = "error"

clzPassphraseWidgetContainer, clzPassphraseWidget, clzPassphraseInput, clzPassphraseEnter, clzPassphraseError :: F.CssClass
clzPassphraseWidgetContainer = "passphrase-widget-container"
clzPassphraseWidget = "passphrase-widget"
clzPassphraseInput = "passphrase-input"
clzPassphraseEnter = "passphrase-enter"
clzPassphraseError = "passphrase-error"

clzSplash :: F.CssClass
clzSplash = "splash"

clzSlideshowPage :: F.CssClass
clzSlideshowPage = "slideshow-page"

clzSlideshowContainer :: F.CssClass
clzSlideshowContainer = "slideshow-container"

clzSlideshowImage :: F.CssClass
clzSlideshowImage = "slideshow-image"

clzSlideshowImageCurrent :: F.CssClass
clzSlideshowImageCurrent = "slideshow-image-current"

clzSlideshowImageNext :: F.CssClass
clzSlideshowImageNext = "slideshow-image-next"

clzSlideshowImagePrev :: F.CssClass
clzSlideshowImagePrev = "slideshow-image-prev"

clzSlideshowImageFuture :: F.CssClass
clzSlideshowImageFuture = "slideshow-image-future"

clzSlideshowImagePast :: F.CssClass
clzSlideshowImagePast = "slideshow-image-past"

clzSlideshowImageHidden :: F.CssClass
clzSlideshowImageHidden = "slideshow-image-hidden"

clzSlideshowArrow :: F.CssClass
clzSlideshowArrow = "slideshow-arrow"

clzSlideshowLeftArrow :: F.CssClass
clzSlideshowLeftArrow = "slideshow-left-arrow"

clzSlideshowRightArrow :: F.CssClass
clzSlideshowRightArrow = "slideshow-right-arrow"

clzSlideshowCenterTarget :: F.CssClass
clzSlideshowCenterTarget = "slideshow-center-target"

clzSlideshowPlaySymbol :: F.CssClass
clzSlideshowPlaySymbol = "slideshow-play-symbol"

clzSlideshowPauseSymbol :: F.CssClass
clzSlideshowPauseSymbol = "slideshow-pause-symbol"

clzFullOpacity :: F.CssClass
clzFullOpacity = "full-opacity"

clzOverlayLogo :: F.CssClass
clzOverlayLogo = "overlay-logo"

clzBlurred :: F.CssClass
clzBlurred = "blurred"

clzUnblurred :: F.CssClass
clzUnblurred = "unblurred"

{-
clzFadeIn :: IsString s => s
clzFadeIn = "fade-in"

clzFadeOut :: IsString s => s
clzFadeOut = "fade-out"
-}

clzBrideGroomProfile :: F.CssClass
clzBrideGroomProfile = "bride-groom-profile"

clzProfile :: F.CssClass
clzProfile = "profile"

clzLeftProfile :: F.CssClass
clzLeftProfile = "left-profile"

clzRightProfile :: F.CssClass
clzRightProfile = "right-profile"

clzCombinedProfile :: F.CssClass
clzCombinedProfile = "combined-profile"

clzProfileRow :: F.CssClass
clzProfileRow = "profile-row"

clzProfileText :: F.CssClass
clzProfileText = "profile-text"

clzProfileImage :: F.CssClass
clzProfileImage = "profile-image"

clzRsvpNowButton :: F.CssClass
clzRsvpNowButton = "rsvp-now-button"

clzNiceButton :: F.CssClass
clzNiceButton = "nice-button"

clzNiceInput :: F.CssClass
clzNiceInput = "nice-input"

clzRsvpForm :: F.CssClass
clzRsvpForm = "rsvp-form"

clzFormRow :: F.CssClass
clzFormRow = "form-row"

clzRsvpXButton :: F.CssClass
clzRsvpXButton = "rsvp-x-button"

clzRsvpCheckBox :: F.CssClass
clzRsvpCheckBox = "rsvp-checkbox"

clzRsvpSaveButton :: F.CssClass
clzRsvpSaveButton = "rsvp-save-button"

clzCountdown :: F.CssClass
clzCountdown = "countdown"

clzPageTitle :: F.CssClass
clzPageTitle = "page-title"

clzScheduleStep :: F.CssClass
clzScheduleStep = "schedule-step"

clzScheduleStepBody :: F.CssClass
clzScheduleStepBody = "schedule-step-body"

clzScheduleStepDetails :: F.CssClass
clzScheduleStepDetails = "schedule-step-details"

clzScheduleStepTitle :: F.CssClass
clzScheduleStepTitle = "schedule-step-title"

clzScheduleStepInstructions :: F.CssClass
clzScheduleStepInstructions = "schedule-step-instructions"

clzScheduleStepImage :: F.CssClass
clzScheduleStepImage = "schedule-step-image"

clzScheduleStepTime :: F.CssClass
clzScheduleStepTime = "schedule-step-time"

clzScheduleStepAddress :: F.CssClass
clzScheduleStepAddress = "schedule-step-address"

clzScheduleMap :: F.CssClass
clzScheduleMap = "schedule-map"

animBlink :: IsString s => s
animBlink = "blink"

animFadeIn :: IsString s => s
animFadeIn = "fade-in"

animDelayedHide :: IsString s => s
animDelayedHide = "delayed-hide"

animBlur :: IsString s => s
animBlur = "blur"

objectFit :: Clay.BackgroundSize -> Clay.Css
objectFit = Clay.key "object-fit"

byClass :: F.CssClass -> Clay.Refinement
byClass = Clay.byClass . F.unCssClass

clz :: F.CssClass -> Clay.Selector
clz = (Clay.star Clay.#) . byClass

fontScareArms :: Clay.Css
fontScareArms = Clay.fontFamily ["Scare Arms"] [Clay.sansSerif]

fontMerriweather :: Clay.Css
fontMerriweather = Clay.fontFamily ["Merriweather Sans"] [Clay.sansSerif]

fontAlexBrush :: Clay.Css
fontAlexBrush = Clay.fontFamily ["Alex Brush"] [Clay.cursive]

fontNovaMono :: Clay.Css
fontNovaMono = Clay.fontFamily ["Nova Mono"] [Clay.monospace]

-- Palette.
chineseRed, softPink, angryPink, darkYellow, lightYellow :: Clay.Color
chineseRed = "#aa381e"
softPink = "#ef7674"
angryPink = "#ec5766"
darkYellow = "#e3c16f"
lightYellow = "#faff70"

colorPrimary, colorAccent, colorBackground, colorShadow :: Clay.Color
colorPrimary = chineseRed
colorAccent = angryPink
colorBackground = darkYellow
colorShadow = Clay.rgb 136 136 136

colorTranslucentBack :: Float -> Clay.Color
colorTranslucentBack = Clay.rgba 255 255 255

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

  Clay.keyframesFromTo
    animFadeIn
    zeroOpacity
    fullOpacity
  Clay.keyframesFromTo
    animBlur
    (Clay.filter $ Clay.blur $ Clay.px 0)
    (Clay.filter $ Clay.blur $ Clay.px 20)
  Clay.keyframes
    animDelayedHide
    [(0.4, Clay.visibility Clay.hidden)]

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

  clz clzRoot Clay.? do
    fullWidth
    fullHeight

  -- Input box glow.
  clz clzGlowyInput Clay.? do
    Clay.input Clay.# "type=\"text\"" Clay.? do
      Clay.color colorPrimary
      Clay.border Clay.solid (Clay.px 5) "#dcdcdc"
      Clay.backgroundColor $ colorTranslucentBack 0.5
      Clay.transitions [("box-shadow", 0.3, Clay.linear, 0), ("border", 0.3, Clay.linear, 0)] -- (property, duration, timing fcn, delay)
    Clay.input Clay.# "type=\"text\"" Clay.# Clay.focus Clay.? do
      Clay.border Clay.solid (Clay.px 5) "#707070"
      Clay.boxShadow 0 0 (Clay.px 5) "#969696"
      Clay.transitions [("box-shadow", 1.2, Clay.linear, 0), ("border", 1.2, Clay.linear, 0)] -- (property, duration, timing fcn, delay)

  -- Nav --
  clz clzLogo Clay.? do
    fontAlexBrush
    desktop $ Clay.float Clay.floatLeft
    Clay.whiteSpace Clay.nowrap
    Clay.margin (0 :: Clay.Size Clay.LengthUnit) 0 0 0
    Clay.fontSize $ Clay.pct 250
    Clay.lineHeight $ Clay.pct 150
    Clay.textShadow (Clay.px 5) (Clay.px 3) (Clay.px 4) "#a9a5a5"
    Clay.color "#333333"

  Clay.nav Clay.? do
    fullWidth
    noSelect
    Clay.zIndex 100
    Clay.position Clay.fixed
    Clay.backgroundColor $ colorTranslucentBack 0.8
    clz clzTabContainer Clay.?
      desktop (Clay.float Clay.floatRight)

  clz clzPageNavPadding Clay.? do
    desktop $ Clay.paddingTop (Clay.em 5)
    mobile $ Clay.paddingTop (Clay.em 8)

  clz clzPageBottomPadding Clay.?
    Clay.paddingBottom (Clay.em 10)

  clz clzTab Clay.? do
    fontMerriweather
    fullHeight
    desktop $ do
      Clay.textAlign Clay.center
      Clay.paddingLeft $ Clay.em 1
    Clay.paddingRight $ Clay.em 1
    Clay.minWidth $ Clay.em 7
    Clay.h2 Clay.? do
      Clay.fontSize (Clay.pct 120)
      Clay.fontWeight $ Clay.weight 300
      desktop $ Clay.marginTop $ Clay.px 15
      mobile $ Clay.marginTop $ Clay.px 5
    Clay.display Clay.inlineBlock
    Clay.cursor Clay.pointer
    Clay.color "#333333"
    Clay.textShadow (Clay.px 1) (Clay.px 1) (Clay.px 1) "#888888"
  let tabGreen = "#71cc43"
  clz clzTab Clay.# Clay.hover Clay.?
    Clay.color tabGreen
  clz clzTab Clay.# byClass clzSelected Clay.?
    Clay.color tabGreen

  clz clzPagePanel Clay.? do
    fontMerriweather
    Clay.color "#333333"
    Clay.position Clay.absolute
    fullWidth
    fullHeight
    Clay.minHeight $ Clay.vh 100
    Clay.transitions [("opacity", 0.4, Clay.easeIn, 0), ("visibility", 0.4, Clay.easeIn, 0)]
    zeroOpacity
    Clay.visibility Clay.hidden
  clz clzPagePanel Clay.# byClass clzSelected Clay.? do
    fullOpacity
    Clay.visibility Clay.visible
    -- Clay.animation animFadeIn (Clay.ms 400) Clay.easeIn (Clay.ms 0) (Clay.iterationCount 1) Clay.normal Clay.forwards
  clz clzPagePanel Clay.# byClass clzUnselected Clay.? do
    zeroOpacity
    Clay.visibility Clay.hidden
    -- Clay.animation animFadeIn (Clay.ms 400) Clay.easeIn (Clay.ms 0) (Clay.iterationCount 1) Clay.reverse Clay.forwards
    -- TODO: Make this work.
    -- Clay.animation animDelayedHide (Clay.ms 0) Clay.easeIn (Clay.ms 400) (Clay.iterationCount 1) Clay.normal Clay.forwards

  clz clzFullOpacity Clay.?
    ("opacity" Clay.-: "1 !important")

  clz clzPlainPage Clay.? do
    fullWidth
    fullHeight
    Clay.overflow Clay.auto -- Fixes margin collapsing on top of RSVP page.
    Clay.backgroundColor $ colorTranslucentBack 0.9

  clz clzMainMessageContainer Clay.? do
    Clay.position Clay.fixed
    Clay.bottom $ Clay.px 0
    Clay.paddingLeft $ Clay.px 5
    Clay.paddingRight $ Clay.px 5

  clz clzMainMessage Clay.?
    Clay.textAlign Clay.center

  clz clzMainMessageHeader Clay.? do
    fontAlexBrush
    Clay.fontWeight $ Clay.weight 500
    Clay.fontSize $ Clay.pct 350
    Clay.marginTop $ Clay.px 0
    Clay.marginBottom $ Clay.px 0

  let buttonGreen :: Clay.Color = "#649c47"
  clz clzRsvpNowButton Clay.? do
    Clay.button Clay.? do
      fontMerriweather
      Clay.marginTop $ Clay.em 0.5
      Clay.fontSize $ Clay.pct 156
      Clay.border Clay.solid (Clay.px 2) Clay.black
      Clay.color Clay.black
      Clay.background $ Clay.rgba 0 0 0 0
      uniformBorderRadius $ Clay.px 2
    Clay.button Clay.# Clay.hover Clay.? do
      Clay.background buttonGreen
      Clay.color Clay.white
      Clay.border Clay.solid (Clay.px 2) buttonGreen

  clz clzCountdown Clay.? do
    fontNovaMono
    Clay.fontWeight $ Clay.weight 500
    Clay.fontSize $ Clay.pct 198

  let doForHeartBorderSize doit = do
        mobile $ doit $ Clay.pct 32 `Clay.by` Clay.auto
        desktop $ doit $ Clay.pct 12 `Clay.by` Clay.auto
  clz clzCard Clay.? do
    desktop $ Clay.padding (Clay.em 7) (Clay.em 5) (Clay.em 1) (Clay.em 5)
    mobile $ Clay.padding (Clay.em 7) (Clay.em 1) (Clay.em 1) (Clay.em 1)

    -- Clay.backgroundImage $ Clay.url Assets.heartBorder
    doForHeartBorderSize Clay.backgroundSize
    Clay.backgroundRepeat Clay.noRepeat
    "background-position" Clay.-: "left 0px bottom -5px"

  -- Background pseudo-element on card.
  clz clzCard Clay.# Clay.before Clay.? do
    -- Heart border, and white transparent gradient.
    {-
    Clay.backgroundImages
      [ Clay.url Assets.heartBorder
      , Clay.linearGradient (Clay.angular $ Clay.deg 180) [(Clay.rgba 255 255 255 0, 0), (Clay.rgba 255 255 255 0.7, Clay.px 125)]
      ]

    doForHeartBorderSize $ \borderSize ->
      Clay.backgroundSizes
        [ borderSize
        , Clay.pct 100 `Clay.by` Clay.pct 100
        ]
    Clay.backgroundRepeats [Clay.noRepeat, Clay.noRepeat]
    "background-position" Clay.-: "left 3px bottom -5px, left bottom"
    -}

    Clay.content $ Clay.stringContent ""
    Clay.transform $ Clay.scaleX (-1)
    Clay.position Clay.absolute
    Clay.left $ Clay.em 0
    Clay.bottom $ Clay.em 0
    fullWidth
    fullHeight
    zIndexNeg1

  clz clzError Clay.? Clay.color angryPink

  clz clzPassphraseWidgetContainer Clay.? do
    fullHeight
    Clay.display Clay.flex
    Clay.flexDirection Clay.column
    Clay.justifyContent Clay.center

  clz clzPassphraseWidget Clay.? do
    fontMerriweather

    let responsiveFontSize = Clay.fontSize $ Clay.pct 110
    clz clzPassphraseInput Clay.? do
      Clay.paddingTop $ Clay.em 2
      Clay.input Clay.? do
        responsiveFontSize
        Clay.textAlign Clay.center
        Clay.backgroundColor $ colorTranslucentBack 0.5
        uniformBorderRadius $ Clay.px 2
        fullWidth
      forM_ pseudoPlaceholders $ \refinement ->
        Clay.input Clay.# refinement Clay.?
          Clay.color Clay.gray
    clz clzPassphraseEnter Clay.? do
      responsiveFontSize
      Clay.paddingTop $ Clay.em 0.3
      Clay.button Clay.?
        fullWidth
    clz clzPassphraseError Clay.? do
      Clay.fontSize $ Clay.pct 90
      Clay.paddingTop $ Clay.em 0.3
      Clay.color angryPink
      Clay.textShadow (Clay.px 8) (Clay.px 8) (Clay.px 10) Clay.black

  clz clzNiceButton Clay.? do
    Clay.button Clay.? do
      uniformBorderRadius $ Clay.px 2
      Clay.borderWidth 0
      Clay.backgroundColor Clay.white
      Clay.outline Clay.solid 0 Clay.white
    Clay.button Clay.# Clay.active Clay.?
      Clay.textShadow (Clay.px 2) (Clay.px 2) 0 "#a9a5a5"

  clz clzNiceInput Clay.?
    Clay.borderWidth 0
  clz clzNiceInput Clay.# Clay.attr "type=\"checkbox\"" Clay.# Clay.focus Clay.?
    Clay.outlineColor "#83cd5d"
  clz clzNiceInput Clay.# Clay.focus Clay.? do
    Clay.borderColor "#83cd5d"
    Clay.borderWidth $ Clay.px 1
    "box-shadow" Clay.-: "inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgb(113, 204, 67)"

  clz clzSplash Clay.? do
    Clay.position Clay.fixed
    zIndexNeg1
    fullWidth
    fullHeight
    objectFit Clay.cover
    Clay.key "object-position" $ Clay.pct 15 -- For mobile, move over 15% so both Emily and I are visible.

  clz clzOverlayLogo Clay.? do
    Clay.zIndex 10
    Clay.maxWidth $ Clay.pct 100
    Clay.display Clay.block
    Clay.margin 0 Clay.auto (Clay.em 2) Clay.auto

  clz clzSlideshowPage Clay.? do
    fullHeight
    Clay.paddingTop $ Clay.vh 10
    Clay.paddingBottom $ Clay.vh 10
    Clay.display Clay.flex
    Clay.alignItems Clay.center
    Clay.overflow Clay.hidden

  clz clzSlideshowContainer Clay.? do
    Clay.height $ Clay.vw 60
    Clay.maxHeight $ Clay.vh 80

  -- Percentage width
  let imageW = 80
      gapW = 2
      overhangW = (100 - imageW - 2*gapW) / 2 -- Must be positive
  clz clzSlideshowImage Clay.? do
    Clay.width $ Clay.pct imageW
    Clay.height $ Clay.vh 90
    Clay.position Clay.absolute
    Clay.transitions [("left", Clay.sec 0.5, Clay.ease, 0)
                     ,("padding-left", Clay.sec 0.5, Clay.ease, 0)
                     ,("padding-right", Clay.sec 0.5, Clay.ease, 0)
                     ]
    Clay.img Clay.? do
      Clay.display Clay.block
--      Clay.marginLeft Clay.auto
--      Clay.marginRight Clay.auto
      Clay.maxWidth $ Clay.pct 100
      Clay.maxHeight $ Clay.pct 100
  clz clzSlideshowImageCurrent Clay.?
    Clay.left (Clay.pct $ overhangW + gapW)
  clz clzSlideshowImagePrev Clay.?
    Clay.left (Clay.pct $ overhangW - imageW)
    -- TODO: Use float left and right with transition.
    -- Clay.img Clay.?
      -- Clay.float Clay.floatRight
  clz clzSlideshowImagePast Clay.?
    Clay.left (Clay.pct $ overhangW - 2*imageW - gapW)
    -- Clay.img Clay.?
      -- Clay.float Clay.floatRight
  clz clzSlideshowImageNext Clay.?
    Clay.left (Clay.pct $ 100 - overhangW)
    -- Clay.img Clay.?
      -- Clay.float Clay.floatLeft
  clz clzSlideshowImageFuture Clay.?
    Clay.left (Clay.pct $ 100 - overhangW + imageW + gapW)
    -- Clay.img Clay.?
      -- Clay.float Clay.floatLeft
  clz clzSlideshowImageHidden Clay.? do
    Clay.transition "left" 0 Clay.ease 0 -- Override transition so it instantly snaps while hidden.
    Clay.visibility Clay.hidden

  clz clzSlideshowArrow Clay.? do
    noSelect
    Clay.zIndex 5
    -- Make it quite a large click target.
    Clay.height $ Clay.pct 90
    Clay.width $ Clay.pct 8
    Clay.cursor Clay.pointer
    Clay.position Clay.absolute
    Clay.fontSize $ Clay.vh 10
    let p = Clay.px 5 in Clay.padding p p p p
    Clay.color Clay.white
    Clay.display Clay.flex
    Clay.alignItems Clay.center
  clz clzSlideshowArrow Clay.# Clay.hover Clay.?
    Clay.color "#bbbbbb"
  clz clzSlideshowLeftArrow Clay.? do
    Clay.left (Clay.px 0)
    Clay.justifyContent Clay.flexStart
  clz clzSlideshowRightArrow Clay.? do
    Clay.right (Clay.px 0)
    Clay.justifyContent Clay.flexEnd

  clz clzSlideshowCenterTarget Clay.? do
    Clay.position Clay.absolute
    Clay.zIndex 6
    Clay.top $ Clay.pct 10
    Clay.left $ Clay.pct 10
    Clay.height $ Clay.pct 90
    Clay.width $ Clay.pct imageW
    Clay.cursor Clay.pointer

  forM_ [clzSlideshowPlaySymbol, clzSlideshowPauseSymbol] $ \c -> clz c Clay.? do
    zeroOpacity
    Clay.transition "opacity" (Clay.sec 0.2) Clay.ease 0
    Clay.left $ Clay.pct 50
    Clay.fontSize $ Clay.vh 14

  clz clzDisplayNone Clay.? Clay.display Clay.none

  clz clzBlurred Clay.?
    Clay.animation animBlur (Clay.ms 1000) Clay.easeIn (Clay.ms 0) (Clay.iterationCount 1) Clay.normal Clay.forwards
  clz clzUnblurred Clay.?
    Clay.animation animBlur (Clay.ms 1000) Clay.easeIn (Clay.ms 0) (Clay.iterationCount 1) Clay.reverse Clay.forwards

  clz clzPageTitle Clay.? do
    fontAlexBrush
    Clay.fontWeight $ Clay.weight 500
    Clay.fontSize $ Clay.pct 450

  let profile = do
        Clay.h2 Clay.? do
          fontAlexBrush
          Clay.fontSize $ Clay.pct 400
          Clay.fontWeight $ Clay.weight 500
        Clay.h3 Clay.? Clay.fontSize (Clay.pct 150)
  clz clzProfile Clay.? do
    profile
    Clay.padding 0 (Clay.em 2) (Clay.em 4) (Clay.em 2)
  clz clzBrideGroomProfile Clay.? do
    profile
    Clay.h3 Clay.? Clay.fontSize (Clay.px 16)

  clz clzProfileRow Clay.? do
    fullWidth
    desktop (Clay.display Clay.flex)

  clz clzProfileText Clay.? do
    Clay.display Clay.flex
    Clay.flexDirection Clay.column
    fullHeight
  clz clzProfileImage Clay.** Clay.img Clay.? fullWidth

  clz clzLeftProfile Clay.? do
    desktop $ do
      Clay.display Clay.flex
      Clay.flexDirection Clay.column
      Clay.alignItems Clay.flexEnd
    clz clzProfileText Clay.?
      desktop (Clay.alignItems Clay.flexEnd)
  clz clzRightProfile Clay.? do
    desktop $ do
      Clay.display Clay.flex
      Clay.flexDirection Clay.column
      Clay.alignItems Clay.flexStart
    clz clzProfileText Clay.?
      desktop (Clay.alignItems Clay.flexStart)
    clz clzProfileRow Clay.?
      Clay.flexDirection Clay.rowReverse

  clz clzCombinedProfile Clay.?
    Clay.padding 0 (Clay.em 2) (Clay.em 4) (Clay.em 2)

  clz clzRsvpForm Clay.? do
--    Clay.border Clay.solid (Clay.px 1) Clay.gray
    let p = Clay.em 2
    Clay.padding p p p p
    Clay.marginBottom $ Clay.em 2

{-
    Clay.backgroundImages
      [ parchmentEffect
      , Clay.url Assets.sloppyHeart
      ]
      -}
    Clay.backgroundPositions
      [ Clay.placed Clay.sideLeft Clay.sideTop
      , Clay.positioned (Clay.pct 25) (Clay.pct 40)
      ]
    Clay.backgroundSizes
      [ Clay.px 50 `Clay.by` Clay.px 50
      , Clay.pct 200 `Clay.by` Clay.pct 200
      ]
    Clay.backgroundRepeats [Clay.repeat, Clay.noRepeat]
    Clay.backgroundColor "#fff1ba"

  clz clzFormRow Clay.? do
    Clay.display Clay.flex
    Clay.alignItems Clay.center
    Clay.label Clay.?
      Clay.marginBottom (Clay.px 0)

  clz clzRsvpXButton Clay.? do
    Clay.float Clay.floatRight
    Clay.marginTop $ Clay.em (-1)
    Clay.marginRight $ Clay.em (-1)
    Clay.cursor Clay.pointer

  clz clzRsvpCheckBox Clay.? do
    Clay.width (Clay.em 2.2)
    Clay.height (Clay.em 2.2)

  clz clzRsvpSaveButton Clay.? do
    Clay.margin (Clay.px 5) 0 (Clay.px 5) 0
    Clay.width $ Clay.em 20
    Clay.button Clay.? do
      fullWidth
      Clay.height $ Clay.em 2.5

  let lighterGray = "#555555"
  clz clzScheduleStep Clay.? do
    Clay.marginBottom (Clay.em 4)
    Clay.display Clay.flex
    Clay.flexDirection Clay.column

  clz clzScheduleStepTitle Clay.? do
    fontAlexBrush
    Clay.fontWeight $ Clay.weight 500
    Clay.fontSize $ Clay.pct 380
  clz clzScheduleStepBody Clay.? do
    Clay.display Clay.flex
    mobile $ Clay.flexDirection Clay.column

  clz clzScheduleStepDetails Clay.? do
    flex1
    Clay.display Clay.flex
    Clay.flexDirection Clay.column
    let p = Clay.em 0.5 in Clay.padding p p p p
  forM_ [clzScheduleStepTime, clzScheduleStepAddress] $ \c -> clz c Clay.? do
    Clay.fontSize $ Clay.pct 200
    Clay.fontWeight $ Clay.weight 700
    Clay.color lighterGray
  clz clzScheduleStepInstructions Clay.? do
    Clay.fontSize $ Clay.pct 200
    Clay.color lighterGray
  clz clzScheduleMap Clay.? do
    flex1
    Clay.borderWidth 0
    Clay.marginTop $ Clay.em 1
    mobile $ Clay.marginBottom $ Clay.em 1
    Clay.iframe Clay.? do
      fullWidth
      fullHeight
      Clay.minHeight $ Clay.em 20

  clz clzScheduleStepImage Clay.? do
    flex1
    let p = Clay.em 0.5 in Clay.padding p p p p
    Clay.img Clay.?
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

