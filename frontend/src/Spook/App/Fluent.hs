{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spook.App.Fluent
  ( module Spook.App.Fluent
  , CssClass(..)
  ) where

import Prelude hiding (div)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import           Data.Generics.Product              (the)
import Data.Map (Map)
import Data.Default (Default, def)
import Data.String (IsString, fromString)
import qualified Data.Map as Map
import qualified Reflex.Dom as R
import Reflex.Material.Types (CssClass(..))
import Control.Lens ((^.), (.~), (&))
import GHC.Generics

{-
newtype CssClass = CssClass Text
  deriving (Show, Eq)
unCssClass :: CssClass -> Text
unCssClass (CssClass c) = c
-}

instance IsString CssClass where
  fromString = CssClass . Text.pack

data Attrs t = Attrs
  { attrsStatic :: Map Text Text
  , attrsDynamic :: R.Dynamic t (Map Text Text)
  }
  deriving (Generic)

attrsToDynMap :: R.Reflex t => Attrs t -> R.Dynamic t (Map Text Text)
attrsToDynMap Attrs{..} = mergeAttrMaps attrsStatic <$> attrsDynamic

instance R.Reflex t => Default (Attrs t) where
  def = Attrs Map.empty $ R.constDyn Map.empty

instance R.Reflex t => Monoid (Attrs t) where
  mempty = def
  Attrs{attrsStatic = aSt, attrsDynamic = aDyn} `mappend` Attrs{attrsStatic = bSt, attrsDynamic = bDyn} = Attrs
    { attrsStatic = mergeAttrMaps aSt bSt
    , attrsDynamic = mergeAttrMaps <$> aDyn <*> bDyn
    }

class ToAttrs t a where
  toAttrs :: a -> Attrs t

instance ToAttrs t (Attrs t) where
  toAttrs = id

-- instance (ToAttrs t a1, ToAttrs t a2, R.Reflex t) => ToAttrs t (a1, a2) where
--   toAttrs (a1, a2) = toAttrs a1 <> toAttrs a2

instance (ToAttrs t a1, ToAttrs t a2, ToAttrs t a3, R.Reflex t) => ToAttrs t (a1, a2, a3) where
  toAttrs (a1, a2, a3) = mconcat [toAttrs a1, toAttrs a2, toAttrs a3]

instance (ToAttrs t a1, ToAttrs t a2, ToAttrs t a3, ToAttrs t a4, R.Reflex t) => ToAttrs t (a1, a2, a3, a4) where
  toAttrs (a1, a2, a3, a4) = mconcat [toAttrs a1, toAttrs a2, toAttrs a3, toAttrs a4]

instance (ToAttrs t a1, ToAttrs t a2, ToAttrs t a3, ToAttrs t a4, ToAttrs t a5, R.Reflex t) => ToAttrs t (a1, a2, a3, a4, a5) where
  toAttrs (a1, a2, a3, a4, a5) = mconcat [toAttrs a1, toAttrs a2, toAttrs a3, toAttrs a4, toAttrs a5]

instance (ToAttrs t a1, ToAttrs t a2, ToAttrs t a3, ToAttrs t a4, ToAttrs t a5, ToAttrs t a6, R.Reflex t) => ToAttrs t (a1, a2, a3, a4, a5, a6) where
  toAttrs (a1, a2, a3, a4, a5, a6) = mconcat [toAttrs a1, toAttrs a2, toAttrs a3, toAttrs a4, toAttrs a5, toAttrs a6]

instance (ToAttrs t a1, ToAttrs t a2, ToAttrs t a3, ToAttrs t a4, ToAttrs t a5, ToAttrs t a6, ToAttrs t a7, R.Reflex t) => ToAttrs t (a1, a2, a3, a4, a5, a6, a7) where
  toAttrs (a1, a2, a3, a4, a5, a6, a7) = mconcat [toAttrs a1, toAttrs a2, toAttrs a3, toAttrs a4, toAttrs a5, toAttrs a6, toAttrs a7]

instance R.Reflex t => ToAttrs t [Attrs t] where
  toAttrs = mconcat

instance R.Reflex t => ToAttrs t (Map Text Text) where
  toAttrs attrs = def {attrsStatic = attrs}

instance R.Reflex t => ToAttrs t (R.Dynamic t (Map Text Text)) where
  toAttrs attrs = (def :: Attrs t) & the @"attrsDynamic" .~ attrs

instance R.Reflex t => ToAttrs t (Text, Text) where
  toAttrs (k, v) = def {attrsStatic = Map.singleton k v}

instance R.Reflex t => ToAttrs t CssClass where
  toAttrs c = def {attrsStatic = Map.singleton classAttr (unCssClass c)}

instance R.Reflex t => ToAttrs t [CssClass] where
  toAttrs cs = def {attrsStatic = foldl mergeAttrMaps Map.empty (Map.singleton classAttr . unCssClass <$> cs)}

instance R.Reflex t => ToAttrs t (R.Dynamic t CssClass) where
  toAttrs csDyn = (def :: Attrs t) & the @"attrsDynamic" .~ fmap (Map.singleton classAttr . unCssClass) csDyn

instance R.Reflex t => ToAttrs t (R.Dynamic t [CssClass]) where
  toAttrs csDyn = (def :: Attrs t) & the @"attrsDynamic" .~ fmap (\cs -> foldl mergeAttrMaps Map.empty (Map.singleton classAttr . unCssClass <$> cs)) csDyn

instance R.Reflex t => ToAttrs t (Text, R.Dynamic t Text) where
  toAttrs (attrName, dyn) = (def :: Attrs t) & the @"attrsDynamic" .~ (Map.singleton attrName <$> dyn)

instance R.Reflex t => ToAttrs t (CssClass, R.Dynamic t Bool) where
  toAttrs (c, dyn) = (def :: Attrs t) & the @"attrsDynamic" .~ ((\b -> if b then Map.singleton classAttr (unCssClass c) else def) <$> dyn)

instance R.Reflex t => ToAttrs t () where
  toAttrs _ = def


classAttr :: Text
classAttr = "class"

styleAttr :: Text
styleAttr = "style"

srcAttr :: Text
srcAttr = "src"

hrefAttr :: Text
hrefAttr = "href"

targetAttr :: Text
targetAttr = "target"

placeholderAttr :: Text
placeholderAttr = "placeholder"

autofocusAttr :: Text
autofocusAttr = "autofocus"

widthAttr :: Text
widthAttr = "width"

heightAttr :: Text
heightAttr = "height"

frameborderAttr :: Text
frameborderAttr = "frameborder"

allowAttr :: Text
allowAttr = "allow"

mergeAttrMaps :: Map Text Text -> Map Text Text -> Map Text Text
mergeAttrMaps = Map.mergeWithKey (\k _a _b -> Just $ if k `elem` [classAttr, styleAttr] then _a <> " " <> _b else _b) id id

type JoinAttr t = forall a b. (ToAttrs t a, ToAttrs t b, R.Reflex t) => a -> b -> Attrs t

infixr 6 <.>
(<.>) :: forall t. JoinAttr t
_a <.> _b = toAttrs _a <> toAttrs _b

type El t m = R.Element R.EventResult (R.DomBuilderSpace m) t
type FluentTag = forall t m a b. (R.DomBuilder t m, R.PostBuild t m, ToAttrs t a) => a -> m b -> m b
type FluentTag' = forall t m a b. (R.DomBuilder t m, R.PostBuild t m, ToAttrs t a) => a -> m b -> m (El t m, b)
type FluentVoidTag = forall t m a. (R.DomBuilder t m, R.PostBuild t m, ToAttrs t a) => a -> m ()
type FluentVoidTag' = forall t m a. (R.DomBuilder t m, R.PostBuild t m, ToAttrs t a) => a -> m (El t m)

tagImpl :: Text -> FluentTag
tagImpl tagName attrs = (snd <$>) . tagImpl' tagName attrs

tagImpl' :: Text -> FluentTag'
tagImpl' tagName attrs child = do
  let attrs' = toAttrs attrs
      combinedDynamic = mergeAttrMaps (attrs' ^. the @"attrsStatic") <$> attrs' ^. the @"attrsDynamic"
  modifyAttrs <- R.dynamicAttributesToModifyAttributes combinedDynamic
  let cfg = def
        & R.initialAttributes .~ Map.mapKeys (R.AttributeName Nothing) (attrs' ^. the @"attrsStatic")
        & R.modifyAttributes .~ (Map.mapKeys (R.AttributeName Nothing) <$> modifyAttrs)
  R.element tagName cfg child

a, div, span, p, article, section, label, img, h1, h2, h3, h4, nav, table, thead, tr, td, i, iframe :: FluentTag
a = tagImpl "a"
div = tagImpl "div"
span = tagImpl "span"
p = tagImpl "p"
article = tagImpl "article"
section = tagImpl "section"
label = tagImpl "label"
img = tagImpl "img"
h1 = tagImpl "h1"
h2 = tagImpl "h2"
h3 = tagImpl "h3"
h4 = tagImpl "h4"
nav = tagImpl "nav"
table = tagImpl "table"
thead = tagImpl "thead"
tr = tagImpl "tr"
td = tagImpl "td"
i = tagImpl "i"
iframe = tagImpl "iframe"

a', div', span', p', article', section', label', img', h1', h2', h3', h4', nav', table', thead', tr', td', i', iframe' :: FluentTag'
a' = tagImpl' "a"
div' = tagImpl' "div"
span' = tagImpl' "span"
p' = tagImpl' "p"
article' = tagImpl' "article"
section' = tagImpl' "section"
label' = tagImpl' "label"
img' = tagImpl' "img"
h1' = tagImpl' "h1"
h2' = tagImpl' "h2"
h3' = tagImpl' "h3"
h4' = tagImpl' "h4"
nav' = tagImpl' "nav"
table' = tagImpl' "table"
thead' = tagImpl' "thead"
tr' = tagImpl' "tr"
td' = tagImpl' "td"
i' = tagImpl' "i"
iframe' = tagImpl' "iframe"

br :: FluentVoidTag
br attrs = tagImpl "br" attrs (return ())

br' :: FluentVoidTag'
br' attrs = fst <$> tagImpl' "br" attrs (return ())

