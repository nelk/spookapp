module Spook.App.Common where

import qualified Reflex.Dom as R
import qualified Data.Map as Map
import Data.Text (Text)

-- |Use to delay triggering of an action until just after the given event fires.
-- Use this to trigger Rpcs when you need a behavior fed to it to have the new
-- value of an event updating it when that same event is triggering the Rpc
-- (i.e. input behavior and triggering event come from same Dynamic).
fireNextFrame :: R.MonadWidget t m
              => R.Event t input
              -> (R.Event t input -> m result)
              -> m result
fireNextFrame inputE exec = do
  delayedInputE <- R.performEvent $ return <$> inputE
  exec delayedInputE

attrHideIf :: R.Reflex t => R.Dynamic t a -> (a -> Bool) -> R.Dynamic t (Map.Map Text Text)
attrHideIf e f = R.ffor e $ \a ->
  if f a then Map.singleton "style" "display: none" else Map.empty

