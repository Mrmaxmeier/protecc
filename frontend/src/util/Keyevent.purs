module Keyevent (subscribe, init) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Query.EventSource (EventSource(..), Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId(..), mapAction)

foreign import init :: Effect Unit

foreign import listen :: (Int -> Effect Unit) -> Effect Unit

foreign import unlisten :: (Int -> Effect Unit) -> Effect Unit

source :: Int -> EventSource Aff Unit
source i =
  effectEventSource \emitter -> do
    let
      listener = \j -> when (i == j) $ emit emitter unit
    listen listener
    pure $ Finalizer (unlisten listener)

subscribe :: ∀ a state slot m. Int -> a -> H.HalogenM state a slot m Aff SubscriptionId
subscribe i a = mapAction (const a) $ H.subscribe $ source i
