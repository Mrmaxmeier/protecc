module Configuration (init, set, subscribe, Configuration, Tag, Service) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId, mapAction)
import Socket (RequestId)
import Socket as Socket

type Tag
  = { slug :: String, name :: String, color :: String }

type Service
  = {}

type Configuration
  = { tags :: Object Tag, services :: Object Service }

foreign import listen :: (Configuration -> Effect Unit) -> Effect Unit

foreign import unlisten :: (Configuration -> Effect Unit) -> Effect Unit

foreign import setImpl :: Configuration -> Effect Unit

foreign import getImpl :: Effect Configuration

init :: ∀ state a slot m. HalogenM state a slot m Aff RequestId
init = Socket.request { watch: "configuration" }

set :: ∀ state a slot m. Configuration -> HalogenM state a slot m Aff Unit
set = H.liftEffect <<< setImpl

source :: EventSource Aff Configuration
source =
  effectEventSource \emitter -> do
    config <- getImpl
    let
      listener = emit emitter
    emit emitter config
    listen listener
    pure $ Finalizer $ unlisten listener

subscribe :: ∀ state a slot m. (Configuration -> a) -> HalogenM state a slot m Aff SubscriptionId
subscribe f = mapAction f $ H.subscribe source
