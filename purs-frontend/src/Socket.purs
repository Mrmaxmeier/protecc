module Socket (init, get, subscribe) where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.HalogenM (SubscriptionId)
import Effect.Aff (Aff)
import SocketIO as SIO

foreign import get :: Effect SIO.Socket

foreign import set :: SIO.Socket -> Effect Unit

init :: Effect Unit
init = set =<< SIO.open "ws://localhost:3000"

subscribe :: ∀ a action state slot m. (SIO.Socket -> EventSource Aff a) -> (a -> action) -> H.HalogenM state action slot m Aff SubscriptionId
subscribe source action = do
  socket <- H.liftEffect get
  H.subscribe $ map action (source socket)
