module SocketIO
  ( open
  , onError
  , onConnect
  , onDisconnect
  , sender
  , connectProducer
  , errorProducer
  , disconnectProducer
  , reconnectingProducer
  , onReconnecting
  , Socket
  ) where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)

foreign import data Socket :: Type

foreign import open :: String -> Effect Socket

foreign import onErrorImpl :: Socket -> (Fn1 String (Effect Unit)) -> Effect Unit

foreign import onConnectImpl :: Socket -> (Fn1 Unit (Effect Unit)) -> Effect Unit

foreign import onDisconnectImpl :: Socket -> (Fn1 String (Effect Unit)) -> Effect Unit

foreign import onReconnectingImpl :: Socket -> (Fn1 Int (Effect Unit)) -> Effect Unit

foreign import send :: Socket -> String -> Effect Unit

onError :: Socket -> (String -> Effect Unit) -> Effect Unit
onError socket = onErrorImpl socket <<< mkFn1

onConnect :: Socket -> (Unit -> Effect Unit) -> Effect Unit
onConnect socket = onConnectImpl socket <<< mkFn1

onReconnecting :: Socket -> (Int -> Effect Unit) -> Effect Unit
onReconnecting socket = onReconnectingImpl socket <<< mkFn1

onDisconnect :: Socket -> (String -> Effect Unit) -> Effect Unit
onDisconnect socket = onDisconnectImpl socket <<< mkFn1

connectProducer :: Socket -> CR.Producer Unit Aff Unit
connectProducer socket =
  CRA.produce \emitter -> do
    onConnect socket (\_ -> emit emitter unit)

errorProducer :: Socket -> CR.Producer String Aff Unit
errorProducer socket =
  CRA.produce \emitter -> do
    onError socket $ emit emitter

disconnectProducer :: Socket -> CR.Producer String Aff Unit
disconnectProducer socket =
  CRA.produce \emitter -> do
    onDisconnect socket $ emit emitter

reconnectingProducer :: Socket -> CR.Producer Int Aff Unit
reconnectingProducer socket =
  CRA.produce \emitter -> do
    onReconnecting socket $ emit emitter

sender :: ∀ m. Socket -> (m -> String) -> CR.Consumer m Aff Unit
sender socket extract =
  CR.consumer \msg -> do
    liftEffect $ send socket (extract msg)
    pure Nothing
