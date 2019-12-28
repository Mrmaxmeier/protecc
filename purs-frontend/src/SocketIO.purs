module SocketIO
  ( open
  , onError
  , onConnect
  , onDisconnect
  , sender
  , connectSource
  , errorSource
  , disconnectSource
  , reconnectingSource
  , onReconnecting
  , messageSource
  , onMessage
  , Socket
  , send
  ) where

import Prelude
import Control.Coroutine as CR
import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Query.EventSource (EventSource, effectEventSource, emit)

foreign import data Socket :: Type

foreign import open :: String -> Effect Socket

foreign import onErrorImpl :: Socket -> (Fn1 String (Effect Unit)) -> Effect Unit

foreign import onConnectImpl :: Socket -> (Fn1 Unit (Effect Unit)) -> Effect Unit

foreign import onDisconnectImpl :: Socket -> (Fn1 String (Effect Unit)) -> Effect Unit

foreign import onReconnectingImpl :: Socket -> (Fn1 Int (Effect Unit)) -> Effect Unit

foreign import onMessageImpl :: Socket -> String -> (Fn1 String (Effect Unit)) -> Effect Unit

foreign import send :: Socket -> String -> String -> Effect Unit

onError :: Socket -> (String -> Effect Unit) -> Effect Unit
onError socket = onErrorImpl socket <<< mkFn1

onConnect :: Socket -> (Unit -> Effect Unit) -> Effect Unit
onConnect socket = onConnectImpl socket <<< mkFn1

onReconnecting :: Socket -> (Int -> Effect Unit) -> Effect Unit
onReconnecting socket = onReconnectingImpl socket <<< mkFn1

onDisconnect :: Socket -> (String -> Effect Unit) -> Effect Unit
onDisconnect socket = onDisconnectImpl socket <<< mkFn1

onMessage :: Socket -> String -> (String -> Effect Unit) -> Effect Unit
onMessage socket cmd = onMessageImpl socket cmd <<< mkFn1

source :: ∀ a. ((a -> Effect Unit) -> Effect Unit) -> EventSource Aff a
source f =
  effectEventSource \emitter -> do
    f $ emit emitter
    pure mempty

connectSource :: Socket -> EventSource Aff Unit
connectSource = source <<< onConnect

errorSource :: Socket -> EventSource Aff String
errorSource = source <<< onError

disconnectSource :: Socket -> EventSource Aff String
disconnectSource = source <<< onDisconnect

reconnectingSource :: Socket -> EventSource Aff Int
reconnectingSource = source <<< onReconnecting

messageSource :: Socket -> String -> EventSource Aff String
messageSource socket = source <<< onMessage socket

sender :: ∀ m. Socket -> (m -> { cmd :: String, arg :: String }) -> CR.Consumer m Aff Unit
sender socket extract =
  CR.consumer \msg -> do
    let
      e = extract msg
    liftEffect $ send socket e.cmd e.arg
    pure Nothing
