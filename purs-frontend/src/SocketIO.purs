module SocketIO
  ( open
  , onError
  , onConnect
  , onDisconnect
  , sender
  , connectProducer
  , errorProducer
  , disconnectProducer
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

foreign import open :: String -> Effect Unit

foreign import onErrorImpl :: (Fn1 String (Effect Unit)) -> Effect Unit

foreign import onConnectImpl :: (Fn1 Unit (Effect Unit)) -> Effect Unit

foreign import onDisconnectImpl :: (Fn1 String (Effect Unit)) -> Effect Unit

foreign import send :: String -> Effect Unit

onError :: (String -> Effect Unit) -> Effect Unit
onError = onErrorImpl <<< mkFn1

onConnect :: (Unit -> Effect Unit) -> Effect Unit
onConnect = onConnectImpl <<< mkFn1

onDisconnect :: (String -> Effect Unit) -> Effect Unit
onDisconnect = onDisconnectImpl <<< mkFn1

connectProducer :: CR.Producer Unit Aff Unit
connectProducer =
  CRA.produce \emitter -> do
    onConnect (\_ -> emit emitter unit)

errorProducer :: CR.Producer String Aff Unit
errorProducer =
  CRA.produce \emitter -> do
    onError $ emit emitter

disconnectProducer :: CR.Producer String Aff Unit
disconnectProducer =
  CRA.produce \emitter -> do
    onDisconnect $ emit emitter

sender :: ∀ m. (m -> String) -> CR.Consumer m Aff Unit
sender extract =
  CR.consumer \msg -> do
    liftEffect $ send (extract msg)
    pure Nothing
