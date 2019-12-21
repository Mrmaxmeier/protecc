module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MainComponent
import SocketIO as SocketIO
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Location (hash)
import Web.HTML.Window as Window
import Effect.Console (log)

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer =
  CRA.produce \emitter -> do
    listener <- DOM.eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
    liftEffect
      $ DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer ::
  (∀ a. MainComponent.Query a -> Aff (Maybe a)) ->
  CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query =
  CR.consumer \event -> do
    let
      hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
    void $ query $ H.tell $ MainComponent.ChangeRoute hash
    pure Nothing

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer1 :: ∀ b. (∀ a. MainComponent.Query a -> Aff (Maybe a)) -> (b -> Unit -> MainComponent.Query Unit) -> CR.Consumer b Aff Unit
wsConsumer1 query f =
  CR.consumer \msg -> do
    void $ query $ H.tell $ f msg
    pure Nothing

wsConsumer :: (∀ a. MainComponent.Query a -> Aff (Maybe a)) -> (Unit -> MainComponent.Query Unit) -> CR.Consumer Unit Aff Unit
wsConsumer query f =
  CR.consumer \msg -> do
    void $ query $ H.tell $ f
    pure Nothing

main :: Effect Unit
main = do
  window <- DOM.window
  location <- Window.location window
  path <- hash location
  SocketIO.open "ws://localhost"
  _ <-
    HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI MainComponent.component unit body
      _ <- io.query $ H.tell $ MainComponent.ChangeRoute $ Str.drop 1 path
      CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
      io.subscribe $ SocketIO.sender (case _ of MainComponent.SocketOutput s -> s)
      CR.runProcess (SocketIO.errorProducer CR.$$ wsConsumer1 io.query MainComponent.SocketError)
      CR.runProcess (SocketIO.disconnectProducer CR.$$ wsConsumer1 io.query MainComponent.SocketDisconnect)
      CR.runProcess (SocketIO.connectProducer CR.$$ wsConsumer io.query MainComponent.SocketConnect)
  pure unit
