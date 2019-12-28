module Main where

import Prelude
import Control.Coroutine (runProcess, Producer, Consumer, await, ($$))
import Control.Coroutine.Aff as CRA
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MainComponent
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Location (hash)
import Web.HTML.Window as Window
import Socket as Socket

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer =
  CRA.produce \emitter -> do
    listener <- DOM.eventListener (traverse_ (CRA.emit emitter) <<< HCE.fromEvent)
    liftEffect
      $ DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer ::
  (∀ a. MainComponent.Query a -> Aff (Maybe a)) ->
  Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query =
  forever do
    event <- await
    let
      hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
    lift $ void $ query $ H.tell $ MainComponent.ChangeRoute hash
    pure Nothing

main :: Effect Unit
main = do
  window <- DOM.window
  location <- Window.location window
  path <- hash location
  Socket.init
  _ <-
    HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI MainComponent.component (Str.drop 1 path) body
      let
        fork = void <<< forkAff <<< runProcess
      fork $ hashChangeProducer $$ hashChangeConsumer io.query
  pure unit
