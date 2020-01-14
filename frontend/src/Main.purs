module Main where

import Prelude
import Control.Coroutine (runProcess, Producer, Consumer, await, ($$))
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainComponent as MainComponent
import Routing.Hash (getHash, hashes)
import Socket as Socket
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Location (hash)
import Web.HTML.Window as Window
import Keyevent as Keyevent

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: Producer String Aff Unit
hashChangeProducer = CRA.produce \emitter -> void $ hashes (\last loc -> maybe (pure unit) (const $ emit emitter loc) last)

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer ::
  (∀ a. MainComponent.Query a -> Aff (Maybe a)) ->
  Consumer String Aff Unit
hashChangeConsumer query =
  forever do
    event <- await
    lift $ void $ query $ H.tell $ MainComponent.ChangeRoute event
    pure Nothing

main :: Effect Unit
main = do
  path <- getHash
  Socket.init
  Keyevent.init
  _ <-
    HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI MainComponent.component path body
      let
        fork = void <<< forkAff <<< runProcess
      fork $ hashChangeProducer $$ hashChangeConsumer io.query
  pure unit
