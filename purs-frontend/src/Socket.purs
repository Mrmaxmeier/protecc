module Socket
  ( init
  , get
  , subscribe
  , messageSource
  , closeSource
  , open
  , close
  , StreamId
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Random (randomInt)
import Foreign.Object (Object)
import Halogen as H
import Halogen.Query.EventSource (EventSource, effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId)
import SocketIO as SIO

foreign import get :: Effect SIO.Socket

foreign import set :: SIO.Socket -> Effect Unit

foreign import registerListener :: StreamId -> (Json -> Effect Unit) -> Effect Unit

foreign import registerCloseListener :: StreamId -> (Effect Unit) -> Effect Unit

foreign import removeListener :: StreamId -> Effect Unit

foreign import getListener :: StreamId -> Effect (Maybe (Json -> Effect Unit))

foreign import getCloseListener :: StreamId -> Effect (Maybe (Effect Unit))

type StreamMessage
  = { id :: StreamId
    , arg :: Json
    }

-- newtype CloseMessage = CloseMessage Int
type OpenMessage
  = { id :: StreamId
    , type :: String
    , arg :: Json
    }

newtype StreamId
  = StreamId Int

instance showStreamId :: Show StreamId where
  show (StreamId i) = "StreamId(" <> show i <> ")"

instance encodeStreamId :: EncodeJson StreamId where
  encodeJson (StreamId i) = encodeJson i

instance decodeStreamId :: DecodeJson StreamId where
  decodeJson j = map StreamId $ decodeJson j

decodeString :: ∀ a. DecodeJson a => String -> Either String a
decodeString s = decodeJson =<< jsonParser s

handleStreamMessage :: String -> Effect Unit
handleStreamMessage s = case decodeString s of
  Left error -> log $ "Error parsing stream message: " <> error
  Right (msg :: StreamMessage) -> do
    listener <- getListener msg.id
    maybe (log $ "No listener registered to handle message " <> s) (\f -> f msg.arg) listener

handleCloseMessage :: String -> Effect Unit
handleCloseMessage s = case decodeString s of
  Left error -> log $ "Error parsing close message: " <> error
  Right id -> removeListener id

init :: Effect Unit
init = do
  socket <- SIO.open "ws://localhost:3000"
  SIO.onMessage socket "stream" handleStreamMessage
  SIO.onMessage socket "close" handleCloseMessage
  set socket

subscribe :: ∀ a state slot m. (SIO.Socket -> EventSource Aff a) -> H.HalogenM state a slot m Aff SubscriptionId
subscribe source = do
  socket <- H.liftEffect get
  H.subscribe $ source socket

messageSource :: ∀ a. DecodeJson a => StreamId -> EventSource Aff a
messageSource id =
  effectEventSource \emitter -> do
    registerListener id
      ( \json -> case decodeJson json of
          Left error -> log $ "Could not decode message of id " <> show id <> ": " <> error
          Right arg -> emit emitter arg
      )
    pure mempty

closeSource :: StreamId -> EventSource Aff Unit
closeSource id =
  effectEventSource \emitter -> do
    registerCloseListener id $ emit emitter unit
    pure mempty

open :: ∀ o state a slot m. EncodeJson o => String -> o -> H.HalogenM state a slot m Aff StreamId
open typ o =
  H.liftEffect do
    socket <- get
    id <- randomInt 0 (pow 2 30)
    log $ "random id: " <> show id
    SIO.send socket "open" (stringify $ encodeJson { id: id, arg: encodeJson o, type: typ })
    pure $ StreamId id

close :: ∀ state a slot m. StreamId -> H.HalogenM state a slot m Aff Unit
close id =
  H.liftEffect do
    socket <- get
    SIO.send socket "close" (stringify $ encodeJson id)
    listener <- getCloseListener id
    fromMaybe (pure unit) listener
    removeListener id
