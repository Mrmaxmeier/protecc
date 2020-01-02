module Socket
  ( init
  , get
  , subscribe
  , messageSource
  , closeSource
  , open
  , close
  , send
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
import Effect.Console (error)
import Effect.Random (randomInt)
import Foreign.Object (Object)
import Halogen as H
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId)
import SocketIO as SIO

foreign import get :: Effect SIO.Socket

foreign import set :: SIO.Socket -> Effect Unit

foreign import registerListener :: StreamId -> (Json -> Effect Unit) -> Effect Unit

foreign import registerCloseListener :: StreamId -> (Effect Unit) -> Effect Unit

foreign import removeListener :: StreamId -> Effect Unit

foreign import getListener :: StreamId -> (∀ a. Maybe a) -> (∀ a. a -> Maybe a) -> Effect (Maybe (Json -> Effect Unit))

foreign import getCloseListener :: StreamId -> (∀ a. Maybe a) -> (∀ a. a -> Maybe a) -> Effect (Maybe (Effect Unit))

type StreamMessage
  = { id :: StreamId
    , arg :: Json
    }

-- type CloseMessage = StreamId
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
  Left e -> error $ "Error parsing stream message: " <> e
  Right (msg :: StreamMessage) -> do
    listener <- getListener msg.id Nothing Just
    maybe (error $ "No listener registered to handle message " <> s) (\f -> f msg.arg) listener

handleCloseMessage :: String -> Effect Unit
handleCloseMessage s = case decodeString s of
  Left e -> error $ "Error parsing close message: " <> e
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
          Left e -> error $ "Could not decode message with id " <> show id <> ": " <> e
          Right arg -> emit emitter arg
      )
    pure $ Finalizer $ closeEffect id

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
    SIO.send socket "open" (stringify $ encodeJson { id: id, arg: encodeJson o, type: typ })
    pure $ StreamId id

closeEffect :: StreamId -> Effect Unit
closeEffect id = do
  socket <- get
  SIO.send socket "close" (stringify $ encodeJson id)
  listener <- getCloseListener id Nothing Just
  fromMaybe (pure unit) listener
  removeListener id

close :: ∀ state a slot m. StreamId -> H.HalogenM state a slot m Aff Unit
close = H.liftEffect <<< closeEffect

send :: ∀ state o a slot m. EncodeJson o => o -> StreamId -> H.HalogenM state a slot m Aff Unit
send o id =
  H.liftEffect do
    socket <- get
    SIO.send socket "stream" (stringify $ encodeJson $ { id: id, arg: encodeJson o })
