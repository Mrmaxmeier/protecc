module Socket
  ( init
  , get
  , subscribe
  , messageSource
  , cancel
  , request
  , subscribeResponses
  , RequestId
  , subscribeConnect
  , errorId
  , subscribeResponse
  ) where

import Halogen.HTML.CSS
import Prelude
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error)
import Effect.Random (randomInt)
import Foreign.Object (Object)
import Halogen as H
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId, mapAction)
import SocketIO as SIO
import Util (mwhen)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (cancel)
import Web.HTML.Location (host, hostname, port, protocol)
import Web.HTML.Window (location)

foreign import get :: Effect SIO.Socket

foreign import set :: SIO.Socket -> Effect Unit

foreign import registerListener :: RequestId -> (Json -> Effect Unit) -> Effect Unit

foreign import removeListener :: RequestId -> Effect Unit

foreign import getListener :: RequestId -> (∀ a. Maybe a) -> (∀ a. a -> Maybe a) -> Effect (Maybe (Json -> Effect Unit))

type StreamMessage
  = { id :: RequestId
    , payload :: Json
    }

newtype RequestId
  = RequestId Int

errorId :: RequestId
errorId = RequestId 0

derive instance eqRequestId :: Eq RequestId

derive newtype instance showRequestId :: Show RequestId

derive newtype instance encodeRequestId :: EncodeJson RequestId

derive newtype instance decodeRequestId :: DecodeJson RequestId

decodeString :: ∀ a. DecodeJson a => String -> Either String a
decodeString s = decodeJson =<< jsonParser s

handleStreamMessage :: String -> Effect Unit
handleStreamMessage s = case decodeString s of
  Left e -> error $ "Error parsing stream message: " <> e
  Right (msg :: StreamMessage) -> do
    listener <- getListener msg.id Nothing Just
    maybe (error $ "No listener registered to handle message " <> s) (\f -> f msg.payload) listener

init :: Effect Unit
init = do
  location <- window >>= location
  host <- host location
  protocol <- protocol location
  socket <- SIO.open $ (if protocol == "http:" then "ws://" else "wss://") <> host
  SIO.onMessage socket "msg" handleStreamMessage
  set socket

subscribe :: ∀ a state slot m. (SIO.Socket -> EventSource Aff a) -> H.HalogenM state a slot m Aff SubscriptionId
subscribe source = do
  socket <- H.liftEffect get
  H.subscribe $ source socket

messageSource :: ∀ a. DecodeJson a => Boolean -> RequestId -> EventSource Aff a
messageSource cancel id =
  effectEventSource \emitter -> do
    registerListener id
      ( \json -> case decodeJson json of
          Left e -> error $ "Could not decode message " <> stringify json <> " with id " <> show id <> ": " <> e
          Right arg -> emit emitter arg
      )
    pure $ mwhen cancel $ Finalizer $ cancelEffect id

subscribeResponses :: ∀ a1 a2 state slot m. DecodeJson a1 => (a1 -> a2) -> RequestId -> H.HalogenM state a2 slot m Aff SubscriptionId
subscribeResponses f = mapAction f <<< H.subscribe <<< messageSource true

subscribeResponse :: ∀ a1 a2 state slot m. DecodeJson a1 => (a1 -> a2) -> RequestId -> H.HalogenM state a2 slot m Aff SubscriptionId
subscribeResponse f = mapAction f <<< H.subscribe <<< messageSource false

subscribeConnect :: ∀ a state slot m. a -> H.HalogenM state a slot m Aff SubscriptionId
subscribeConnect action = mapAction (const action) $ subscribe SIO.connectSource

request :: ∀ o state a slot m. EncodeJson o => o -> H.HalogenM state a slot m Aff RequestId
request o =
  H.liftEffect do
    socket <- get
    id <- randomInt 0 (pow 2 30)
    SIO.send socket "msg" (stringify $ encodeJson { id: id, payload: o })
    pure $ RequestId id

cancelEffect :: RequestId -> Effect Unit
cancelEffect id = do
  socket <- get
  SIO.send socket "msg" (stringify $ encodeJson { id: id, payload: "cancel" })
  removeListener id

cancel :: ∀ state a slot m. RequestId -> H.HalogenM state a slot m Aff Unit
cancel = H.liftEffect <<< cancelEffect
