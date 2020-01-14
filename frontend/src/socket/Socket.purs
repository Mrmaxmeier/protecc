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
  ) where

import Prelude
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
import Halogen.HTML.CSS
import Foreign.Object (Object)
import Halogen as H
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId, mapAction)
import SocketIO as SIO
import Web.HTML.Event.EventTypes (cancel)

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

instance eqRequestId :: Eq RequestId where
  eq (RequestId a) (RequestId b) = a == b

instance showRequestId :: Show RequestId where
  show (RequestId i) = "RequestId(" <> show i <> ")"

instance encodeRequestId :: EncodeJson RequestId where
  encodeJson (RequestId i) = encodeJson i

instance decodeRequestId :: DecodeJson RequestId where
  decodeJson j = map RequestId $ decodeJson j

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
  socket <- SIO.open "ws://192.168.1.108:4000"
  SIO.onMessage socket "msg" handleStreamMessage
  set socket

subscribe :: ∀ a state slot m. (SIO.Socket -> EventSource Aff a) -> H.HalogenM state a slot m Aff SubscriptionId
subscribe source = do
  socket <- H.liftEffect get
  H.subscribe $ source socket

messageSource :: ∀ a. DecodeJson a => RequestId -> EventSource Aff a
messageSource id =
  effectEventSource \emitter -> do
    registerListener id
      ( \json -> case decodeJson json of
          Left e -> error $ "Could not decode message " <> stringify json <> " with id " <> show id <> ": " <> e
          Right arg -> emit emitter arg
      )
    pure $ Finalizer $ cancelEffect id

subscribeResponses :: ∀ a1 a2 state slot m. DecodeJson a1 => (a1 -> a2) -> RequestId -> H.HalogenM state a2 slot m Aff SubscriptionId
subscribeResponses f = mapAction f <<< H.subscribe <<< messageSource

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
