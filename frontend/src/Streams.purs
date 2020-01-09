module Streams where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML (div, div_)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (mapAction)
import Socket as Socket
import SocketIO as SIO
import Util (logs)
import WindowTable as WindowTable

type Slot
  = ( table :: H.Slot WindowTable.Query (WindowTable.Message Stream) Unit
    )

_table = SProxy :: SProxy "table"

data Action
  = Init
  | WindowTable (WindowTable.Message Stream)
  | SocketConnect

data Query a
  = NoOpQ a

type State
  = { windowStream :: Maybe Socket.RequestId
    }

type Stream
  = { id :: Int
    , client :: Addr
    , server :: Addr
    , tags :: Array Int
    , features :: Object Number
    }

type Segment
  = { sender :: Sender
    , start :: Int
    , timestamp :: Int
    , flags :: Int
    }

newtype Addr
  = Addr (Tuple String Int)

ip :: Addr -> String
ip (Addr t) = fst t

port :: Addr -> Int
port (Addr t) = snd t

instance showAddr :: Show Addr where
  show addr = ip addr <> ":" <> show (port addr)

instance decodeAddr :: DecodeJson Addr where
  decodeJson json = do
    t <- decodeJson json
    Right $ Addr t

instance encodeAddr :: EncodeJson Addr where
  encodeJson (Addr t) = encodeJson t

data Sender
  = Client
  | Server

instance decodeJsonSender :: DecodeJson Sender where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "client" -> Right Client
      "server" -> Right Server
      s -> Left $ s <> " is neither client nor server"

instance encodeJsonSender :: EncodeJson Sender where
  encodeJson = case _ of
    Client -> encodeJson "client"
    Server -> encodeJson "server"

component :: ∀ i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState :: i -> State
  initialState = const { windowStream: Nothing }

  handleAction :: Action -> H.HalogenM State Action Slot o Aff Unit
  handleAction = case _ of
    Init -> void $ Socket.subscribeConnect SocketConnect
    WindowTable msg -> pure unit
    SocketConnect -> do
      stream <- Socket.request { watch: { window: { index: "all", params: WindowTable.initialWindowParams } } }
      _ <- H.modify (_ { windowStream = Just stream })
      pure unit

  showCell :: ∀ o. Show o => o -> H.ComponentHTML (WindowTable.Action Stream) () Aff
  showCell obj = HH.td_ [ HH.text $ show obj ]

  renderRow :: Stream -> Array (H.ComponentHTML (WindowTable.Action Stream) () Aff)
  renderRow stream = [ showCell stream.id, showCell $ stream.client, showCell $ stream.server, showCell stream.tags, showCell stream.features ]

  render :: State -> H.ComponentHTML Action Slot Aff
  render state = HH.slot _table unit (WindowTable.component _.id [ "Id", "Client", "Server", "Tags", "Features" ] renderRow) state.windowStream (Just <<< WindowTable)
