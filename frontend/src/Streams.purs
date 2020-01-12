module Streams where

import Prelude
import CSS as CSS
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML (div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import Numeral (formatBytes)
import SemanticUI (sdiv)
import SemanticUI as S
import Socket (RequestId)
import Socket as Socket
import SocketIO as SIO
import Stream (Addr)
import Stream as Stream
import Tags as Tags
import Util (Id, css, logj, logo, logs, mwhen)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import WindowTable (Message, RowDescription)
import WindowTable as WindowTable

type Slot a
  = ( table :: H.Slot WindowTable.Query (WindowTable.Message Stream) Unit
    , tags :: H.Slot a Void Id
    , streamDetails :: H.Slot a Void Unit
    )

_table = SProxy :: SProxy "table"

_tags = SProxy :: SProxy "tags"

_streamDetails = SProxy :: SProxy "streamDetails"

data Action
  = Init
  | WindowTable (WindowTable.Message Stream)
  | SocketConnect
  | CloseDetails
  | PreventDefault MouseEvent

data Query a
  = NoOpQ a

type State
  = { windowStream :: Maybe Socket.RequestId
    , streamDetails :: Maybe Id
    }

type Stream
  = { id :: Id
    , client :: Addr
    , server :: Addr
    , tags :: Array Int
    , clientDataLen :: Number
    , serverDataLen :: Number
    }

component :: ∀ i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState :: i -> State
  initialState = const { windowStream: Nothing, streamDetails: Nothing }

  handleAction :: ∀ s. Action -> H.HalogenM State Action (Slot s) o Aff Unit
  handleAction = case _ of
    Init -> void $ Socket.subscribeConnect SocketConnect
    WindowTable msg -> case msg of
      WindowTable.ShowDetails stream -> H.modify_ $ _ { streamDetails = Just stream.id }
      WindowTable.CloseDetails -> H.modify_ $ _ { streamDetails = Nothing }
    SocketConnect -> do
      state <- H.get
      stream <- Socket.request { watch: { window: { index: "all", params: WindowTable.initialWindowParams } } }
      _ <- H.modify $ _ { windowStream = Just stream }
      pure unit
    CloseDetails -> H.modify_ $ _ { streamDetails = Nothing }
    PreventDefault event -> H.liftEffect $ stopPropagation $ toEvent event

  showCell :: ∀ a s. Show a => a -> H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff
  showCell = stringCell <<< show

  stringCell :: ∀ s. String -> H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff
  stringCell s = HH.td_ [ HH.text s ]

  renderRow :: ∀ s. Stream -> Array (H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff)
  renderRow stream =
    [ showCell stream.id
    , showCell stream.client
    , showCell stream.server
    , stringCell $ formatBytes stream.clientDataLen
    , stringCell $ formatBytes stream.serverDataLen
    , HH.td_ [ HH.slot _tags stream.id Tags.component stream.tags absurd ]
    ]

  rows :: Array RowDescription
  rows = [ { name: "Id", width: 2 }, { name: "Client", width: 3 }, { name: "Server", width: 3 }, { name: "Client Data", width: 1 }, { name: "Server Data", width: 1 }, { name: "Tags", width: 6 } ]

  render :: ∀ s. State -> H.ComponentHTML Action (Slot s) Aff
  render state =
    div_
      ( [ HH.slot _table unit (WindowTable.component _.id rows renderRow) state.windowStream (Just <<< WindowTable) ]
          <> maybe
              []
              ( \streamDetails ->
                  [ div [ classes [ S.ui, S.dimmer, S.modals, S.page, S.visible, S.active ], css "display: flex !important", onClick $ Just <<< const CloseDetails ]
                      [ div [ classes [ S.ui, S.active, S.modal, S.large, S.visible, S.transition ], onClick $ Just <<< PreventDefault ]
                          [ sdiv [ S.header ] [ text $ "Stream " <> show streamDetails ]
                          , sdiv [ S.scrolling, S.content ] [ HH.slot _streamDetails unit Stream.component streamDetails absurd ]
                          ]
                      ]
                  ]
              )
              state.streamDetails
      )
