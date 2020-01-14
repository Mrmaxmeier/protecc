module Streams where

import Prelude
import CSS as CSS
import Configuration as Config
import ConfigurationTypes (Configuration, Tag)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (filter, head, (:))
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Dropdown (Message(..), ServiceMessage, TagMessage)
import Dropdown as Dropdown
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (Object, lookup, values)
import Halogen as H
import Halogen.HTML (div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, href)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import Keyevent as Keyevent
import Numeral (formatBytes)
import Routing.Hash (setHash)
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
import Web.HTML (window) as DOM
import Web.HTML.Location (setHref)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import WindowTable (Message, RowDescription)
import WindowTable as WindowTable

type Slot a
  = ( table :: H.Slot WindowTable.Query (WindowTable.Message Stream) Unit
    , tagDropdown :: H.Slot Identity TagMessage Unit
    , serviceDropdown :: H.Slot Identity ServiceMessage Unit
    , tags :: H.Slot a Void Id
    , streamDetails :: H.Slot a Void Unit
    )

_table = SProxy :: SProxy "table"

_tags = SProxy :: SProxy "tags"

_streamDetails = SProxy :: SProxy "streamDetails"

_tagDropdown = SProxy :: SProxy "tagDropdown"

_serviceDropdown = SProxy :: SProxy "serviceDropdown"

data Action
  = Init
  | WindowTable (WindowTable.Message Stream)
  | SocketConnect
  | CloseDetails
  | PreventDefault MouseEvent
  | ConfigUpdate Configuration
  | TagUpdate TagMessage
  | ServiceUpdate ServiceMessage
  | InputChanged Input

data Query a
  = NoOpQ a

type Input
  = Index

type State
  = { windowStream :: Maybe Socket.RequestId
    , streamDetails :: Maybe Id
    , config :: Maybe Configuration
    , index :: Index
    }

type Stream
  = { id :: Id
    , client :: Addr
    , server :: Addr
    , tags :: Array Id
    , clientDataLen :: Number
    , serverDataLen :: Number
    }

type Index
  = { tag :: Maybe Id, port :: Maybe Int }

component :: ∀ o. H.Component HH.HTML Query Input o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init, receive = Just <<< InputChanged }
    }
  where
  initialState :: Input -> State
  initialState index = { windowStream: Nothing, streamDetails: Nothing, config: Nothing, index: index }

  handleAction :: ∀ s. Action -> H.HalogenM State Action (Slot s) o Aff Unit
  handleAction = case _ of
    Init -> do
      _ <- Keyevent.subscribe 27 $ WindowTable WindowTable.CloseDetails
      _ <- Socket.subscribeConnect SocketConnect
      void $ Config.subscribe ConfigUpdate
    WindowTable msg -> case msg of
      WindowTable.ShowDetails stream -> H.modify_ $ _ { streamDetails = Just stream.id }
      WindowTable.CloseDetails -> H.modify_ $ _ { streamDetails = Nothing }
    SocketConnect -> sendRequest
    CloseDetails -> H.modify_ $ _ { streamDetails = Nothing }
    PreventDefault event -> H.liftEffect $ stopPropagation $ toEvent event
    ConfigUpdate config -> H.modify_ $ _ { config = Just config }
    TagUpdate msg -> case msg of
      Selected tag -> do
        state <- H.get
        navigateTo (map _.id tag) state.index.port
    ServiceUpdate msg -> case msg of
      Selected service -> do
        state <- H.get
        navigateTo state.index.tag (map _.port service)
    InputChanged input -> do
      state <- H.get
      when (state.index /= input) do
        state <- H.modify $ _ { index = input }
        maybe (pure unit) Socket.cancel state.windowStream
        sendRequest

  sendRequest :: ∀ s. H.HalogenM State Action (Slot s) o Aff Unit
  sendRequest = do
    state <- H.get
    stream <- case state.index of
      { tag: Nothing, port: Nothing } -> Socket.request { watch: { window: { index: "all", params: WindowTable.initialWindowParams } } }
      { tag: Just tag, port: Nothing } -> Socket.request { watch: { window: { index: { tagged: tag }, params: WindowTable.initialWindowParams } } }
      { tag: Nothing, port: Just port } -> Socket.request { watch: { window: { index: { service: port }, params: WindowTable.initialWindowParams } } }
      { tag: Just tag, port: Just port } -> Socket.request { watch: { window: { index: { serviceTagged: Tuple port tag }, params: WindowTable.initialWindowParams } } }
    H.modify_ $ _ { windowStream = Just stream }

  navigateTo :: ∀ s. Maybe Id -> Maybe Int -> H.HalogenM State Action (Slot s) o Aff Unit
  navigateTo tag stream = H.liftEffect $ setHash loc
    where
    loc = case Tuple tag stream of
      Tuple Nothing Nothing -> "streams"
      Tuple (Just tag) Nothing -> "streams/tag/" <> show tag
      Tuple Nothing (Just service) -> "streams/service/" <> show service
      Tuple (Just tag) (Just service) -> "streams/" <> show service <> "/" <> show tag

  showCell :: ∀ a s. Show a => a -> H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff
  showCell = stringCell <<< show

  stringCell :: ∀ s. String -> H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff
  stringCell s = HH.td_ [ HH.text s ]

  renderRow :: ∀ s. Stream -> Array (H.ComponentHTML (WindowTable.Action Stream) (Slot s) Aff)
  renderRow stream =
    [ HH.td_ [ HH.a [ href $ "#stream/" <> show stream.id ] [ text $ show stream.id ] ]
    , showCell stream.client
    , showCell stream.server
    , stringCell $ formatBytes stream.clientDataLen
    , stringCell $ formatBytes stream.serverDataLen
    , HH.td_ [ HH.slot _tags stream.id (Tags.component false) { tags: stream.tags, stream: stream.id } absurd ]
    ]

  rows :: Array RowDescription
  rows = [ { name: "Id", width: 2 }, { name: "Client", width: 3 }, { name: "Server", width: 3 }, { name: "Client Data", width: 1 }, { name: "Server Data", width: 1 }, { name: "Tags", width: 6 } ]

  render :: ∀ s. State -> H.ComponentHTML Action (Slot s) Aff
  render state =
    div_
      $ [ sdiv [ S.ui, S.container ]
            [ sdiv [ S.basic, S.segment, S.ui, S.form ]
                [ sdiv [ S.fields ]
                    [ sdiv [ S.field ]
                        [ HH.label_ [ text "Tag" ]
                        , HH.slot _tagDropdown unit Dropdown.tagDropdown
                            { selection: (\conf -> head $ filter (\tag -> Just tag.id == state.index.tag) conf.tags) =<< state.config
                            , rows: Nothing : maybe [] (\conf -> map Just $ conf.tags) state.config
                            }
                            (Just <<< TagUpdate)
                        ]
                    , sdiv [ S.field ]
                        [ HH.label_ [ text "Service" ]
                        , HH.slot _serviceDropdown unit Dropdown.serviceDropdown
                            { selection: (\conf -> head $ filter (\service -> Just service.port == state.index.port) conf.services) =<< state.config
                            , rows: Nothing : maybe [] (\conf -> map Just $ conf.services) state.config
                            }
                            (Just <<< ServiceUpdate)
                        ]
                    ]
                ]
            ]
        , HH.slot _table unit (WindowTable.component _.id rows renderRow) state.windowStream (Just <<< WindowTable)
        ]
      <> maybe
          []
          ( \streamDetails ->
              [ div [ classes [ S.ui, S.dimmer, S.modals, S.page, S.visible, S.active ], css "display: flex !important", onClick $ Just <<< const CloseDetails ]
                  [ div [ classes [ S.ui, S.active, S.modal, S.large, S.visible, S.transition ], onClick $ Just <<< PreventDefault ]
                      [ sdiv [ S.header ] [ text $ "Stream ", HH.a [ href $ "#stream/" <> show streamDetails ] [ text $ show streamDetails ] ]
                      , sdiv [ S.scrolling, S.content ] [ HH.slot _streamDetails unit Stream.component streamDetails absurd ]
                      ]
                  ]
              ]
          )
          state.streamDetails
