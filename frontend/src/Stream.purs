module Stream where

import Prelude
import CSS as CSS
import CSS.TextAlign as CT
import Configuration as Config
import ConfigurationTypes (Configuration)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Int.Bits (and, shl, (.&.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith, toCodePointArray)
import Data.String.Base64 (decode, encodeUrl)
import Data.String.CodeUnits (length)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Dataview (DisplayType(..), displayTypeFromString, displayTypeToString)
import Dataview as Dataview
import Effect.Aff (Aff)
import Foreign.Object (Object, lookup)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (a, div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (InputType(..), classes, download, href, selected, type_, value)
import Halogen.HTML.Properties as HP
import Numeral (formatBytes)
import SemanticUI (loaderDiv, sdiv)
import SemanticUI as S
import Socket (RequestId)
import Socket as Socket
import Tags as Tags
import Util (Id, mwhen, prettyShow)

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

derive instance senderEq :: Eq Sender

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

type StreamDetails
  = { id :: Id
    , client :: Addr
    , server :: Addr
    , tags :: Array Id
    , features :: Object Number
    , segments :: Array SegmentWithData
    , clientDataLen :: Number
    , serverDataLen :: Number
    }

type SegmentWithData
  = { sender :: Sender
    , data :: String
    , seq :: Number
    , ack :: Number
    , timestamp :: Number
    , flags :: Int
    }

flag :: Int -> Int -> Boolean
flag a b = (shl 1 a) .&. b /= 0

fin :: Int -> Boolean
fin = flag 0

syn :: Int -> Boolean
syn = flag 1

rst :: Int -> Boolean
rst = flag 2

psh :: Int -> Boolean
psh = flag 3

ack :: Int -> Boolean
ack = flag 4

urg :: Int -> Boolean
urg = flag 5

ece :: Int -> Boolean
ece = flag 6

cwr :: Int -> Boolean
cwr = flag 7

type Slot a
  = ( tags :: H.Slot a Void Unit )

_tags = SProxy :: SProxy "tags"

type Input
  = Id

type State
  = { id :: Input
    , stream :: Maybe StreamDetails
    , requestId :: Maybe RequestId
    , collapse :: Boolean
    , displayType :: DisplayType
    }

data Action
  = StreamUpdate { streamDetails :: StreamDetails }
  | InputChanged Input
  | Init
  | SocketConnect
  | ToggleCollapse
  | DisplayTypeChange String

component :: ∀ q o. H.Component HH.HTML q Input o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction, receive = Just <<< InputChanged }
    }
  where
  initialState :: Input -> State
  initialState input = { id: input, stream: Nothing, requestId: Nothing, collapse: true, displayType: Auto }

  dataFiltered :: (SegmentWithData -> Boolean) -> StreamDetails -> String
  dataFiltered f stream =
    encodeUrl
      $ joinWith ""
      $ map
          ( \s -> case decode s.data of
              Left _ -> ""
              Right s -> s
          )
      $ filter f
      $ stream.segments

  render :: ∀ a. State -> H.ComponentHTML Action (Slot a) Aff
  render state = maybe (loaderDiv false) renderDetails state.stream
    where
    renderDetails stream =
      HH.span_
        [ HH.table [ classes [ S.ui, S.table, S.basic, S.celled ] ]
            [ HH.thead_ [ HH.tr_ [ HH.th_ [ text "Client" ], HH.th_ [ text "Server" ], HH.th_ [ text "Stream Data" ], HH.th_ [ text "Client Data" ], HH.th_ [ text "Server Data" ] ] ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td_ [ text $ show $ stream.client ]
                    , HH.td_ [ text $ show $ stream.server ]
                    , HH.td_ [ HH.a [ download $ show stream.id <> ".bin", href $ "data:text/plain;base64," <> dataFiltered (const true) stream ] [ text "Download" ], text $ "(" <> (formatBytes $ stream.clientDataLen + stream.serverDataLen) <> ")" ]
                    , HH.td_ [ HH.a [ download $ show stream.id <> "-client.bin", href $ "data:text/plain;base64," <> dataFiltered (\s -> s.sender == Client) stream ] [ text "Download" ], text $ "(" <> (formatBytes stream.clientDataLen) <> ")" ]
                    , HH.td_ [ HH.a [ download $ show stream.id <> "-server.bin", href $ "data:text/plain;base64," <> dataFiltered (\s -> s.sender == Server) stream ] [ text "Download" ], text $ "(" <> (formatBytes stream.serverDataLen) <> ")" ]
                    ]
                ]
            ]
        , sdiv [ S.ui, S.divider ] []
        , HH.td_ [ HH.td_ [ HH.slot _tags unit (Tags.component true) { tags: stream.tags, stream: stream.id } absurd ] ]
        , sdiv [ S.ui, S.divider ] []
        , div [ HC.style $ CSS.paddingBottom $ CSS.px 10.0 ]
            [ HH.select [ classes [ S.ui, S.selection, S.dropdown ], onValueChange $ Just <<< DisplayTypeChange ]
                [ HH.option [ selected $ state.displayType == Auto, value $ displayTypeToString Auto ] [ text $ "Auto" ]
                , HH.option [ selected $ state.displayType == Ascii, value $ displayTypeToString Ascii ] [ text $ "Ascii" ]
                , HH.option [ selected $ state.displayType == Hexdump, value $ displayTypeToString Hexdump ] [ text $ "Hexdump" ]
                , HH.option [ selected $ state.displayType == B64, value $ displayTypeToString B64 ] [ text $ "Base64" ]
                ]
            , HH.button
                [ classes $ [ S.ui, S.basic, S.button ] <> if state.collapse then [ S.yellow ] else [ S.green ], HC.style $ CSS.marginLeft $ CSS.px 10.0, onClick $ Just <<< (const ToggleCollapse) ]
                [ text if state.collapse then "Uncollapse Empty" else "Collapse Empty" ]
            ]
        , div_
            $ map
                ( \segment ->
                    div
                      [ classes [ S.ui, S.message ]
                      , HC.style
                          $ ( case segment.sender of
                                Client -> CSS.marginRight
                                Server -> CSS.marginLeft
                            )
                          $ CSS.pct 20.0
                      ]
                      [ HH.pre
                          [ HC.style
                              ( case segment.sender of
                                  Client -> CT.textAlign CT.leftTextAlign
                                  Server -> CT.textAlign CT.rightTextAlign
                              )
                          ]
                          [ div [ HC.style $ CSS.paddingBottom $ CSS.px 10.0 ]
                              $ [ sdiv [ S.ui, S.label, S.grey ] [ text "seq", sdiv [ S.detail ] [ text $ prettyShow segment.seq ] ] ]
                              <> mwhen (syn segment.flags) [ sdiv [ S.ui, S.label, S.green ] [ text "syn" ] ]
                              <> mwhen (ack segment.flags) [ sdiv [ S.ui, S.label, S.blue ] [ text "ack", sdiv [ S.detail ] [ text $ prettyShow segment.ack ] ] ]
                              <> mwhen (psh segment.flags) [ sdiv [ S.ui, S.label, S.purple ] [ text "psh" ] ]
                              <> mwhen (fin segment.flags) [ sdiv [ S.ui, S.label, S.yellow ] [ text "fin" ] ]
                              <> mwhen (rst segment.flags) [ sdiv [ S.ui, S.label, S.red ] [ text "rst" ] ]
                          , Dataview.render state.displayType segment.data
                          ]
                      ]
                )
            $ filter (\s -> not state.collapse || length s.data > 0) stream.segments
        ]

  handleAction :: ∀ a. Action -> H.HalogenM State Action (Slot a) o Aff Unit
  handleAction = case _ of
    Init -> void $ Socket.subscribeConnect SocketConnect
    SocketConnect -> do
      state <- H.get
      cancelDetails
      subscribeDetails state.id
    StreamUpdate stream -> do
      H.modify_ $ _ { stream = Just stream.streamDetails }
    InputChanged input -> do
      cancelDetails
      subscribeDetails input
    ToggleCollapse -> H.modify_ $ \state -> state { collapse = not state.collapse }
    DisplayTypeChange dt -> H.modify_ $ _ { displayType = displayTypeFromString dt }

  cancelDetails :: ∀ a. H.HalogenM State Action (Slot a) o Aff Unit
  cancelDetails = do
    state <- H.get
    maybe (pure unit) Socket.cancel state.requestId
    H.modify_ $ _ { stream = Nothing, requestId = Nothing }

  subscribeDetails :: ∀ a. Id -> H.HalogenM State Action (Slot a) o Aff Unit
  subscribeDetails id = do
    requestId <- Socket.request { watch: { streamDetails: id } }
    void $ Socket.subscribeResponses StreamUpdate requestId
