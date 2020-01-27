module Dashboard where

import Prelude
import CSS as CSS
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (div)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import Numeral as Numeral
import SemanticUI (sdiv)
import SemanticUI as S
import Socket as Socket
import SocketIO as SIO
import Util (Rec, Size, WMaybe, prettifyJson, rec, unrec, wmaybe)

data Query a
  = NoOpQ

data Action
  = SocketConnect
  | CounterMessage { counters :: Counters }
  | Init

type Counters
  = Rec
      ( packets_unhandled :: WMaybe Size
      , packets_malformed :: WMaybe Size
      , packets_without_stream :: WMaybe Size
      , packets_tcp :: WMaybe Size
      , packet_bytes :: WMaybe Size
      , pcap_processing_milliseconds :: WMaybe Size
      , streams_completed :: WMaybe Size
      , streams_timeout_expired :: WMaybe Size
      , streams_without_syn :: WMaybe Size
      , streams_processed_out_of_order :: WMaybe Size
      , pcap_blocks :: WMaybe Size
      , pcaps_imported :: WMaybe Size
      , db_services :: WMaybe Size
      , db_streams_rss :: WMaybe Size
      , query_rows_scanned :: WMaybe Size
      , query_rows_returned :: WMaybe Size
      , window_sent :: WMaybe Size
      , ws_connections :: WMaybe Size
      , ws_rx :: WMaybe Size
      , ws_tx :: WMaybe Size
      )

type State
  = { counters :: Counters
    }

component :: ∀ i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
    }
  where
  handleAction :: ∀ s. Action -> H.HalogenM State Action s o Aff Unit
  handleAction = case _ of
    Init -> do
      _ <- Socket.subscribeConnect SocketConnect
      pure unit
    SocketConnect -> do
      streamId <- Socket.request { watch: "counters" }
      _ <- Socket.subscribeResponses CounterMessage streamId
      pure unit
    CounterMessage counters -> do
      state <- H.get
      H.put $ state { counters = counters.counters <> state.counters }

  initialState :: i -> State
  initialState = const ({ counters: mempty })

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.div_
      [ sdiv [ S.ui, S.three, S.statistics ]
          [ renderStatistic "Streams" c.streams_completed Numeral.formatSize
          , renderStatistic "Packets" c.packets_tcp Numeral.formatSize
          , renderStatistic "Data" c.packet_bytes Numeral.formatBytes
          ]
      , HH.pre_ [ HH.text $ prettifyJson $ stringify $ encodeJson (unrec state.counters) ]
      ]
    where
    c = unrec state.counters

  renderStatistic label value show =
    sdiv [ S.statistic ]
      [ HK.div [ classes [ S.value ] ] [ Tuple "val" (HH.text $ wmaybe "0" show value) ]
      , sdiv [ S.label ] [ HH.text label ]
      ]
