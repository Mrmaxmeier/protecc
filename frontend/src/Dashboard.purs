module Dashboard where

import Prelude
import CSS as CSS
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (div)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import SemanticUI as S
import Socket as Socket
import SocketIO as SIO
import Util (WMaybe, wmaybe, rec, unrec, Rec, prettifyJson)

data Query a
  = NoOpQ

data Action
  = SocketConnect
  | CounterMessage { counters :: Counters }
  | Init

type Counters
  = Rec
      ( packets_unhandled :: WMaybe Int
      , packets_malformed :: WMaybe Int
      , packets_without_stream :: WMaybe Int
      , packets_tcp :: WMaybe Int
      , packet_bytes :: WMaybe Int
      , pcap_processing_milliseconds :: WMaybe Int
      , streams_completed :: WMaybe Int
      , streams_timeout_expired :: WMaybe Int
      , pcap_blocks :: WMaybe Int
      , pcaps_imported :: WMaybe Int
      , db_services :: WMaybe Int
      , db_stat_service_promotion :: WMaybe Int
      , query_rows_scanned :: WMaybe Int
      , query_rows_returned :: WMaybe Int
      , ws_connections :: WMaybe Int
      , ws_rx :: WMaybe Int
      , ws_tx :: WMaybe Int
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
      [ div [ classes [ S.ui, S.three, S.statistics ] ]
          [ renderStatistic "Streams" c.streams_completed
          , renderStatistic "Packets" c.packets_tcp
          , renderStatistic "Packets without Stream" c.packets_without_stream
          ]
      , HH.pre_ [ HH.text $ prettifyJson $ stringify $ encodeJson (unrec state.counters) ]
      ]
    where
    c = unrec state.counters

  renderStatistic label value =
    div [ classes [ S.statistic ] ]
      [ div [ classes [ S.value ] ] [ HH.text $ wmaybe "?" show value ]
      , div [ classes [ S.label ] ] [ HH.text label ]
      ]
