module Dashboard where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
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
import Util (WMaybe, wmaybe, rec, unrec, Rec)

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
      , streams_completed :: WMaybe Int
      , streams_timeout_expired :: WMaybe Int
      , pcap_blocks :: WMaybe Int
      , pcaps_imported :: WMaybe Int
      , db_services :: WMaybe Int
      , db_stat_service_promotion :: WMaybe Int
      , query_rows_scanned :: WMaybe Int
      , query_rows_returned :: WMaybe Int
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
    Init -> void $ mapAction (const SocketConnect) $ Socket.subscribe SIO.connectSource
    SocketConnect -> do
      streamId <- Socket.open "counters" unit
      _ <- mapAction CounterMessage $ H.subscribe $ Socket.messageSource streamId
      pure unit
    CounterMessage counters -> do
      state <- H.get
      H.put $ state { counters = counters.counters <> state.counters }

  initialState :: i -> State
  initialState = const ({ counters: mempty })

  render state =
    div [ HC.style (CSS.paddingTop $ CSS.px 20.0) ]
      [ div [ classes [ S.ui, S.three, S.statistics ] ]
          [ renderStatistic "Streams" (unrec state.counters).streams_completed
          , renderStatistic "Packets" (unrec state.counters).packets_tcp
          , renderStatistic "Packets without Stream" (unrec state.counters).packets_without_stream
          ]
      ]

  renderStatistic label value =
    div [ classes [ S.statistic ] ]
      [ div [ classes [ S.value ] ] [ HH.text $ wmaybe "?" show value ]
      , div [ classes [ S.label ] ] [ HH.text label ]
      ]
