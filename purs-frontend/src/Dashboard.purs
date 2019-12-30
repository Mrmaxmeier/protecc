module Dashboard where

import Prelude
import CSS as CSS
import Data.Maybe (Maybe(..), maybe)
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

data Query a
  = NoOpQ

data Action
  = SocketConnect
  | CounterMessage Counters
  | Init

type Counters
  = { packets :: Int
    , streams :: Int
    , reassembly_errors :: Int
    , packets_unhandled :: Int
    , packets_malformed :: Int
    , packets_without_stream :: Int
    , packets_tcp :: Int
    , streams_completed :: Int
    , streams_timeout_expired :: Int
    , pcap_blocks :: Int
    , pcaps_imported :: Int
    , db_services :: Int
    , db_stat_service_promotion :: Int
    , query_rows_scanned :: Int
    , query_rows_returned :: Int
    }

type State
  = { counters :: Maybe Counters
    }

component :: ∀ i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
    }
  where
  handleAction = case _ of
    Init -> void $ mapAction (const SocketConnect) $ Socket.subscribe SIO.connectSource
    SocketConnect -> do
      streamId <- Socket.open "counters" unit
      _ <- mapAction CounterMessage $ H.subscribe $ Socket.messageSource streamId
      pure unit
    CounterMessage counters -> H.modify_ $ _ { counters = Just counters }

  initialState = const { counters: Nothing }

  render state =
    div [ HC.style (CSS.paddingTop $ CSS.px 20.0) ]
      [ div [ classes [ S.ui, S.three, S.statistics ] ]
          $ map (\f -> f state.counters)
              [ renderStatistic "Streams" _.streams
              , renderStatistic "Packets" _.packets
              , renderStatistic "Reassembly Errors" _.reassembly_errors
              ]
      ]

renderStatistic :: ∀ a. String -> (Counters -> Int) -> Maybe Counters -> H.ComponentHTML a () Aff
renderStatistic label value counters =
  div [ classes [ S.statistic ] ]
    [ div [ classes [ S.value ] ] [ maybe (HH.text "?") (\c -> HH.text $ show (value c)) counters ]
    , div [ classes [ S.label ] ] [ HH.text label ]
    ]
