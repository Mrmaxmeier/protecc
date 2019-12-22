module Dashboard where

import Prelude

import CSS as CSS
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (a, div)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import SemanticUI as S

data Query a 
  = NoOpQ

type Counters 
  = { packets :: Int
    , streams :: Int
    , reassemblyErrors :: Int
    , packetsUnhandled :: Int
    , packetsMalformed :: Int
    , packetsWithoutStream :: Int
    , packetsTcp :: Int
    , streamsCompleted :: Int
    , pcapBlocks :: Int
    , dbServices :: Int
    , dbStatServicePromotion :: Int
    }

type State
  = { counters :: Maybe Counters
    }

component :: ∀i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

initialState :: ∀i. i -> State
initialState = const { counters: Nothing }

render :: ∀a. State -> H.ComponentHTML a () Aff
render state 
  = div [ HC.style (CSS.paddingTop $ CSS.px 20.0) ] 
    [ div [classes [S.ui, S.three, S.statistics] ] $
        map (\f -> f state.counters) 
            [ renderStatistic "Streams" _.streams
            , renderStatistic "Packets" _.packets
            , renderStatistic "Reassembly Errors" _.reassemblyErrors
            ]
    
    ]

renderStatistic :: ∀a. String -> (Counters -> Int) -> Maybe Counters -> H.ComponentHTML a () Aff
renderStatistic label value counters 
  = div [classes [S.statistic]] 
    [ div [ classes [S.value] ] [maybe (HH.text "?") (\c -> HH.text $ show (value c)) counters ] -- TODO
    , div [ classes [S.label]] [HH.text label]
    ]