module Dropdown where

import Prelude
import CSS as CSS
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (lookup)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (a, div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import SemanticUI (sa, sdiv)
import SemanticUI as S
import Util (logo)

type State h d
  = { selection :: h, rows :: Array d }

type Input h d
  = State h d

data Action h d
  = InputUpdate (Input h d)
  | Clicked d

data Message h d
  = Selected d

type Config h d
  = { renderHeader :: h -> H.ComponentHTML (Action h d) () Aff
    , renderInList :: d -> H.ComponentHTML (Action h d) () Aff
    }

component :: ∀ q h d. Config h d -> H.Component HH.HTML q (Input h d) (Message h d) Aff
component config =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< InputUpdate }
    }
  where
  initialState :: Input h d -> State h d
  initialState = identity

  render :: State h d -> H.ComponentHTML (Action h d) () Aff
  render state =
    sdiv [ S.ui, S.compact, S.menu ]
      [ sdiv [ S.ui, S.simple, S.dropdown, S.item ]
          [ config.renderHeader state.selection
          , HH.i [ classes [ S.dropdown, S.icon ] ] []
          , sdiv [ S.menu, S.scrolling ]
              $ map (\row -> div [ classes [ S.item ], onClick $ Just <<< (const $ Clicked row) ] [ config.renderInList row ])
                  state.rows
          ]
      ]

  handleAction :: ∀ s. Action h d -> H.HalogenM (State h d) (Action h d) s (Message h d) Aff Unit
  handleAction = case _ of
    InputUpdate input -> void $ H.put input
    Clicked d -> H.raise $ Selected d

type Color
  = { name :: String, value :: String }

type ColorMessage
  = Message Color Color

grey :: Color
grey = { name: "Grey", value: "grey" }

green :: Color
green = { name: "Green", value: "green" }

red :: Color
red = { name: "Red", value: "red" }

orange :: Color
orange = { name: "Orange", value: "orange" }

yellow :: Color
yellow = { name: "Yellow", value: "yellow" }

olive :: Color
olive = { name: "Olive", value: "olive" }

teal :: Color
teal = { name: "Teal", value: "teal" }

violet :: Color
violet = { name: "Violet", value: "violet" }

purple :: Color
purple = { name: "Purple", value: "purple" }

pink :: Color
pink = { name: "Pink", value: "pink" }

brown :: Color
brown = { name: "Brown", value: "brown" }

black :: Color
black = { name: "Black", value: "black" }

colors :: NonEmptyArray Color
colors =
  cons' grey
    [ green
    , red
    , orange
    , yellow
    , olive
    , teal
    , violet
    , purple
    , pink
    , brown
    , black
    ]

valueToColor :: String -> Color
valueToColor = case _ of
  "green" -> green
  "red" -> red
  "orange" -> orange
  "yellow" -> yellow
  "olive" -> olive
  "teal" -> teal
  "violet" -> violet
  "purple" -> purple
  "pink" -> pink
  "brown" -> brown
  "black" -> black
  _ -> grey

colorDropdown :: H.Component HH.HTML Identity (Input Color Color) (Message Color Color) Aff
colorDropdown = component { renderHeader: renderLabel, renderInList: renderLabel }
  where
  renderLabel color = HH.span_ [ div [ classes [ S.ui, S.empty, S.circular, S.label, ClassName color.value ], HC.style $ CSS.marginRight $ CSS.px 5.0 ] [], text color.name ]
