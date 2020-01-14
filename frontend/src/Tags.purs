module Tags where

import Prelude
import CSS as CSS
import Configuration as Config
import ConfigurationTypes (Configuration, Tag)
import Data.Array (all, filter, head)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Dropdown (Message(..))
import Dropdown as Dropdown
import Effect.Aff (Aff)
import Foreign.Object (lookup, toUnfoldable, values)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (a, div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import SemanticUI (sa, sdiv, sicon)
import SemanticUI as S
import Util (Id, fromString, logo, mwhen)
import Socket as Socket

type Slot
  = ( dropdown :: H.Slot Identity Dropdown.TagMessage Unit
    )

_dropdown = SProxy :: SProxy "dropdown"

type Input
  = { tags :: Array Id, stream :: Id }

type State
  = { input :: Input, config :: Maybe Configuration, selectedId :: Maybe Id }

data Action
  = AddTag
  | RemoveTag Id
  | ConfigUpdate Configuration
  | TagSelect Dropdown.TagMessage
  | Init
  | InputUpdate Input

component :: ∀ q o. Boolean -> H.Component HH.HTML q Input o Aff
component editable =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction, receive = Just <<< InputUpdate }
    }
  where
  initialState :: Input -> State
  initialState input = { input: input, config: Nothing, selectedId: Nothing }

  render :: State -> H.ComponentHTML Action Slot Aff
  render state =
    div_
      $ [ HK.div_
            $ map
                ( \id ->
                    let
                      tag = fromMaybe { name: "Tag not Found!", color: "red", slug: show $ id, owner: "nobody", id: fromString "0" } $ (\c -> head $ filter (\tag -> tag.id == id) c.tags) =<< state.config
                    in
                      Tuple tag.slug $ sdiv ([ S.ui, S.label ] <> [ ClassName tag.color ]) $ [ text tag.name ] <> mwhen editable [ HH.i [ classes [ S.delete, S.icon ], onClick $ Just <<< (const $ RemoveTag tag.id) ] [] ]
                )
                state.input.tags
        ]
      <> mwhen editable
          [ div [ HC.style $ CSS.marginTop $ CSS.pt 4.0 ]
              [ HH.slot _dropdown unit Dropdown.tagDropdown { selection: selectedTag, rows: map Just unselectedTags } $ Just <<< TagSelect
              , HH.button [ classes [ S.ui, S.green, S.basic, S.button ], onClick $ Just <<< (const AddTag), HC.style $ CSS.marginLeft $ CSS.pt 3.0 ] [ text "Add" ]
              ]
          ]
    where
    unselectedTags = maybe [] (\conf -> filter (\tag -> all (\id -> id /= tag.id) state.input.tags) conf.tags) state.config

    selectedTag = (\id -> head $ filter (\tag -> tag.id == id) unselectedTags) =<< state.selectedId

  handleAction :: ∀ s. Action -> H.HalogenM State Action s o Aff Unit
  handleAction = case _ of
    Init -> void $ Config.subscribe ConfigUpdate
    ConfigUpdate config -> do
      H.modify_ $ _ { config = Just config }
    TagSelect msg -> case msg of
      Selected tag -> H.modify_ $ _ { selectedId = map _.id tag }
    RemoveTag tag -> do
      state <- H.get
      void $ Socket.request { removeTag: Tuple state.input.stream tag }
    AddTag -> do
      state <- H.get
      logo state.selectedId
      case state.selectedId of
        Nothing -> pure unit
        Just id -> void $ Socket.request { addTag: Tuple state.input.stream id }
    InputUpdate input -> H.modify_ $ _ { input = input }
