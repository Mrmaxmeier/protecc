module Configuration (init, set, subscribe, component, Configuration, Tag, Service) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, cons', head, singleton, toArray)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Dropdown as Dropdown
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (Object, toUnfoldable)
import Halogen (ClassName(..), HalogenM)
import Halogen as H
import Halogen.HTML (text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onChange, onClick, onKeyDown, onSubmit, onValueChange)
import Halogen.HTML.Properties (InputType(..), classes, disabled, type_, value)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Halogen.Query.HalogenM (SubscriptionId, mapAction)
import SemanticUI (loaderDiv, sa, sdiv)
import SemanticUI as S
import Socket (RequestId)
import Socket as Socket
import Util (logo, logs, mwhen, onEnter)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code)

type Tag
  = { slug :: String, name :: String, color :: String, owner :: String }

type Service
  = {}

type Configuration
  = { tags :: Object Tag, services :: Object Service }

foreign import listen :: (Configuration -> Effect Unit) -> Effect Unit

foreign import unlisten :: (Configuration -> Effect Unit) -> Effect Unit

foreign import setImpl :: Configuration -> Effect Unit

foreign import getImpl :: (Configuration -> Maybe Configuration) -> Maybe Configuration -> Effect (Maybe Configuration)

init :: ∀ state a slot m. HalogenM state a slot m Aff RequestId
init = Socket.request { watch: "configuration" }

set :: ∀ state a slot m. Configuration -> HalogenM state a slot m Aff Unit
set = H.liftEffect <<< setImpl

source :: EventSource Aff Configuration
source =
  effectEventSource \emitter -> do
    config <- getImpl Just Nothing
    let
      listener = emit emitter
    maybe (pure unit) (emit emitter) config
    listen listener
    pure $ Finalizer $ unlisten listener

subscribe :: ∀ state a slot m. (Configuration -> a) -> HalogenM state a slot m Aff SubscriptionId
subscribe f = mapAction f $ H.subscribe source

type State
  = { config :: Maybe Configuration, addTag :: EditTag, editTag :: Maybe EditTag }

data Action
  = Init
  | ConfigUpdate Configuration
  | AddTagUpdate EditTag
  | SubmitAddTag
  | EditTagUpdate EditTag
  | SubmitEditTag
  | EditTagColorChange Dropdown.ColorMessage
  | AddTagColorChange Dropdown.ColorMessage
  | Test KeyboardEvent

type Slot
  = ( addColor :: H.Slot Identity Dropdown.ColorMessage Unit
    , editColor :: H.Slot Identity Dropdown.ColorMessage String
    )

_addColor = SProxy :: SProxy "addColor"

_editColor = SProxy :: SProxy "editColor"

type EditTag
  = { name :: String
    , slug :: String
    , color :: Dropdown.Color
    , owner :: String
    }

editTagToTag :: EditTag -> Tag
editTagToTag et = { name: et.name, slug: et.slug, color: et.color.value, owner: et.owner }

component :: ∀ q o i. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState input = { config: Nothing, addTag: { slug: "", name: "", color: head Dropdown.colors, owner: "webui" }, editTag: Nothing }

  render :: State -> H.ComponentHTML Action Slot Aff
  render state = sdiv [ S.ui, S.container ] content
    where
    content = case state.config of
      Nothing -> [ loaderDiv false ]
      Just config ->
        [ HH.h1_ [ text "Tags" ]
        , HH.table [ classes [ S.center, S.ui, S.basic, S.compact, S.collapsing, S.celled, S.table ] ]
            [ HH.thead_ [ HH.tr_ [ HH.th_ [ text "Slug" ], HH.th_ [ text "Name" ], HH.th_ [ text "Owner" ], HH.th_ [ text "Color" ], HH.th_ [] ] ]
            , HK.tbody_
                $ ( map
                      ( \(Tuple id tag) ->
                          Tuple id
                            $ case state.editTag of
                                Just editTag
                                  | editTag.slug == tag.slug ->
                                    HH.tr_
                                      [ HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitEditTag) ] [ sdiv [ S.disabled, S.field ] [ HH.input [ type_ InputText, disabled true, value editTag.slug ] ] ] ]
                                      , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitEditTag) ] [ sdiv [ S.field ] [ HH.input [ type_ InputText, onValueChange $ Just <<< EditTagUpdate <<< (editTag { name = _ }), value editTag.name ] ] ] ]
                                      , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitEditTag) ] [ sdiv [ S.disabled, S.field ] [ HH.input [ type_ InputText, disabled true, value editTag.owner ] ] ] ]
                                      , HH.slot _editColor editTag.slug Dropdown.colorDropdown { selection: editTag.color, rows: toArray Dropdown.colors } $ Just <<< EditTagColorChange
                                      , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitEditTag) ] [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const SubmitEditTag) ] [ text $ "Submit" ] ] ]
                                      ]
                                _ ->
                                  HH.tr_
                                    [ HH.td_ [ text $ tag.slug ]
                                    , HH.td_ [ text $ tag.name ]
                                    , HH.td_ [ text $ tag.owner ]
                                    , HH.td_ [ sdiv [ S.ui, S.label, ClassName tag.color ] [ text $ (Dropdown.valueToColor tag.color).name ] ]
                                    , HH.td_ [ HH.button [ classes [ S.ui, S.mini, S.button ], onClick $ Just <<< (const $ EditTagUpdate { name: tag.name, slug: tag.slug, color: Dropdown.valueToColor tag.color, owner: tag.owner }) ] [ text $ "Edit" ] ]
                                    ]
                      )
                      $ toUnfoldable config.tags
                  )
                <> [ Tuple "add"
                      $ HH.tr_
                          [ HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitAddTag) ] [ sdiv [ S.field ] [ HH.input [ type_ InputText, onValueChange $ Just <<< AddTagUpdate <<< (state.addTag { slug = _ }), value state.addTag.slug ] ] ] ]
                          , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitAddTag) ] [ sdiv [ S.field ] [ HH.input [ type_ InputText, onValueChange $ Just <<< AddTagUpdate <<< (state.addTag { name = _ }), value state.addTag.name ] ] ] ]
                          , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitAddTag) ] [ sdiv [ S.disabled, S.field ] [ HH.input [ type_ InputText, disabled true, value state.addTag.owner ] ] ] ]
                          , HH.slot _addColor unit Dropdown.colorDropdown { selection: state.addTag.color, rows: toArray Dropdown.colors } $ Just <<< AddTagColorChange
                          , HH.td_ [ HH.form [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitAddTag) ] [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const SubmitAddTag) ] [ text $ "Add" ] ] ]
                          ]
                  ]
            ]
        ]

  handleAction :: Action -> H.HalogenM State Action Slot o Aff Unit
  handleAction = case _ of
    Init -> void $ subscribe ConfigUpdate
    ConfigUpdate config -> do
      H.modify_ $ _ { config = Just config }
    AddTagUpdate add -> H.modify_ $ _ { addTag = add }
    SubmitAddTag -> do
      state <- H.get
      void $ Socket.request { updateConfiguration: { setTag: editTagToTag state.addTag } }
    EditTagUpdate tag -> H.modify_ $ _ { editTag = Just tag }
    SubmitEditTag -> do
      state <- H.get
      _ <- Socket.request { updateConfiguration: { setTag: map editTagToTag state.editTag } }
      H.modify_ $ _ { editTag = Nothing }
    EditTagColorChange msg -> case msg of
      Dropdown.Selected c -> H.modify_ $ \state -> state { editTag = map (_ { color = c }) state.editTag }
    AddTagColorChange msg -> case msg of
      Dropdown.Selected c -> H.modify_ $ _ { addTag { color = c } }
    Test e -> do
      logo e
      logs $ code e
