module Configuration (init, set, subscribe, component) where

import ConfigurationTypes
import Prelude
import Data.Argonaut.Core (Json)
import Data.Array (any, filter, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, cons', head, singleton, toArray)
import Data.BigInt (BigInt)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Dropdown as Dropdown
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (Object, alter, empty, fold, lookup, toUnfoldable, union, values)
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
import Util (Id, Size, logo, logs, mwhen, onEnter)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code)

foreign import listen :: (Configuration -> Effect Unit) -> Effect Unit

foreign import unlisten :: (Configuration -> Effect Unit) -> Effect Unit

foreign import setImpl :: Configuration -> Effect Unit

foreign import getImpl :: (Configuration -> Maybe Configuration) -> Maybe Configuration -> Effect (Maybe Configuration)

init :: ∀ state a slot m. HalogenM state a slot m Aff RequestId
init = Socket.request { watch: "configuration" }

set :: ∀ state a slot m. ConfigurationMessage -> HalogenM state a slot m Aff Unit
set = H.liftEffect <<< setImpl <<< fromMessage

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
  = { config :: Maybe Configuration, addTag :: EditTag, editTag :: Maybe EditTag, editService :: Maybe EditService, tagSizes :: Object Size, serviceSizes :: Object NewService }

type IndexSizes
  = { services :: Object Size
    , tags :: Object Size
    }

data Action
  = Init
  | SocketConnect
  | ConfigUpdate Configuration
  | AddTagUpdate EditTag
  | SubmitAddTag
  | EditTagUpdate EditTag
  | SubmitEditTag
  | EditTagColorChange Dropdown.ColorMessage
  | AddTagColorChange Dropdown.ColorMessage
  | AddServiceUpdate String NewService
  | SubmitAddService String
  | EditServiceUpdate EditService
  | SubmitEditService
  | IndexSizesUpdate { indexSizes :: IndexSizes }

type Slot
  = ( addColor :: H.Slot Identity Dropdown.ColorMessage Unit
    , editColor :: H.Slot Identity Dropdown.ColorMessage String
    )

_addColor = SProxy :: SProxy "addColor"

_editColor = SProxy :: SProxy "editColor"

type NewService
  = { name :: String
    , slug :: String
    , port :: Int
    , streams :: Size
    }

type EditTag
  = { name :: String
    , slug :: String
    , color :: Dropdown.Color
    , owner :: String
    }

type EditService
  = ServiceMessage

editTagToTag :: EditTag -> TagMessage
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
  initialState input = { config: Nothing, addTag: { slug: "", name: "", color: head Dropdown.colors, owner: "webui" }, editTag: Nothing, editService: Nothing, tagSizes: empty, serviceSizes: empty }

  handleAction :: Action -> H.HalogenM State Action Slot o Aff Unit
  handleAction = case _ of
    Init -> do
      _ <- subscribe ConfigUpdate
      void $ Socket.subscribeConnect SocketConnect
    SocketConnect -> do
      id <- Socket.request { watch: "indexSizes" }
      void $ Socket.subscribeResponses IndexSizesUpdate id
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
    AddServiceUpdate port service -> do
      H.modify_
        $ \state ->
            state
              { serviceSizes =
                alter
                  ( case _ of
                      Nothing -> Nothing
                      Just _ -> Just service
                  )
                  port
                  state.serviceSizes
              }
    SubmitAddService port -> do
      state <- H.get
      let
        service = lookup port state.serviceSizes
      maybe (pure unit) (\service -> void $ Socket.request { updateConfiguration: { setService: { slug: service.slug, name: service.name, port: service.port } } }) service
    EditServiceUpdate service -> H.modify_ $ _ { editService = Just service }
    SubmitEditService -> do
      state <- H.get
      _ <- Socket.request { updateConfiguration: { setService: state.editService } }
      H.modify_ $ _ { editService = Nothing }
    IndexSizesUpdate update ->
      void
        $ H.modify_
            ( \state ->
                state
                  { tagSizes = union update.indexSizes.tags state.tagSizes
                  , serviceSizes =
                    fold
                      ( \b identifier a ->
                          alter
                            ( case _ of
                                Nothing -> Just { name: "", slug: "", port: fromMaybe 0 $ fromString identifier, streams: a }
                                Just r -> Just $ r { streams = a }
                            )
                            identifier
                            b
                      )
                      state.serviceSizes
                      update.indexSizes.services
                  }
            )

  render :: State -> H.ComponentHTML Action Slot Aff
  render state = sdiv [ S.ui, S.container ] content
    where
    content = case state.config of
      Nothing -> [ loaderDiv false ]
      Just config ->
        [ HH.h1_ [ text "Tags" ]
        , HH.table [ classes [ S.center, S.ui, S.basic, S.compact, S.collapsing, S.celled, S.table ] ]
            [ HH.thead_ [ HH.tr_ [ HH.th_ [ text "Slug" ], HH.th_ [ text "Name" ], HH.th_ [ text "Owner" ], HH.th_ [ text "Color" ], HH.th_ [ text "Streams" ], HH.th_ [] ] ]
            , HK.tbody_
                $ ( map
                      ( \tag ->
                          Tuple (show tag.id)
                            $ case state.editTag of
                                Just editTag
                                  | editTag.slug == tag.slug ->
                                    HH.tr_
                                      [ HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditTag)
                                              ]
                                              [ sdiv [ S.disabled, S.field ]
                                                  [ HH.input [ type_ InputText, disabled true, value editTag.slug ] ]
                                              ]
                                          ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditTag)
                                              ]
                                              [ sdiv [ S.field ]
                                                  [ HH.input [ type_ InputText, onValueChange $ Just <<< EditTagUpdate <<< (editTag { name = _ }), value editTag.name ] ]
                                              ]
                                          ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditTag)
                                              ]
                                              [ sdiv [ S.disabled, S.field ]
                                                  [ HH.input [ type_ InputText, disabled true, value editTag.owner ] ]
                                              ]
                                          ]
                                      , HH.slot _editColor editTag.slug Dropdown.colorDropdown { selection: editTag.color, rows: toArray Dropdown.colors } $ Just <<< EditTagColorChange
                                      , HH.td_ [ text $ maybe "-" show $ lookup (show tag.id) state.tagSizes ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditTag)
                                              ]
                                              [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const SubmitEditTag) ] [ text $ "Submit" ] ]
                                          ]
                                      ]
                                _ ->
                                  HH.tr_
                                    [ HH.td_ [ text $ tag.slug ]
                                    , HH.td_ [ text $ tag.name ]
                                    , HH.td_ [ text $ tag.owner ]
                                    , HH.td_ [ sdiv [ S.ui, S.label, ClassName tag.color ] [ text $ (Dropdown.valueToColor tag.color).name ] ]
                                    , HH.td_ [ text $ maybe "-" show $ lookup (show tag.id) state.tagSizes ]
                                    , HH.td_ [ HH.button [ classes [ S.ui, S.mini, S.button ], onClick $ Just <<< (const $ EditTagUpdate { name: tag.name, slug: tag.slug, color: Dropdown.valueToColor tag.color, owner: tag.owner }) ] [ text $ "Edit" ] ]
                                    ]
                      )
                      config.tags
                  )
                <> [ Tuple "add"
                      $ HH.tr_
                          [ HH.td_
                              [ HH.form
                                  [ classes [ S.ui, S.form ]
                                  , onSubmit $ Just <<< (const SubmitAddTag)
                                  ]
                                  [ sdiv [ S.field ]
                                      [ HH.input [ type_ InputText, onValueChange $ Just <<< AddTagUpdate <<< (state.addTag { slug = _ }), value state.addTag.slug ] ]
                                  ]
                              ]
                          , HH.td_
                              [ HH.form
                                  [ classes [ S.ui, S.form ]
                                  , onSubmit $ Just <<< (const SubmitAddTag)
                                  ]
                                  [ sdiv [ S.field ]
                                      [ HH.input [ type_ InputText, onValueChange $ Just <<< AddTagUpdate <<< (state.addTag { name = _ }), value state.addTag.name ] ]
                                  ]
                              ]
                          , HH.td_
                              [ HH.form
                                  [ classes [ S.ui, S.form ]
                                  , onSubmit $ Just <<< (const SubmitAddTag)
                                  ]
                                  [ sdiv [ S.disabled, S.field ]
                                      [ HH.input [ type_ InputText, disabled true, value state.addTag.owner ] ]
                                  ]
                              ]
                          , HH.slot _addColor unit Dropdown.colorDropdown { selection: state.addTag.color, rows: toArray Dropdown.colors } $ Just <<< AddTagColorChange
                          , HH.td_ [ text "-" ]
                          , HH.td_
                              [ HH.form
                                  [ classes [ S.ui, S.form ]
                                  , onSubmit $ Just <<< (const SubmitAddTag)
                                  ]
                                  [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const SubmitAddTag) ] [ text $ "Add" ] ]
                              ]
                          ]
                  ]
            ]
        , HH.h1_ [ text "Services" ]
        , HH.table [ classes [ S.center, S.ui, S.basic, S.compact, S.collapsing, S.celled, S.table ] ]
            [ HH.thead_ [ HH.tr_ [ HH.th_ [ text "Slug" ], HH.th_ [ text "Port" ], HH.th_ [ text "Name" ], HH.th_ [ text "Streams" ], HH.th_ [] ] ]
            , HK.tbody_
                $ ( map
                      ( \service ->
                          Tuple (show service.id)
                            $ case state.editService of
                                Just editService
                                  | editService.slug == service.slug ->
                                    HH.tr_
                                      [ HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditService)
                                              ]
                                              [ sdiv [ S.disabled, S.field ] [ HH.input [ type_ InputText, disabled true, onValueChange $ Just <<< EditServiceUpdate <<< (editService { slug = _ }), value editService.slug ] ] ]
                                          ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditService)
                                              ]
                                              [ sdiv [ S.field, S.disabled ]
                                                  [ HH.input [ disabled true, type_ InputNumber, onValueChange $ Just <<< EditServiceUpdate <<< (editService { port = _ }) <<< fromMaybe 0 <<< fromString, value $ show $ editService.port ] ]
                                              ]
                                          ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ]
                                              , onSubmit $ Just <<< (const SubmitEditService)
                                              ]
                                              [ sdiv [ S.field ]
                                                  [ HH.input [ type_ InputText, onValueChange $ Just <<< EditServiceUpdate <<< (editService { name = _ }), value editService.name ] ]
                                              ]
                                          ]
                                      , HH.td_ [ text $ maybe "-" (\o -> show o.streams) $ lookup (show service.port) state.serviceSizes ]
                                      , HH.td_
                                          [ HH.form
                                              [ classes [ S.ui, S.form ], onSubmit $ Just <<< (const SubmitEditService)
                                              ]
                                              [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const SubmitEditService) ] [ text $ "Submit" ] ]
                                          ]
                                      ]
                                _ ->
                                  HH.tr_
                                    [ HH.td_ [ text $ service.slug ]
                                    , HH.td_ [ text $ show service.port ]
                                    , HH.td_ [ text $ service.name ]
                                    , HH.td_ [ text $ maybe "-" (\o -> show o.streams) $ lookup (show service.port) state.serviceSizes ]
                                    , HH.td_ [ HH.button [ classes [ S.ui, S.mini, S.button ], onClick $ Just <<< (const $ EditServiceUpdate $ toServiceMessage service) ] [ text $ "Edit" ] ]
                                    ]
                      )
                      config.services
                  )
                <> map
                    ( \service ->
                        Tuple ("new" <> show service.port)
                          $ HH.tr_
                              [ HH.td_
                                  [ HH.form
                                      [ classes [ S.ui, S.form ]
                                      , onSubmit $ Just <<< (const $ SubmitAddService $ show service.port)
                                      ]
                                      [ sdiv [ S.field ]
                                          [ HH.input [ type_ InputText, onValueChange $ Just <<< AddServiceUpdate (show service.port) <<< (service { slug = _ }), value service.slug ] ]
                                      ]
                                  ]
                              , HH.td_
                                  [ HH.form
                                      [ classes [ S.ui, S.form ]
                                      , onSubmit $ Just <<< (const $ SubmitAddService $ show service.port)
                                      ]
                                      [ sdiv [ S.disabled, S.field ]
                                          [ HH.input [ disabled true, type_ InputNumber, onValueChange $ Just <<< AddServiceUpdate (show service.port) <<< (service { port = _ }) <<< fromMaybe 0 <<< fromString, value $ show $ service.port ] ]
                                      ]
                                  ]
                              , HH.td_
                                  [ HH.form
                                      [ classes [ S.ui, S.form ]
                                      , onSubmit $ Just <<< (const $ SubmitAddService $ show service.port)
                                      ]
                                      [ sdiv [ S.field ]
                                          [ HH.input [ type_ InputText, onValueChange $ Just <<< AddServiceUpdate (show service.port) <<< (service { name = _ }), value service.name ] ]
                                      ]
                                  ]
                              , HH.td_ [ text $ show service.streams ]
                              , HH.td_
                                  [ HH.form
                                      [ classes [ S.ui, S.form ]
                                      , onSubmit $ Just <<< (const $ SubmitAddService $ show service.port)
                                      ]
                                      [ HH.button [ classes [ S.ui, S.mini, S.button, S.green ], onClick $ Just <<< (const $ SubmitAddService $ show service.port) ] [ text $ "Add" ] ]
                                  ]
                              ]
                    )
                    (sortBy (\a b -> compare b.streams a.streams) $ filter (\s -> not $ any (\s2 -> s.port == s2.port) config.services) $ values state.serviceSizes)
            ]
        ]
