module StarlarkEditor where

import Prelude
import CSS as CSS
import Configuration as Config
import ConfigurationTypes (Configuration, Service, Tag)
import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (length)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error)
import Foreign.Object (Object, empty, filterKeys, lookup, mapWithKey, member, toUnfoldable)
import Halogen (ClassName(..), SubscriptionId)
import Halogen as H
import Halogen.HTML (a, div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (EventSource(..), effectEventSource, emit)
import Halogen.Query.HalogenM (mapAction)
import SemanticUI (sa, sdiv)
import SemanticUI as S
import Socket as Socket
import Util (logo, logs)
import Web.HTML (HTMLElement)

foreign import data Editor :: Type

foreign import data ContextKey :: Type -> Type

foreign import init :: HTMLElement -> Effect Editor

foreign import content :: Editor -> Effect String

foreign import setError :: Editor -> String -> Effect Unit

foreign import updateLanguage :: (Array Tag) -> (Array Service) -> Effect Unit

foreign import setContent :: Editor -> String -> Effect Unit

foreign import addAction :: Editor -> EditorAction -> Effect Unit

foreign import showTextInput :: Editor -> String -> String -> (String -> Effect Unit) -> Effect Unit

foreign import saveToLocalStorage :: String -> String -> Effect Unit

foreign import loadFromLocalStorage :: Effect (Object String)

foreign import createContextKey :: ∀ a. Editor -> String -> a -> Effect (ContextKey a)

foreign import setContextKey :: ∀ a. ContextKey a -> a -> Effect Unit

foreign import onEdit :: (String -> Effect Unit) -> Editor -> Effect Unit

type EditorAction
  = { id :: String
    , label :: String
    , keybindings :: Array Int
    , precondition :: String
    , contextMenuOrder :: Number
    , run :: Effect Unit
    }

type State
  = { config :: Maybe Configuration, editor :: Maybe Editor, loadedFile :: Maybe File, editorContent :: String }

type File
  = { name :: String, content :: String }

data Query a
  = GetValue (String -> a)
  | SetError (String) a

data Message
  = Execute

data Action
  = Init
  | ConfigUpdate Configuration
  | TriggerExecute
  | Save
  | Delete
  | SaveAsAction
  | SaveAs String
  | Load File
  | OnEdit String

defaultContent :: String
defaultContent =
  """index(service=services.some_service)

def q():
    if id % 1000 == 0:
        emit(client_port)
    return id % 500 == 0

sort_key(data_len)
q()
# press ctrl + space for autocompletion, all relevant builtins are listed there
# press ctrl + space again while autocompletion is open for more information on builtins
# press F1 to see all available commands, these are all the custom ones:
# Execute Query (Shift + Enter)
# Save to local storage (Ctrl + S)
# Load local: <save name>
# Reload local storage (in case you want to load a script you saved in another tab)"""

source :: ∀ a. ((a -> Effect Unit) -> Effect Unit) -> EventSource Aff a
source f =
  effectEventSource \emitter -> do
    f $ emit emitter
    pure mempty

actionSource :: Editor -> { id :: String, label :: String, keybindings :: Array Int, contextMenuOrder :: Number, precondition :: String } -> EventSource Aff Unit
actionSource editor action = source $ \callback -> addAction editor { id: action.id, label: action.label, keybindings: action.keybindings, contextMenuOrder: action.contextMenuOrder, precondition: action.precondition, run: callback unit }

textInputSource :: Editor -> String -> String -> EventSource Aff String
textInputSource editor text id = source $ \callback -> showTextInput editor text id callback

subscribeAction :: ∀ s. Action -> Editor -> { id :: String, label :: String, keybindings :: Array Int, contextMenuOrder :: Number, precondition :: String } -> H.HalogenM State Action s Message Aff SubscriptionId
subscribeAction action editor = mapAction (const action) <<< H.subscribe <<< actionSource editor

subscribeTextInput :: ∀ s. (String -> Action) -> Editor -> String -> String -> H.HalogenM State Action s Message Aff SubscriptionId
subscribeTextInput action editor text = mapAction action <<< H.subscribe <<< textInputSource editor text

onEditSource :: Editor -> EventSource Aff String
onEditSource editor = source $ \callback -> onEdit callback editor

subscribeOnEdit :: ∀ s. (String -> Action) -> Editor -> H.HalogenM State Action s Message Aff SubscriptionId
subscribeOnEdit action = mapAction action <<< H.subscribe <<< onEditSource

component :: ∀ i. H.Component HH.HTML Query i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  initialState :: i -> State
  initialState _ = { config: Nothing, editor: Nothing, loadedFile: Nothing, editorContent: defaultContent }

  elem :: H.RefLabel
  elem = H.RefLabel "element"

  render :: ∀ s. State -> H.ComponentHTML Action s Aff
  render state =
    div_
      [ div
          [ classes [ S.ui, S.black, S.label ]
          , HC.style
              $ do
                  CSS.borderRadius (CSS.rem 0.21428571) (CSS.rem 0.21428571) (CSS.rem 0.0) (CSS.rem 0.0)
                  CSS.width $ CSS.pct 100.0
          ]
          [ (if isNothing state.loadedFile || Just state.editorContent == map _.content state.loadedFile then text else pendingChanges) $ fromMaybe "Local Scratch Buffer" (map _.name state.loadedFile)
          ]
      , div [ HP.ref elem, classes [ ClassName "editor" ] ] []
      ]
    where
    pendingChanges c = HH.i_ [ text $ c <> "*" ]

  handleAction :: ∀ s. Action -> H.HalogenM State Action s Message Aff Unit
  handleAction = case _ of
    Init -> do
      _ <- Config.subscribe ConfigUpdate
      H.liftEffect $ updateLanguage [] []
      element <- H.getHTMLElementRef elem
      case element of
        Nothing -> H.liftEffect $ error "wtf element is missing"
        Just element -> do
          editor <- H.liftEffect $ init element
          saves <- H.liftEffect loadFromLocalStorage
          H.liftEffect $ setContent editor $ fromMaybe defaultContent $ lookup "scratch" saves
          _ <- subscribeAction TriggerExecute editor { id: "execute", label: "Execute Query", keybindings: [ 1027 ], contextMenuOrder: 0.0, precondition: "" }
          _ <- subscribeAction Save editor { id: "save", label: "Save", keybindings: [ 2097 ], contextMenuOrder: 0.0, precondition: "" }
          _ <- subscribeAction SaveAsAction editor { id: "saveAs", label: "Save as...", keybindings: [], contextMenuOrder: 0.0, precondition: "" }
          _ <- subscribeOnEdit OnEdit editor
          H.modify_ $ _ { editor = Just editor }
    ConfigUpdate config -> do
      H.modify_ $ _ { config = Just config }
      H.liftEffect $ updateLanguage config.tags config.services
      state <- H.get
      maybe (pure unit)
        ( \editor ->
            void $ sequence $ mapWithKey (\name value -> subscribeAction (Load { name: name, content: value }) editor { id: "load-" <> name, label: "Load: " <> name, keybindings: [], contextMenuOrder: 0.0, precondition: "" }) config.scripts
        )
        state.editor
    TriggerExecute -> do
      H.raise Execute
    SaveAsAction -> do
      state <- H.get
      maybe (pure unit)
        ( \editor ->
            void $ subscribeTextInput SaveAs editor "Enter script name" "saveAs"
        )
        state.editor
    SaveAs name ->
      when (length name > 0) do
        state <- H.get
        maybe (pure unit)
          ( \editor -> do
              value <- H.liftEffect $ content editor
              void $ Socket.request { updateConfiguration: { setScript: Tuple name value } }
              H.modify_ $ _ { loadedFile = Just { name: name, content: value } }
          )
          state.editor
    Delete -> do
      state <- H.get
      maybe (pure unit)
        ( \file ->
            void $ Socket.request { updateConfiguration: { removeScript: file } }
        )
        state.loadedFile
    Save -> do
      state <- H.get
      maybe (handleAction SaveAsAction)
        ( \file -> do
            handleAction $ SaveAs file.name
        )
        state.loadedFile
    Load file -> do
      state <- H.get
      maybe (pure unit)
        ( \editor ->
            H.liftEffect $ setContent editor file.content
        )
        state.editor
      H.modify_ $ _ { loadedFile = Just file }
    OnEdit value -> do
      H.liftEffect $ saveToLocalStorage "scratch" value
      H.modify_ $ _ { editorContent = value }

  handleQuery :: ∀ a s. Query a -> H.HalogenM State Action s Message Aff (Maybe a)
  handleQuery = case _ of
    GetValue f -> do
      state <- H.get
      case state.editor of
        Nothing -> pure Nothing
        Just editor -> do
          cnt <- H.liftEffect $ content editor
          pure $ Just $ f cnt
    SetError s a -> do
      state <- H.get
      maybe (pure unit)
        ( \editor -> H.liftEffect $ setError editor s
        )
        state.editor
      pure $ Just a
