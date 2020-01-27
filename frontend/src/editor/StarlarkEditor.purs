module StarlarkEditor where

import Prelude
import Configuration as Config
import ConfigurationTypes (Configuration, Service, Tag)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error)
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
import Util (logo, logs)
import Web.HTML (HTMLElement)

foreign import data Editor :: Type

foreign import init :: HTMLElement -> Effect Editor

foreign import content :: Editor -> Effect String

foreign import setError :: String -> Effect Unit

foreign import updateLanguage :: (Array Tag) -> (Array Service) -> Effect Unit

foreign import setContent :: Editor -> String -> Effect Unit

foreign import addAction :: Editor -> EditorAction -> Effect Unit

type EditorAction
  = { id :: String
    , label :: String
    , keybindings :: Array Int
    , contextMenuOrder :: Number
    , run :: Effect Unit
    }

type State
  = { config :: Maybe Configuration, editor :: Maybe Editor }

data Query a
  = GetValue (String -> a)
  | SetError (String) a

data Message
  = Execute

data Action
  = Init
  | ConfigUpdate Configuration
  | TriggerExecute

defaultContent :: String
defaultContent =
  """# index(tag=tags.xxx, service=services.xxx) / index(tag=tags.xxx) / index(service=services.xxx)
# ^- put this call somewhere where it will get called on every execution to choose the index on which this script
#    should run, otherwise the query will get executed on the "everything"-index

# The last expression of the query gets used as a filter, unless you call emit or addTag, then the query will accept the stream
# If the last expression is not a boolean, the result will get reported back to you as json
id % 500 == 0

# snacc supports queries written in starlark, a non-turing complete python dialect
# all provided values and most builtin functions are available and documented via autocomplete (ctrl + space)
# press ctrl+space while the autocomplete window is open to see further documentation
# starlark contains most terminating python features, such as list and dictionary comprehensions as well as for loops
# and non-recursive function definitions
# more infos here: https://github.com/bazelbuild/starlark/blob/master/spec.md
# press F1 to see all available commands, these are all custom ones:
# Execute Query (Shift + Enter)
# TODO: save
# TODO: load"""

source :: ∀ a. ((a -> Effect Unit) -> Effect Unit) -> EventSource Aff a
source f =
  effectEventSource \emitter -> do
    f $ emit emitter
    pure mempty

actionSource :: Editor -> { id :: String, label :: String, keybindings :: Array Int, contextMenuOrder :: Number } -> EventSource Aff Unit
actionSource editor action = source $ \callback -> addAction editor { id: action.id, label: action.label, keybindings: action.keybindings, contextMenuOrder: action.contextMenuOrder, run: callback unit }

subscribeAction :: ∀ s. Action -> Editor -> { id :: String, label :: String, keybindings :: Array Int, contextMenuOrder :: Number } -> H.HalogenM State Action s Message Aff SubscriptionId
subscribeAction action editor = mapAction (const action) <<< H.subscribe <<< actionSource editor

component :: ∀ i. H.Component HH.HTML Query i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  initialState :: i -> State
  initialState _ = { config: Nothing, editor: Nothing }

  elem :: H.RefLabel
  elem = H.RefLabel "element"

  render :: ∀ s. State -> H.ComponentHTML Action s Aff
  render state = div [ HP.ref elem, classes [ ClassName "editor" ] ] []

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
          H.liftEffect $ setContent editor defaultContent
          _ <- subscribeAction TriggerExecute editor { id: "execute", label: "Execute Query", keybindings: [ 1027 ], contextMenuOrder: 0.0 }
          H.modify_ $ _ { editor = Just editor }
    ConfigUpdate config -> do
      H.modify_ $ _ { config = Just config }
      H.liftEffect $ updateLanguage config.tags config.services
    TriggerExecute -> H.raise Execute

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
      H.liftEffect $ setError s
      pure $ Just a
