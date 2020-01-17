module SkarlarkEditor where

import Prelude
import ConfigurationTypes (Configuration)
import Configuration as Config
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (a, div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import SemanticUI (sa, sdiv)
import SemanticUI as S
import Util (logo, logs)
import Web.HTML (HTMLElement)

foreign import data Editor :: Type

foreign import init :: HTMLElement -> Effect Editor

foreign import content :: Editor -> Effect String

foreign import setError :: String -> Effect Unit

type State
  = { config :: Maybe Configuration, editor :: Maybe Editor }

data Query a
  = GetValue (String -> a)
  | SetError (String) a

data Action
  = Init

component :: ∀ o i. H.Component HH.HTML Query i o Aff
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

  handleAction :: ∀ s. Action -> H.HalogenM State Action s o Aff Unit
  handleAction = case _ of
    Init -> do
      element <- H.getHTMLElementRef elem
      case element of
        Nothing -> logs "wtf element is missing"
        Just element -> do
          editor <- H.liftEffect $ init element
          H.modify_ $ _ { editor = Just editor }

  handleQuery :: ∀ a s. Query a -> H.HalogenM State Action s o Aff (Maybe a)
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
