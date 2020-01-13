module Tags where

import Prelude
import Configuration (Configuration)
import Configuration as Config
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
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import SemanticUI (sa, sdiv)
import SemanticUI as S
import Util (logo)

type Input
  = Array Int

type State
  = { input :: Input, config :: Maybe Configuration }

data Action
  = AddTag Int
  | RemoveTag Int
  | ConfigUpdate Configuration
  | Init

component :: ∀ q o. Boolean -> H.Component HH.HTML q Input o Aff
component editable =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
    }
  where
  initialState :: Input -> State
  initialState input = { input: input, config: Nothing }

  render :: ∀ s. State -> H.ComponentHTML Action s Aff
  render state =
    if editable then
      div_ []
    else
      HK.div_
        $ map
            ( \id ->
                let
                  tag = fromMaybe { name: "Tag not Found!", color: "red", slug: show $ id, owner: "nobody" } $ (\c -> lookup (show id) c.tags) =<< state.config
                in
                  Tuple tag.slug $ sdiv ([ S.ui, S.label ] <> [ ClassName tag.color ]) [ text tag.name ]
            )
            state.input

  handleAction :: ∀ s. Action -> H.HalogenM State Action s o Aff Unit
  handleAction = case _ of
    Init -> void $ Config.subscribe ConfigUpdate
    ConfigUpdate config -> do
      H.modify_ $ _ { config = Just config }
    _ -> pure unit
