module WindowTable where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (any, filter, take)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML (div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import SemanticUI as S
import Socket as Socket
import Util (logj, logs, prettifyJson)

initialWindowParams :: { size :: Int, attached :: Boolean }
initialWindowParams = { size: 50, attached: true }

data Query a
  = NoOpQ a

type WindowUpdate r
  = { windowUpdate ::
      { new :: Array r
      , extended :: Array r
      , changed :: Array r
      , deleted :: Array Int
      }
    }

data Action r
  = InputChanged (Input r)
  | Init
  | WindowResponse (WindowUpdate r)

type InnerState r
  = { windowSize :: Int
    , attached :: Boolean
    , elements :: Array r
    }

type State r
  = { window :: Maybe Socket.RequestId
    , inner :: Maybe (InnerState r)
    }

type Input r
  = Maybe Socket.RequestId

data Message r
  = ShowDetails r

component :: ∀ r. DecodeJson r => EncodeJson r => (r -> Int) -> Array String -> (r -> Array (H.ComponentHTML (Action r) () Aff)) -> H.Component HH.HTML Query (Input r) (Message r) Aff
component identify rows rowRenderer =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< InputChanged, initialize = Just Init }
    }
  where
  initialState :: (Input r) -> (State r)
  initialState input = { window: input, inner: Nothing }

  handleAction :: ∀ s. (Action r) -> H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  handleAction = case _ of
    InputChanged input -> do
      H.modify_ (_ { window = input })
      initStream
    Init -> do
      initStream
    WindowResponse update -> do
      state <-
        H.get
      H.put
        $ state
            { inner =
              add update.windowUpdate.new $ extended update.windowUpdate.extended $ changed update.windowUpdate.changed $ deleted update.windowUpdate.deleted $ state.inner
            }
      pure unit

  add :: Array r -> Maybe (InnerState r) -> Maybe (InnerState r)
  add new = map (\inner -> inner { elements = take inner.windowSize $ new <> inner.elements })

  extended :: Array r -> Maybe (InnerState r) -> Maybe (InnerState r)
  extended new = map (\inner -> inner { elements = inner.elements <> new })

  changed :: Array r -> Maybe (InnerState r) -> Maybe (InnerState r)
  changed changes = identity

  deleted :: Array Int -> Maybe (InnerState r) -> Maybe (InnerState r)
  deleted deletes = map (\inner -> inner { elements = filtered inner.elements })
    where
    filtered :: Array r -> Array r
    filtered elements = filter (\stream -> any (_ == identify stream) deletes) elements

  initStream :: ∀ s. H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  initStream = do
    state <- H.get
    inner <- case state.window of
      Nothing -> pure Nothing
      Just window -> do
        _ <- Socket.subscribeResponses WindowResponse window
        pure $ Just { windowSize: initialWindowParams.size, attached: initialWindowParams.attached, elements: [] }
    H.modify_ (_ { inner = inner })

  render :: (State r) -> H.ComponentHTML (Action r) () Aff
  render state =
    div [ classes [ S.ui, S.container ] ]
      [ content
      ]
    where
    content = case state.inner of
      Nothing -> div [ classes [ S.ui, S.placeholder, S.segment ] ] [ div [ classes [ S.ui, S.active, S.dimmer ] ] [ div [ classes $ [ S.ui, S.loader ] <> if isNothing state.window then [ S.indeterminate ] else [] ] [] ] ]
      Just inner ->
        HH.table [ classes [ S.ui, S.selectable, S.single, S.line, S.compact, S.table ] ]
          [ HH.thead_ [ HH.tr_ $ map (\r -> HH.th_ [ text r ]) rows ]
          , HH.tbody_ $ map (\r -> HH.tr_ $ rowRenderer r) inner.elements
          ]
