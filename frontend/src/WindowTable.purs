module WindowTable where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, any, drop, filter, length, take)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML (div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onChange, onClick, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, classes, disabled, type_)
import Halogen.Query.HalogenM (imapState)
import SemanticUI (sa, sbutton, sdiv, sicon)
import SemanticUI as S
import Socket (RequestId)
import Socket as Socket
import Util (logj, logo, logs, mwhen, prettifyJson)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement (indeterminate)
import Web.UIEvent.MouseEvent (MouseEvent)

pageSize :: Int
pageSize = 50

initialWindowParams :: { size :: Int, attached :: Boolean }
initialWindowParams = { size: pageSize * 2, attached: true }

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
  | RowClick r MouseEvent
  | ToggleAttached
  | NextPage
  | PreviousPage

type InnerState r
  = { pagesLoaded :: Int
    , attached :: Boolean
    , elements :: Array r
    , clicked :: Maybe Int
    , window :: Socket.RequestId
    , loading :: Boolean
    , page :: Int
    }

type State r
  = { inner :: Maybe (InnerState r)
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
  initialInnerState :: (Input r) -> Maybe (InnerState r)
  initialInnerState input = do
    stream <- input
    Just
      $ { pagesLoaded: 2
        , attached: initialWindowParams.attached
        , elements: []
        , clicked: Nothing
        , window: stream
        , loading: true
        , page: 0
        }

  initialState :: (Input r) -> (State r)
  initialState input = { inner: initialInnerState input }

  mapInner :: ∀ a s. a -> H.HalogenM (InnerState r) (Action r) s (Message r) Aff a -> H.HalogenM (State r) (Action r) s (Message r) Aff a
  mapInner result monad = do
    state <- H.get
    case state.inner of
      Nothing -> pure result
      -- TODO: can this be done nicely?
      Just inner ->
        imapState (\inner -> { inner: Just inner })
          ( \state -> case state.inner of
              Nothing -> unsafePerformEffect $ throw "There is literally no way this can ever occur"
              Just a -> a
          )
          monad

  mapInner_ :: ∀ s a. H.HalogenM (InnerState r) (Action r) s (Message r) Aff a -> H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  mapInner_ = mapInner unit <<< void

  handleAction :: ∀ s. (Action r) -> H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  handleAction = case _ of
    InputChanged input -> do
      H.modify_ (_ { inner = initialInnerState input })
      mapInner_ initStream
    Init -> do
      mapInner_ initStream
    WindowResponse update ->
      mapInner_ do
        H.modify_
          ( \state ->
              state
                { elements =
                  take (state.pagesLoaded * pageSize)
                    $ (\array -> update.windowUpdate.new <> array <> update.windowUpdate.extended)
                    $ identity -- TODO changed
                    $ filter (\stream -> all (_ /= identify stream) update.windowUpdate.deleted)
                    $ state.elements
                }
          )
        when (length update.windowUpdate.extended > 0) $ H.modify_ (_ { loading = false })
    RowClick element _ ->
      mapInner_ do
        H.raise $ ShowDetails element
        H.modify_ (_ { clicked = Just $ identify element })
    ToggleAttached ->
      mapInner_ do
        state <- H.modify (\state -> state { attached = not state.attached })
        sendWindowUpdate
    NextPage ->
      mapInner_ do
        state <- H.get
        when ((state.page + 2) * pageSize <= length state.elements) do
          H.modify_ $ \s -> s { page = s.page + 1 }
          when (state.page + 1 == state.pagesLoaded - 1) do
            H.modify_ $ \s -> s { pagesLoaded = s.pagesLoaded + 1 }
            void $ sendWindowUpdate
    PreviousPage ->
      mapInner_ do
        state <- H.get
        when (state.page > 0) (H.put $ state { page = state.page - 1 })

  sendWindowUpdate :: ∀ s. H.HalogenM (InnerState r) (Action r) s (Message r) Aff RequestId
  sendWindowUpdate = do
    state <- H.get
    Socket.request { windowUpdate: { id: state.window, params: { size: state.pagesLoaded * pageSize, attached: state.attached } } }

  initStream :: ∀ s. H.HalogenM (InnerState r) (Action r) s (Message r) Aff Unit
  initStream = do
    state <- H.get
    _ <- Socket.subscribeResponses WindowResponse state.window
    pure unit

  render :: (State r) -> H.ComponentHTML (Action r) () Aff
  render state = sdiv [ S.ui, S.container ] content
    where
    content = case state.inner of
      Nothing -> [ loader true ]
      Just inner ->
        if length inner.elements == 0 then
          [ loader false ]
        else
          [ sdiv [ S.ui, S.basic, S.segment ]
              [ HH.table [ classes [ S.ui, S.selectable, S.single, S.line, S.very, S.compact, S.table ] ]
                  [ HH.thead_ [ HH.tr_ $ map (\r -> HH.th_ [ text r ]) rows ]
                  , HK.tbody_
                      $ map
                          ( \r ->
                              let
                                id = identify r
                              in
                                Tuple (show id)
                                  ( HH.tr
                                      [ classes $ mwhen (Just id == inner.clicked) [ S.active ]
                                      , HE.onClick (Just <<< RowClick r)
                                      ]
                                      $ rowRenderer r
                                  )
                          )
                          (take pageSize $ drop (pageSize * inner.page) $ inner.elements)
                  ]
              , sdiv [ S.ui, S.right, S.rail ]
                  [ sdiv [ S.ui, S.compact, S.segments ]
                      [ sdiv [ S.ui, S.compact, S.segment ]
                          [ sdiv ([ S.ui, S.toggle, S.checkbox ] <> mwhen inner.attached [ S.checked ])
                              [ HH.input [ type_ InputCheckbox, checked inner.attached, onChange $ Just <<< (const ToggleAttached) ]
                              , HH.label_ [ text "Attached" ]
                              ]
                          ]
                      , sdiv [ S.ui, S.compact, S.center, S.aligned, S.segment ]
                          [ sdiv [ S.ui, S.pagination, S.menu ]
                              [ HH.a [ classes $ [ S.icon, S.item ] <> mwhen (inner.page == 0) [ S.disabled ], onClick $ Just <<< (const PreviousPage) ] [ sicon [ S.angle, S.left ] ]
                              , sdiv [ S.item, S.label ] [ text $ show (inner.page + 1) ]
                              , HH.a [ classes [ S.icon, S.item ], onClick $ Just <<< (const NextPage) ] [ sicon [ S.angle, S.right ] ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]

    loader indeterminate =
      sdiv [ S.ui, S.placeholder, S.segment ]
        [ sdiv [ S.ui, S.active, S.dimmer ]
            [ sdiv ([ S.ui, S.loader ] <> mwhen indeterminate [ S.indeterminate ]) []
            ]
        ]
