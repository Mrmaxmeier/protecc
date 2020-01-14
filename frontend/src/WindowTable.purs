module WindowTable where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, any, drop, filter, foldr, insert, insertBy, length, reverse, take)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Ordering (invert)
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
import Keyevent as Keyevent
import Partial.Unsafe (unsafeCrashWith)
import SemanticUI (loaderDiv, sa, sbutton, sdiv, sicon)
import SemanticUI as S
import Socket (RequestId)
import Socket as Socket
import Util (Id, dropUntil, logj, logo, logs, mwhen, prettifyJson)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement (indeterminate)
import Web.UIEvent.MouseEvent (MouseEvent)

pageSize :: Int
pageSize = 25

initialWindowParams :: { size :: Int, attached :: Boolean }
initialWindowParams = { size: pageSize * 2, attached: true }

data Query a
  = NoOpQ a

type WindowUpdate r
  = { windowUpdate ::
      { new :: Array r
      , deleted :: Array Id
      , changed :: Array r
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
  | SelectNext
  | SelectPrev

type InnerState r
  = { pagesLoaded :: Int
    , attached :: Boolean
    , elements :: Array r
    , clicked :: Maybe Id
    , window :: Socket.RequestId
    , loading :: Boolean
    , recvdEmpty :: Boolean
    , page :: Int
    }

type RowDescription
  = { name :: String, width :: Int }

type State r
  = Maybe (InnerState r)

type Input r
  = Maybe Socket.RequestId

data Message r
  = ShowDetails r
  | CloseDetails

component :: ∀ r s. DecodeJson r => EncodeJson r => (r -> Id) -> Array RowDescription -> (r -> Array (H.ComponentHTML (Action r) s Aff)) -> H.Component HH.HTML Query (Input r) (Message r) Aff
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
        , loading: false
        , recvdEmpty: false
        , page: 0
        }

  initialState :: (Input r) -> (State r)
  initialState input = initialInnerState input

  mapInner :: ∀ a s. a -> H.HalogenM (InnerState r) (Action r) s (Message r) Aff a -> H.HalogenM (State r) (Action r) s (Message r) Aff a
  mapInner result monad = do
    state <- H.get
    case state of
      Nothing -> pure result
      Just inner ->
        imapState (\inner -> Just inner)
          ( \state -> case state of
              Nothing -> unsafeCrashWith "There is literally no way this can ever occur"
              Just a -> a
          )
          monad

  mapInner_ :: ∀ s a. H.HalogenM (InnerState r) (Action r) s (Message r) Aff a -> H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  mapInner_ = mapInner unit <<< void

  handleAction :: ∀ s. (Action r) -> H.HalogenM (State r) (Action r) s (Message r) Aff Unit
  handleAction = case _ of
    InputChanged input -> do
      state <- H.get
      when (maybe true (\new -> maybe true (\inner -> new /= inner.window) state) input) do
        H.put $ initialInnerState input
        mapInner_ initStream
    Init -> do
      mapInner_ initStream
      _ <- Keyevent.subscribe 39 NextPage
      _ <- Keyevent.subscribe 37 PreviousPage
      _ <- Keyevent.subscribe 38 SelectPrev
      _ <- Keyevent.subscribe 40 SelectNext
      void $ Keyevent.subscribe 65 ToggleAttached
    WindowResponse { windowUpdate: update } -> do
      nextPage <-
        mapInner false do
          state <-
            H.modify
              ( \state ->
                  state
                    { elements =
                      take (state.pagesLoaded * pageSize)
                        $ (\array -> foldr (insertBy $ \a b -> invert $ compare (identify a) (identify b)) array $ update.new <> update.changed)
                        $ filter (\stream -> all (_ /= identify stream) (update.deleted <> map identify update.changed))
                        $ state.elements
                    }
              )
          let
            enoughElements = state.pagesLoaded * pageSize <= length state.elements && state.loading
          when (enoughElements) do
            H.modify_ (_ { loading = false })
          when (maybe false (\e1 -> all (\e2 -> e1 /= identify e2) state.elements) state.clicked) do
            H.modify_ $ _ { clicked = Nothing }
          when (length update.new == 0 && length update.deleted == 0) do
            H.modify_ $ _ { recvdEmpty = true }
          pure enoughElements
      when nextPage $ handleAction NextPage
    RowClick element _ -> do
      mapInner_ do
        H.raise $ ShowDetails element
        state <- H.get
        when (state.attached) do
          H.modify_ $ _ { attached = false }
          sendWindowUpdate
        H.modify_ (_ { clicked = Just $ identify element })
    ToggleAttached ->
      mapInner_ do
        state <- H.modify (\state -> state { attached = not state.attached })
        when (state.attached) $ H.modify_ $ _ { page = 0, pagesLoaded = 2, elements = take (2 * pageSize) state.elements }
        sendWindowUpdate
    NextPage ->
      mapInner_ do
        state <- H.get
        let
          canAdvance = (state.page + 2) * pageSize <= length state.elements
        when canAdvance do
          H.modify_ $ \s -> s { page = s.page + 1 }
          let
            loadMore = state.page + 1 == state.pagesLoaded - 1
          when (state.attached) do
            H.modify_ $ _ { attached = false }
            when (not loadMore) $ sendWindowUpdate
          when loadMore do
            H.modify_ $ \s -> s { pagesLoaded = s.pagesLoaded + 1 }
            sendWindowUpdate
        when (not canAdvance) do
          H.modify_ $ _ { loading = true }
    PreviousPage ->
      mapInner_ do
        state <- H.get
        when (state.page > 0) (H.put $ state { page = state.page - 1, loading = false })
    SelectNext ->
      mapInner_ do
        state <- H.get
        case state.clicked of
          Nothing -> pure unit
          Just clicked -> do
            pure unit
    SelectPrev -> pure unit

  sendWindowUpdate :: ∀ s. H.HalogenM (InnerState r) (Action r) s (Message r) Aff Unit
  sendWindowUpdate = do
    state <- H.get
    void $ Socket.request { windowUpdate: { id: state.window, params: { size: state.pagesLoaded * pageSize, attached: state.attached } } }

  initStream :: ∀ s. H.HalogenM (InnerState r) (Action r) s (Message r) Aff Unit
  initStream = do
    state <- H.get
    _ <- Socket.subscribeResponses WindowResponse state.window
    pure unit

  render :: (State r) -> H.ComponentHTML (Action r) s Aff
  render state = sdiv [ S.ui, S.container ] content
    where
    content = case state of
      Nothing -> [ loaderDiv true ]
      Just inner ->
        if length inner.elements == 0 && not inner.recvdEmpty then
          [ loaderDiv false ]
        else
          [ sdiv [ S.ui, S.basic, S.segment ]
              [ HH.table [ classes [ S.ui, S.selectable, S.single, S.line, S.very, S.compact, S.table ] ]
                  [ HH.thead_ [ HH.tr_ $ map (\r -> HH.th [ classes [ S.width r.width, S.wide ] ] [ text r.name ]) rows ]
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
                              , HH.a [ classes [ S.icon, S.item ], onClick $ Just <<< (const NextPage) ] [ sicon if inner.loading then [ S.disabled, S.notched, S.circle, S.loading ] else [ S.angle, S.right ] ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
