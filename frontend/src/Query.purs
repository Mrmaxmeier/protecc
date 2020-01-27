module Query where

import Prelude
import CSS as CSS
import CSS.TextAlign (center, textAlign)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (drop, length, take)
import Data.BigInt (toNumber, toString)
import Data.Int (ceil)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Dropdown (Message(..))
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Effect.Console (log)
import Halogen (liftEffect, query)
import Halogen as H
import Halogen.HTML (div, div_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, classes, disabled, href, name, placeholder, tabIndex, type_, value)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import Keyevent as Keyevent
import Math (abs)
import Numeral (formatBytes, formatPercent)
import Numeral as Numeral
import SemanticUI (sdiv, sicon)
import SemanticUI as S
import Socket (RequestId, errorId)
import Socket as Socket
import SocketIO (Socket)
import SocketIO as SIO
import StarlarkEditor (Editor)
import StarlarkEditor as Editor
import Streams as Streams
import Util (Id, Rec, WMaybe, dec, diff, fromString, inc, logo, logs, mwhen, prettifyJson, prettyShow, rec, tryFromString, unrec, wmaybe)

maxPageFetch :: Int
maxPageFetch = 10

type Slot
  = ( editor :: H.Slot Editor.Query Editor.Message Unit
    )

_editor = SProxy :: SProxy "editor"

data Action
  = SocketConnect
  | UpperChange String
  | LowerChange String
  | PageSizeChange Int
  | QueryResponse RequestId { starlarkScan :: { error :: Maybe Error, scanProgress :: Id, rangeExhausted :: Boolean, boundLow :: Id, boundHigh :: Id, scanResults :: Array Result } }
  | NextPage
  | PrevPage
  | Execute
  | ToggleDirection
  | TogglePause
  | ContinueFetching
  | Init
  | EditorProxy Editor.Message

type Result
  = Streams.Stream

data QueryRunState
  = Running
  | Idle
  | Done
  | Errored Error

isError :: QueryRunState -> Boolean
isError = case _ of
  Errored _ -> true
  _ -> false

derive instance queryRunStateEq :: Eq QueryRunState

type QueryState
  = { progress ::
      Maybe
        { lastScanned :: Id
        , from :: Id
        , to :: Id
        }
    , results :: Array Result
    , program :: String
    , pastToFuture :: Boolean
    , pageSize :: Int
    , lastRequest :: RequestId
    , paused :: Boolean
    , maxPages :: Int
    , runState :: QueryRunState
    , page :: Int
    }

type Error
  = String

type State
  = { query :: Maybe QueryState
    , error :: Maybe Error
    , upper :: String
    , lower :: String
    , pageSize :: Int
    , pastToFuture :: Boolean
    }

component :: ∀ q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ = { query: Nothing, upper: "", lower: "", pageSize: 20, pastToFuture: false, error: Nothing }

  handleAction :: Action -> H.HalogenM State Action Slot o Aff Unit
  handleAction = case _ of
    Init -> do
      _ <- Socket.subscribeConnect SocketConnect
      _ <- Keyevent.subscribe 39 NextPage
      _ <- Keyevent.subscribe 37 PrevPage
      pure unit
    SocketConnect -> do
      pure unit
    LowerChange s -> H.modify_ $ _ { lower = s }
    UpperChange s -> H.modify_ $ _ { upper = s }
    PageSizeChange i -> H.modify_ $ _ { pageSize = i }
    EditorProxy msg -> case msg of
      Editor.Execute -> handleAction Execute
    NextPage -> do
      state <- H.get
      case state.query of
        Nothing -> error "Trying to go to the next page, but there is currently no query!"
        Just query -> do
          when (pageN query > query.page + 1)
            $ H.modify_
            $ _ { query = Just query { page = query.page + 1 } }
    PrevPage -> do
      state <- H.get
      case state.query of
        Nothing -> error "Trying to go to the next page, but there is currently no query!"
        Just query -> do
          when (0 < query.page)
            $ H.modify_
            $ _ { query = Just query { page = query.page - 1 } }
    Execute -> do
      code' <- H.query _editor unit $ H.request Editor.GetValue
      let
        code = fromMaybe "" code'
      _ <- H.query _editor unit $ H.tell $ Editor.SetError ""
      state <- H.get
      id <- Socket.request { starlarkScan: { code: code, boundLow: tryFromString state.lower, boundHigh: tryFromString state.upper, reverse: state.pastToFuture, windowSize: state.pageSize } }
      H.modify_ $ _ { query = Just ({ progress: Nothing, results: [], program: code, pastToFuture: state.pastToFuture, pageSize: state.pageSize, lastRequest: id, paused: false, runState: Running, maxPages: maxPageFetch, page: 0 } :: QueryState) }
      void $ Socket.subscribeResponse (QueryResponse id) id
    ToggleDirection -> H.modify_ $ \state -> state { pastToFuture = not state.pastToFuture }
    TogglePause -> do
      state <- H.get
      maybe
        (error "Trying to toggle pause, but no query is currently being executed")
        ( \query -> do
            if query.paused then do
              handleAction ContinueFetching
            else
              H.modify_ $ _ { query = Just query { paused = true } }
        )
        state.query
    ContinueFetching -> do
      state <- H.get
      maybe
        (error "Trying to continue fetching, but no query is currently being executed")
        ( \query ->
            maybe (error "Trying to continue fetching, but no progress has been made")
              ( \progress -> do
                  let
                    lower = if query.pastToFuture then inc progress.lastScanned else progress.to

                    upper = if query.pastToFuture then progress.to else dec progress.lastScanned

                    maxPages = if length query.results >= query.pageSize * query.maxPages then query.maxPages + maxPageFetch else query.maxPages

                    windowSize = min query.pageSize $ maxPages * query.pageSize - length query.results
                  when (query.runState == Running || query.runState == Idle) do
                    id <- Socket.request { starlarkScan: { code: query.program, boundLow: lower, boundHigh: upper, reverse: query.pastToFuture, windowSize: windowSize } }
                    H.modify_ $ _ { query = Just query { runState = Running, paused = false, lastRequest = id, maxPages = maxPages } }
                    void $ Socket.subscribeResponse (QueryResponse id) id
              )
              query.progress
        )
        state.query
    QueryResponse id { starlarkScan: v } -> do
      state <- H.get
      case state.query of
        Just query
          | id == query.lastRequest && query.runState == Running -> do
            let
              progress =
                maybe
                  { lastScanned: v.scanProgress, from: if query.pastToFuture then v.boundLow else v.boundHigh, to: if query.pastToFuture then v.boundHigh else v.boundLow }
                  (_ { lastScanned = v.scanProgress })
                  query.progress

              runState = case v.error of
                Nothing -> if v.rangeExhausted then Done else Running
                Just error -> Errored error

              query' = query { progress = Just progress, results = query.results <> v.scanResults, runState = runState }
            state <- H.modify_ $ _ { query = Just $ query' }
            case v.error of
              Nothing -> pure unit
              Just error -> void $ H.query _editor unit $ H.tell $ Editor.SetError error
            let
              paused =
                if length query'.results >= query'.pageSize * query'.maxPages then do
                  true
                else
                  query'.paused
            if runState == Running && paused then
              H.modify_ $ _ { query = Just query' { paused = true, runState = Idle } }
            else
              handleAction ContinueFetching
        _ -> do
          error "Received message that does not belong to the current query"

  renderProgressBar :: QueryState -> Array (H.ComponentHTML Action Slot Aff)
  renderProgressBar query =
    [ div [ classes [ S.field ], HC.style $ CSS.width $ CSS.pct 100.0 ]
        [ div [ classes $ [ S.ui, S.progress, color ] <> mwhen inProgress [ S.active ] ]
            [ div
                [ classes [ S.bar ]
                , HC.style do
                    CSS.width $ CSS.pct if isError query.runState then 100.0 else percentage
                    CSS.minWidth $ CSS.pct 1.0
                ]
                []
            , div
                [ classes [ S.progress ]
                , HC.style do
                    CSS.position CSS.absolute
                    CSS.top $ CSS.px 0.0
                    CSS.width $ CSS.pct 100.0
                    textAlign center
                ]
                [ text progressText ]
            ]
        ]
    , HH.button [ classes $ [ S.ui, S.mini, S.icon, S.button ] <> mwhen isDone [ S.disabled ], disabled isDone, onClick $ Just <<< (const TogglePause) ] [ sicon if query.paused then [ S.play ] else [ S.pause ] ]
    ]
    where
    progressText = case query.progress of
      Nothing -> "Initializing query..."
      Just progress -> formatPercent percentage <> "%"

    percentage = case query.progress of
      Nothing -> 0.0
      Just progress -> (toNumber $ diff progress.lastScanned progress.from) / (toNumber $ diff progress.to progress.from) * 100.0

    color = case query.runState of
      Running -> S.green
      Done -> S.green
      Errored _ -> S.red
      Idle -> S.orange

    inProgress = case query.runState of
      Running -> true
      _ -> false

    isDone = query.runState /= Running && query.runState /= Idle

  showCell :: ∀ a. Show a => a -> H.ComponentHTML Action Slot Aff
  showCell = stringCell <<< show

  stringCell :: String -> H.ComponentHTML Action Slot Aff
  stringCell s = HH.td_ [ HH.text s ]

  renderRow :: Streams.Stream -> Array (H.ComponentHTML Action Slot Aff)
  renderRow stream =
    [ HH.td_ [ HH.a [ href $ "#stream/" <> show stream.id ] [ text $ show stream.id ] ]
    , showCell stream.client
    , showCell stream.server
    ]

  pageN :: QueryState -> Int
  pageN query = ceil $ (Int.toNumber $ length query.results) / Int.toNumber query.pageSize

  renderTable :: QueryState -> Array (H.ComponentHTML Action Slot Aff)
  renderTable query =
    [ sdiv [ S.ui, S.basic, S.segment ]
        [ HH.table [ classes [ S.ui, S.table ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ text "Id" ]
                    , HH.th_ [ text "Client" ]
                    , HH.th_ [ text "Server" ]
                    ]
                ]
            , HK.tbody_
                $ ( \r ->
                      Tuple (show r.id) $ HH.tr_ $ renderRow r
                  )
                <$> (take query.pageSize $ drop (query.pageSize * query.page) query.results)
            ]
        , sdiv [ S.ui, S.right, S.rail ]
            [ div [ classes [ S.ui, S.pagination, S.menu ], HC.style $ CSS.marginTop $ CSS.em 1.0 ]
                [ HH.a [ classes $ [ S.icon, S.item ] <> mwhen (query.page == 0) [ S.disabled ], onClick $ Just <<< (const PrevPage) ] [ sicon [ S.angle, S.left ] ]
                , sdiv [ S.item, S.label ] [ text $ show (query.page + 1) <> "/" <> show (pageN query) ]
                , HH.a [ classes $ [ S.icon, S.item ] <> mwhen (pageN query <= query.page + 1) [ S.disabled ], onClick $ Just <<< (const NextPage) ] [ sicon [ S.angle, S.right ] ]
                ]
            ]
        ]
    ]

  render :: State -> H.ComponentHTML Action Slot Aff
  render state =
    sdiv [ S.ui, S.container ]
      $ [ HH.slot _editor unit Editor.component unit $ Just <<< EditorProxy
        , sdiv [ S.ui, S.divider ] []
        , HH.form [ classes [ S.ui, S.form ] ]
            [ sdiv [ S.inline, S.fields ]
                $ [ div [ HC.style $ CSS.marginRight $ CSS.em 0.5 ] [ text "Range:" ]
                  , div [ classes [ S.field ], HC.style $ CSS.paddingRight $ CSS.px 0.0 ] [ HH.input [ type_ InputText, placeholder "Latest", HC.style $ CSS.width $ CSS.em 6.0, value state.lower, onValueChange $ Just <<< UpperChange ] ]
                  , div
                      [ classes [ S.ui, S.icon, S.button ]
                      , HC.style do
                          CSS.marginRight $ CSS.px 5.0
                          CSS.marginLeft $ CSS.px 5.0
                      , onClick $ Just <<< (const ToggleDirection)
                      ]
                      [ sicon $ [ S.long, S.alternate, S.arrow ] <> if state.pastToFuture then [ S.left ] else [ S.right ] ]
                  , sdiv [ S.field ] [ HH.input [ type_ InputText, placeholder "Earliest", HC.style $ CSS.width $ CSS.em 6.0, value state.lower, onValueChange $ Just <<< LowerChange ] ]
                  , div [ HC.style $ CSS.marginRight $ CSS.em 0.5 ] [ text "Page\xa0size:" ]
                  , sdiv [ S.field ] [ HH.input [ type_ InputText, HC.style $ CSS.width $ CSS.em 6.0, value $ show state.pageSize, onValueChange $ map PageSizeChange <<< Int.fromString ] ]
                  , div [ classes [ S.ui, S.button, S.green ], HC.style $ CSS.marginRight $ CSS.em 3.0, onClick $ Just <<< (const Execute) ] [ text "Execute" ]
                  ]
                <> maybe [] renderProgressBar state.query
            ]
        ]
      <> maybe [] renderTable state.query
