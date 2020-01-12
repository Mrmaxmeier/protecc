module MainComponent where

import Prelude
import CSS as CSS
import Configuration (Configuration)
import Configuration as Config
import Dashboard as Dashboard
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML (a, div)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (mapAction)
import Routing (match)
import Routing.Match (Match, end, lit)
import SemanticUI (sdiv)
import SemanticUI as S
import Socket as Socket
import SocketIO as SocketIO
import Streams as Streams
import Util (logo)

type Slot
  = ( dashboard :: H.Slot Dashboard.Query Void Unit
    , streams :: H.Slot Streams.Query Void Unit
    )

_dashboard = SProxy :: SProxy "dashboard"

_streams = SProxy :: SProxy "streams"

data Query a
  = ChangeRoute String a

data Action
  = SocketConnect
  | SocketDisconnect String
  | SocketReconnecting Int
  | SocketMessage String
  | ConfigMessage { configuration :: Configuration }
  | Init

data SocketState
  = Disconnected
  | Connecting
  | Connected

type State
  = { route :: Route
    , socketState :: SocketState
    }

data Message
  = SocketOutput String String

type Input
  = String

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery, handleAction = handleAction, initialize = Just Init }
    }

initialState :: Input -> State
initialState path = { route: pathToRoute path, socketState: Connecting }

render :: State -> H.ComponentHTML Action Slot Aff
render state =
  HH.div_
    [ renderMenu state
    , renderRoute state.route state
    ]

handleQuery :: ∀ a. Query a -> H.HalogenM State Action Slot Message Aff (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ $ _ { route = pathToRoute msg }
    pure $ Just a

handleAction :: Action -> H.HalogenM State Action Slot Message Aff Unit
handleAction = case _ of
  Init -> init
  SocketConnect -> do
    H.modify_ $ _ { socketState = Connected }
    id <- Config.init
    void $ Socket.subscribeResponses ConfigMessage id
  SocketDisconnect _ -> H.modify_ $ _ { socketState = Disconnected }
  SocketReconnecting _ -> H.modify_ $ _ { socketState = Connecting }
  ConfigMessage config -> do
    logo config
    Config.set config.configuration
  _ -> pure unit

init :: H.HalogenM State Action Slot Message Aff Unit
init = do
  _ <- mapAction (const SocketConnect) $ Socket.subscribe SocketIO.connectSource
  _ <- mapAction SocketReconnecting $ Socket.subscribe SocketIO.reconnectingSource
  _ <- mapAction SocketDisconnect $ Socket.subscribe SocketIO.disconnectSource
  pure unit

-- Routing
data Route
  = Index
  | Streams
  | NotFound String

pathToRoute :: String -> Route
pathToRoute path = case match routeMatch path of
  Left _ -> NotFound path
  Right route -> route

routeMatch :: Match Route
routeMatch =
  oneOf
    [ Index <$ end
    , Streams <$ lit "streams" <* end
    ]

type MenuEntry
  = { name :: String
    , link :: String
    }

dashboardEntry :: MenuEntry
dashboardEntry = { name: "Dashboard", link: "#" }

streamsEntry :: MenuEntry
streamsEntry = { name: "Streams", link: "#streams" }

menuEntries :: Array MenuEntry
menuEntries = [ dashboardEntry, streamsEntry ]

routeToEntry :: Route -> Maybe MenuEntry
routeToEntry = case _ of
  Index -> Just dashboardEntry
  Streams -> Just streamsEntry
  _ -> Nothing

-- rendering
renderRoute :: Route -> State -> H.ComponentHTML Action Slot Aff
renderRoute Index state = HH.slot _dashboard unit Dashboard.component unit absurd

renderRoute Streams state = HH.slot _streams unit Streams.component unit absurd

renderRoute (NotFound s) state =
  div [ classes [ S.ui, S.container ] ]
    [ div [ classes [ S.ui, S.placeholder, S.segment ] ]
        [ div [ classes [ S.ui, S.icon, S.header ] ]
            [ HH.i [ classes [ S.blind, S.icon ] ] []
            , HH.text "Page not found!"
            ]
        ]
    ]

renderMenu :: State -> H.ComponentHTML Action Slot Aff
renderMenu state =
  div [ classes [ S.ui, S.attached, S.inverted, S.segment ], HC.style (CSS.marginBottom $ CSS.px 15.0) ]
    [ sdiv [ S.ui, S.inverted, S.secondary, S.pointing, S.menu ]
        [ sdiv [ S.ui, S.container ]
            $ map
                (\entry -> a [ classes $ activeIfEqual entry state.route, HP.href entry.link ] [ HH.text entry.name ])
                menuEntries
        , sdiv [ S.right, S.menu ] [ renderSocketState state.socketState ]
        ]
    ]
  where
  activeIfEqual entry route = if Just entry == routeToEntry route then [ S.active, S.item ] else [ S.item ]

renderSocketState :: SocketState -> H.ComponentHTML Action Slot Aff
renderSocketState state = sdiv [ S.ui, S.icon, S.item ] [ HH.i [ classes $ [ S.icon ] <> icon ] [] ]
  where
  icon = case state of
    Connecting -> [ S.yellow, S.sync ]
    Connected -> [ S.green, S.wifi ]
    Disconnected -> [ S.red, S.ban ]
