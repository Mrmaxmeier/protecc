module MainComponent where

import Prelude
import CSS (Predicate(..))
import CSS as CSS
import Configuration as Config
import ConfigurationTypes (Configuration, ConfigurationMessage)
import Dashboard as Dashboard
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Identity (Identity(..))
import Data.Int as Int
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
import Routing.Match (Match, end, int, lit, str)
import SemanticUI (sdiv)
import SemanticUI as S
import StarlarkEditor as Editor
import Socket as Socket
import SocketIO as SocketIO
import Streams as Streams
import Util (fromString, logo, id, Id, matchMaybe)
import Stream as Stream
import Query as Query

type Slot
  = ( dashboard :: H.Slot Dashboard.Query Void Unit
    , streams :: H.Slot Streams.Query Void Unit
    , config :: H.Slot Streams.Query Void Unit
    , query :: H.Slot Identity Void Unit
    , stream :: H.Slot Identity Void Unit
    )

_dashboard = SProxy :: SProxy "dashboard"

_streams = SProxy :: SProxy "streams"

_config = SProxy :: SProxy "config"

_stream = SProxy :: SProxy "stream"

_query = SProxy :: SProxy "query"

data Query a
  = ChangeRoute String a

data Action
  = SocketConnect
  | SocketDisconnect String
  | SocketReconnecting Int
  | SocketMessage String
  | ConfigMessage { configuration :: ConfigurationMessage }
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
  ConfigMessage config -> Config.set config.configuration
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
  | Streams (Maybe Int) (Maybe Id)
  | Stream Id
  | Configuration
  | Query
  | NotFound String

pathToRoute :: String -> Route
pathToRoute path = case match routeMatch path of
  Left _ -> NotFound path
  Right route -> route

routeMatch :: Match Route
routeMatch =
  oneOf
    [ Index <$ end
    , Streams Nothing Nothing <$ lit "streams" <* end
    , Streams Nothing <$> (lit "streams" *> lit "tag" *> matchMaybe id <* end)
    , (\p -> Streams p Nothing) <$> (lit "streams" *> lit "service" *> matchMaybe int <* end)
    , Streams <$> (lit "streams" *> matchMaybe int) <*> (matchMaybe id <* end)
    , Stream <$> (lit "stream" *> id <* end)
    , Configuration <$ lit "config" <* end
    , Query <$ lit "query" <* end
    ]

type MenuEntry
  = { name :: String
    , link :: String
    }

dashboardEntry :: MenuEntry
dashboardEntry = { name: "Dashboard", link: "#" }

streamsEntry :: MenuEntry
streamsEntry = { name: "Streams", link: "#streams" }

configEntry :: MenuEntry
configEntry = { name: "Config", link: "#config" }

queryEntry :: MenuEntry
queryEntry = { name: "Query", link: "#query" }

menuEntries :: Array MenuEntry
menuEntries = [ dashboardEntry, streamsEntry, configEntry, queryEntry ]

routeToEntry :: Route -> Maybe MenuEntry
routeToEntry = case _ of
  Index -> Just dashboardEntry
  Streams _ _ -> Just streamsEntry
  Configuration -> Just configEntry
  Query -> Just queryEntry
  _ -> Nothing

-- rendering
renderRoute :: Route -> State -> H.ComponentHTML Action Slot Aff
renderRoute Index state = HH.slot _dashboard unit Dashboard.component unit absurd

renderRoute (Streams port tag) state = HH.slot _streams unit Streams.component { port: port, tag: tag } absurd

renderRoute Configuration state = HH.slot _config unit Config.component unit absurd

renderRoute Query state = HH.slot _query unit Query.component unit absurd

renderRoute (Stream id) state = sdiv [ S.ui, S.container ] [ HH.slot _stream unit Stream.component id absurd ]

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
