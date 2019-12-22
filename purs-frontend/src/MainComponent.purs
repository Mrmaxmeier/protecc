module MainComponent where

import Prelude

import CSS as CSS
import Dashboard as Dashboard
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (a, div)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Routing (match)
import Routing.Match (Match, end)
import SemanticUI as S

type Slot
  = ( dashboard :: H.Slot Dashboard.Query Void Unit
  )

_dashboard = SProxy :: SProxy "dashboard"

data Query a
  = ChangeRoute String a
  | SocketConnect a
  | SocketDisconnect String a
  | SocketError String a
  | SocketReconnecting Int a

data Action
  = NoOp

data SocketState 
  = Disconnected
  | Connecting
  | Connected

type State
  = { route :: Route
    , socketState :: SocketState
    }

data Message
  = SocketOutput String

component :: ∀ i. H.Component HH.HTML Query i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery, handleAction = handleAction }
    }

initialState :: ∀ i. i -> State
initialState = const { route: Index, socketState: Connecting }

render :: State -> H.ComponentHTML Action Slot Aff
render state =
  HH.div_
    [ renderMenu state
    , renderRoute state.route state
    ]

renderMenu :: State -> H.ComponentHTML Action Slot Aff
renderMenu state =
  div [ classes [ S.ui, S.attached, S.inverted, S.segment ] ]
    [ div [ classes [ S.ui, S.inverted, S.secondary, S.pointing, S.menu ] ]
        [ div [ classes [ S.ui, S.container ] ]
            $ map
                (\entry -> a [ classes $ activeIfEqual entry state.route, HP.href entry.link ] [ HH.text entry.name ])
                menuEntries
        , div [ classes [ S.right, S.menu ] ] [renderSocketState state.socketState]
        ]
    ]
  where
  activeIfEqual entry route = if entry == routeToEntry route then [ S.active, S.item ] else [ S.item ]

renderSocketState :: SocketState -> H.ComponentHTML Action Slot Aff
renderSocketState state = 
  div [classes [S.ui, S.icon, S.item]] [ HH.i [classes $ [S.icon] <> icon] [] ]
  where
  icon = case state of 
    Connecting -> [S.yellow, S.sync]
    Connected -> [S.green, S.wifi]
    Disconnected -> [S.red, S.ban]

handleQuery :: ∀ a. Query a -> H.HalogenM State Action Slot Message Aff (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ $ _ { route = pathToRoute msg }
    pure $ Just a
  SocketConnect a -> do
    H.modify_ $ _ { socketState = Connected }
    pure $ Just a
  SocketDisconnect reason a -> do
    H.modify_ $ _ { socketState = Disconnected }
    pure $ Just a
  SocketError error a -> pure $ Just a
  SocketReconnecting attempt a -> do
    H.modify_ $ _ { socketState = Connecting }
    pure $ Just a

handleAction :: Action -> H.HalogenM State Action Slot Message Aff Unit
handleAction = case _ of
  NoOp -> pure unit

-- Routing
data Route
  = Index
  | NotFound String

instance showRoute :: Show Route where
  show Index = "Index"
  show (NotFound s) = "Not Found: " <> s

pathToRoute :: String -> Route
pathToRoute path = case match routeMatch path of
  Left _ -> NotFound path
  Right route -> route

routeMatch :: Match Route
routeMatch =
  oneOf
    [ pure Index
    ]
    <* end

type MenuEntry
  = { name :: String
    , link :: String
    }

dashboardEntry :: MenuEntry
dashboardEntry = { name: "Dashboard", link: "#" }

memesEntry :: MenuEntry
memesEntry = { name: "Memes", link: "#memes" }

menuEntries :: Array MenuEntry
menuEntries = [ dashboardEntry, memesEntry ]

routeToEntry :: Route -> MenuEntry
routeToEntry Index = dashboardEntry

routeToEntry (NotFound _) = memesEntry

renderRoute :: Route -> State -> H.ComponentHTML Action Slot Aff
renderRoute Index state = HH.slot _dashboard unit Dashboard.component unit absurd

renderRoute (NotFound s) state =
  div [ classes [ S.ui, S.container ], HC.style (CSS.paddingTop $ CSS.px 20.0) ]
    [ div [ classes [ S.ui, S.placeholder, S.segment ] ]
        [ div [ classes [ S.ui, S.icon, S.header ] ]
            [ HH.i [ classes [ S.blind, S.icon ] ] []
            , HH.text "Page not found!"
            ]
        ]
    ]