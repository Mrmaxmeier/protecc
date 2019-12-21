module MainComponent where

import Prelude
import CSS as CSS
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen (ClassName(..))
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
import SocketIO as SIO
import Web.Event.Event (Event)
import Web.Event.EventTarget (eventListener)
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.CloseEvent as CloseEvent
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

type Slot
  = H.Slot Query Void

data Query a
  = ChangeRoute String a
  | SocketConnect a
  | SocketDisconnect String a
  | SocketError String a

data Action
  = NoOp

type State
  = { route :: Route
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
initialState = const { route: Index }

render :: State -> H.ComponentHTML Action () Aff
render state =
  HH.div_
    [ renderMenu state
    , renderRoute state.route state
    ]

renderMenu :: State -> H.ComponentHTML Action () Aff
renderMenu state =
  div [ classes [ S.ui, S.attached, S.inverted, S.segment ] ]
    [ div [ classes [ S.ui, S.inverted, S.secondary, S.pointing, S.menu ] ]
        [ div [ classes [ S.ui, S.container ] ]
            $ map
                (\entry -> HH.a [ classes $ activeIfEqual entry state.route, HP.href entry.link ] [ HH.text entry.name ])
                menuEntries
        , div [ classes [ S.right, S.menu ] ] []
        ]
    ]
  where
  activeIfEqual entry route = if entry == routeToEntry route then [ S.active, S.item ] else [ S.item ]

handleQuery :: ∀ a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ $ _ { route = pathToRoute msg }
    pure (Just a)
  SocketConnect a -> pure $ Just a
  SocketDisconnect reason a -> pure $ Just a
  SocketError error a -> pure $ Just a

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
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

renderRoute :: Route -> State -> H.ComponentHTML Action () Aff
renderRoute Index state = div [] []

renderRoute (NotFound s) state =
  div [ classes [ S.ui, S.container ], HC.style (CSS.paddingTop $ CSS.px 20.0) ]
    [ div [ classes [ S.ui, S.placeholder, S.segment ] ]
        [ div [ classes [ S.ui, S.icon, S.header ] ]
            [ HH.i [ classes [ S.blind, S.icon ] ] []
            , HH.text "Page not found!"
            ]
        ]
    ]
