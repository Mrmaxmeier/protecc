module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Routing
import Socket
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Routing.Route
    , ws : Socket.State
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (Routing.parse url) Socket.init, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SocketWrap Socket.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Routing.parse url }
            , Cmd.none
            )

        SocketWrap socketMsg ->
            let
                ( newWs, cmds ) =
                    Socket.update socketMsg model.ws
            in
            ( { model | ws = newWs }, Cmd.map SocketWrap cmds )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SocketWrap <| Socket.subscriptions model.ws



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.route of
                Routing.NotFound path ->
                    view404 path

                Routing.Index ->
                    viewIndex
    in
    { title = Routing.title model.route
    , body =
        [ viewMenu model
        , Html.map SocketWrap <| Socket.viewErrors model.ws
        , body model
        ]
    }


viewMenu : Model -> Html Msg
viewMenu model =
    let
        links =
            [ { text = "Dashboard", link = "#", route = "index" }
            , { text = "Memes", link = "#memes", route = "notfound" }
            ]

        activeIfEqual r1 r2 =
            if r1 == Routing.name r2 then
                "active item"

            else
                "item"
    in
    div [ class "ui attached inverted segment" ]
        [ div [ class "ui inverted secondary pointing menu" ]
            [ div [ class "ui container" ] <|
                List.map
                    (\l -> a [ class <| activeIfEqual l.route model.route, href l.link ] [ text l.text ])
                    links
            , div [ class "right menu" ] (List.map (Html.map SocketWrap) <| Socket.viewMenuWidget model.ws)
            ]
        ]


view404 : String -> Model -> Html Msg
view404 path _ =
    div [ class "ui container", style "padding-top" "20px" ]
        [ div [ class "ui placeholder segment" ]
            [ div [ class "ui icon header" ]
                [ i [ class "blind icon" ] []
                , text "Page not found"
                ]
            ]
        ]


viewIndex : Model -> Html Msg
viewIndex model =
    div [ class "ui container" ] []
