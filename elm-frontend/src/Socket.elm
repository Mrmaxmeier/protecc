port module Socket exposing (Msg(..), State, init, subscriptions, update, viewErrors, viewMenuWidget)

import Html exposing (..)
import Html.Attributes exposing (..)


type Msg
    = Status String
    | Error String


type alias State =
    { status : String
    , errors : List String
    }


init : State
init =
    { status = "connecting", errors = [] }


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        Status s ->
            ( { model | status = s }, Cmd.none )

        Error s ->
            ( { model | errors = s :: model.errors }, Cmd.none )


viewMenuWidget : State -> List (Html Msg)
viewMenuWidget state =
    let
        icon =
            case state.status of
                "disconnected" ->
                    "red ban"

                "connected" ->
                    "green wifi"

                "connecting" ->
                    "yellow sync"

                _ ->
                    "purple question"
    in
    [ div [ class "ui icon item" ] [ i [ class <| icon ++ " icon" ] [] ] ]


viewErrors : State -> Html Msg
viewErrors state =
    div [] []



--<| List.map (\error -> div [ class "ui negative message" ] [ i [ class "close icon" ] [], text error ]) state.errors


port socketStatus : (String -> msg) -> Sub msg


port socketError : (String -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ socketStatus Status
        , socketError Error
        ]
