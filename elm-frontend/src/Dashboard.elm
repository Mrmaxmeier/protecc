port module Dashboard exposing (Msg, State, init, onSocketMsg, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import Socket


type alias Counters =
    { packets : Int
    , streams : Int
    , reassemblyErrors : Int
    , packetsUnhandled : Int
    , packetsMalformed : Int
    , packetsWithoutStream : Int
    , packetsTcp : Int
    , streamsCompleted : Int
    , pcapBlocks : Int
    , dbServices : Int
    , dbStatServicePromotion : Int
    }


type alias State =
    { counters : Maybe Counters
    }


init : State
init =
    { counters = Maybe.Nothing }


type Msg
    = CounterUpdate Counters


port updateCounters : () -> Cmd msg


port counterUpdate : (Counters -> msg) -> Sub msg


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ counterUpdate CounterUpdate
        ]


view : State -> Html Msg
view state =
    div [ style "padding-top" "20px" ]
        [ div [ class "ui three statistics" ] <|
            List.map
                (\f -> f state.counters)
                [ viewStatistic "Streams" .streams
                , viewStatistic "TCP Packets" .packetsTcp
                , viewStatistic "Packets in total" .packets
                ]
        ]


viewStatistic : String -> (Counters -> Int) -> Maybe Counters -> Html Msg
viewStatistic label value counters =
    div [ class "statistic" ]
        [ div [ class "value" ] [ Maybe.withDefault (text "?") <| Maybe.map (\c -> value c |> String.fromInt |> text) counters ]
        , div [ class "label" ] [ text label ]
        ]


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        CounterUpdate counters ->
            ( { state | counters = Just counters }, Cmd.none )


onSocketMsg : Socket.Msg -> Cmd Msg
onSocketMsg msg =
    case msg of
        Socket.Status "connected" ->
            updateCounters ()

        _ ->
            Cmd.none
