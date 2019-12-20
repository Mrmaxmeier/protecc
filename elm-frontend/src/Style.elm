module Style exposing (bgGrey, borderGrey, sized, textGrey)

import Element exposing (..)
import Element.Font as Font


sized : Int -> String -> Element msg
sized i s =
    el [ Font.size (modular 16 1.25 i |> round) ] <| text s


bgGrey : Color
bgGrey =
    rgb255 250 250 250


borderGrey : Color
borderGrey =
    rgba 0 0 0 0.7


textGrey : Color
textGrey =
    rgb255 54 65 73
