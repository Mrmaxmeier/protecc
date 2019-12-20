module Routing exposing (Route(..), name, parse, title)

import Maybe
import Url
import Url.Parser exposing (Parser, oneOf, top)


type Route
    = Index
    | NotFound String


parse : Url.Url -> Route
parse url =
    fragToUrl url
        |> Url.Parser.parse parser
        |> Maybe.withDefault (NotFound url.path)


name : Route -> String
name route =
    case route of
        Index ->
            "index"

        NotFound _ ->
            "notfound"


parser : Parser (Route -> a) a
parser =
    oneOf [ Url.Parser.map Index top ]


fragToUrl : Url.Url -> Url.Url
fragToUrl url =
    { url
        | path = url.fragment |> Maybe.withDefault ""
    }


title : Route -> String
title route =
    case route of
        NotFound _ ->
            "Page not found"

        Index ->
            "AD Dashboard"
