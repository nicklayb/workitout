module Route exposing (Route(..), fromUrl, routeToString, urlChanged)

import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query


type Route
    = Home
    | RunPlan (Maybe String)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map RunPlan (s "plans" </> s "run" <?> Query.string "path")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString page =
    let
        ( pieces, query ) =
            case page of
                Home ->
                    ( [], [] )

                RunPlan maybePath ->
                    let
                        queryParams =
                            case maybePath of
                                Just path ->
                                    [ UrlBuilder.string "path" path ]

                                _ ->
                                    []
                    in
                    ( [ "plans", "run" ], queryParams )
    in
    "/" ++ UrlBuilder.relative pieces query


urlChanged : Url -> Url -> Bool
urlChanged first second =
    List.any identity
        [ first.host /= second.host
        , first.path /= second.path
        , not <| maybeEquals first.query second.query
        , not <| maybeEquals first.fragment second.fragment
        ]


maybeEquals : Maybe String -> Maybe String -> Bool
maybeEquals first second =
    Maybe.withDefault "" first == Maybe.withDefault "" second
