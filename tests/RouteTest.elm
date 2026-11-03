module RouteTest exposing (suite)

{-| Unit tests for the `Route` module: URL serialization round trips and
change detection.
-}

import Expect
import Plan exposing (Day(..))
import Route exposing (Route(..))
import Test exposing (Test, describe, test)
import Url exposing (Url)


baseUrl : String
baseUrl =
    "https://springfield.example"


parseRoute : String -> Maybe Route
parseRoute path =
    Url.fromString (baseUrl ++ path) |> Maybe.andThen Route.fromUrl


{-| Parses a URL fixture, falling back to the base URL so tests stay total.
-}
urlFrom : String -> Url
urlFrom path =
    Url.fromString (baseUrl ++ path)
        |> Maybe.withDefault
            { protocol = Url.Https, host = "", port_ = Nothing, path = "", query = Nothing, fragment = Nothing }


routes : List Route
routes =
    [ Home
    , RunPlan Nothing Nothing
    , RunPlan (Just "plans/duff.yml") Nothing
    , RunPlan Nothing (Just Wednesday)
    , RunPlan (Just "springfield/moe's tavern & grill.yml") (Just Sunday)
    ]


suite : Test
suite =
    describe "Route"
        [ describe "routeToString"
            [ test "home is the root" <|
                \_ -> Route.routeToString Home |> Expect.equal "/"
            , test "run plan without parameters" <|
                \_ -> Route.routeToString (RunPlan Nothing Nothing) |> Expect.equal "/plans/run"
            , test "run plan with day and path parameters" <|
                \_ ->
                    Route.routeToString (RunPlan (Just "duff.yml") (Just Monday))
                        |> Expect.equal "/plans/run?day=monday&path=duff.yml"
            ]
        , describe "fromUrl"
            [ test "parses back every serialized route" <|
                \_ ->
                    routes
                        |> List.map (Route.routeToString >> parseRoute)
                        |> Expect.equal (List.map Just routes)
            , test "ignores an unknown day" <|
                \_ -> parseRoute "/plans/run?day=krustyday" |> Expect.equal (Just (RunPlan Nothing Nothing))
            , test "rejects unknown paths" <|
                \_ -> parseRoute "/kwik-e-mart" |> Expect.equal Nothing
            ]
        , describe "urlChanged"
            [ test "is false for identical URLs" <|
                \_ -> Route.urlChanged (urlFrom "/plans/run?day=monday") (urlFrom "/plans/run?day=monday") |> Expect.equal False
            , test "treats a missing and an empty query as equal" <|
                \_ -> Route.urlChanged (urlFrom "/plans/run") (urlFrom "/plans/run?") |> Expect.equal False
            , test "is true when the path changes" <|
                \_ -> Route.urlChanged (urlFrom "/") (urlFrom "/plans/run") |> Expect.equal True
            , test "is true when the query changes" <|
                \_ -> Route.urlChanged (urlFrom "/plans/run?day=monday") (urlFrom "/plans/run?day=friday") |> Expect.equal True
            , test "is true when the fragment changes" <|
                \_ -> Route.urlChanged (urlFrom "/#homer") (urlFrom "/#marge") |> Expect.equal True
            ]
        ]
