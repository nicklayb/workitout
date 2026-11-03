module ResourceTest exposing (suite)

{-| Unit tests for the `Resource` module.
-}

import Expect
import Resource exposing (Resource(..))
import Test exposing (Test, describe, test)


donut : String
donut =
    "Pink frosted donut"


errorMessage : String
errorMessage =
    "D'oh!"


notLoaded : Resource String
notLoaded =
    Resource.init


failed : Resource String
failed =
    Resource.fail errorMessage


suite : Test
suite =
    describe "Resource"
        [ test "init is not loaded" <|
            \_ -> notLoaded |> Expect.equal NotLoaded
        , test "loaded wraps the value" <|
            \_ -> Resource.loaded donut |> Expect.equal (Loaded donut)
        , test "fail wraps the error" <|
            \_ -> failed |> Expect.equal (Failed errorMessage)
        , describe "loading"
            [ test "keeps the previously loaded value" <|
                \_ -> Resource.loaded donut |> Resource.loading |> Expect.equal (Loading (Just donut))
            , test "has no value when nothing was loaded" <|
                \_ -> notLoaded |> Resource.loading |> Expect.equal (Loading Nothing)
            , test "drops the error of a failed resource" <|
                \_ -> failed |> Resource.loading |> Expect.equal (Loading Nothing)
            ]
        , describe "value"
            [ test "returns the loaded value" <|
                \_ -> Resource.loaded donut |> Resource.value |> Expect.equal (Just donut)
            , test "returns the value kept while reloading" <|
                \_ -> Resource.loaded donut |> Resource.loading |> Resource.value |> Expect.equal (Just donut)
            , test "returns nothing when not loaded" <|
                \_ -> notLoaded |> Resource.value |> Expect.equal Nothing
            , test "returns nothing when failed" <|
                \_ -> failed |> Resource.value |> Expect.equal Nothing
            ]
        ]
