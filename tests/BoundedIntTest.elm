module BoundedIntTest exposing (suite)

{-| Unit tests for the `BoundedInt` module: values are always capped within
their bounds.
-}

import BoundedInt exposing (Bounds)
import Expect
import Test exposing (Test, describe, test)


donutBounds : Bounds
donutBounds =
    { minimum = 1, maximum = 12 }


suite : Test
suite =
    describe "BoundedInt"
        [ describe "init"
            [ test "keeps a value within the bounds" <|
                \_ -> BoundedInt.init donutBounds 7 |> BoundedInt.value |> Expect.equal 7
            , test "keeps the bounds themselves" <|
                \_ ->
                    [ 1, 12 ]
                        |> List.map (BoundedInt.init donutBounds >> BoundedInt.value)
                        |> Expect.equal [ 1, 12 ]
            , test "caps a value above the maximum" <|
                \_ -> BoundedInt.init donutBounds 742 |> BoundedInt.value |> Expect.equal 12
            , test "caps a value below the minimum" <|
                \_ -> BoundedInt.init donutBounds -5 |> BoundedInt.value |> Expect.equal 1
            ]
        , describe "set"
            [ test "replaces the value" <|
                \_ -> BoundedInt.init donutBounds 3 |> BoundedInt.set 9 |> BoundedInt.value |> Expect.equal 9
            , test "caps the new value within the original bounds" <|
                \_ -> BoundedInt.init donutBounds 3 |> BoundedInt.set 100 |> BoundedInt.value |> Expect.equal 12
            ]
        , test "bounds returns the bounds it was built with" <|
            \_ -> BoundedInt.init donutBounds 3 |> BoundedInt.bounds |> Expect.equal donutBounds
        ]
