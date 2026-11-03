module List.ExtraTest exposing (suite)

{-| Unit tests for the `List.Extra` module.
-}

import Expect
import List.Extra
import Test exposing (Test, describe, test)


children : List String
children =
    [ "Lisa", "Maggie" ]


suite : Test
suite =
    describe "List.Extra"
        [ describe "prependIf"
            [ test "prepends when the condition holds" <|
                \_ -> List.Extra.prependIf True "Bart" children |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
            , test "leaves the list untouched otherwise" <|
                \_ -> List.Extra.prependIf False "Bart" children |> Expect.equal children
            ]
        , describe "prependIfElse"
            [ test "prepends the first value when the condition holds" <|
                \_ -> List.Extra.prependIfElse True "Bart" "Milhouse" children |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
            , test "prepends the second value otherwise" <|
                \_ -> List.Extra.prependIfElse False "Bart" "Milhouse" children |> Expect.equal [ "Milhouse", "Lisa", "Maggie" ]
            ]
        ]
