module ZiplistTest exposing (suite)

{-| Unit tests for the `Ziplist` module.
-}

import Expect
import Test exposing (Test, describe, test)
import Ziplist exposing (Position(..))


simpsons : List String
simpsons =
    [ "Homer", "Marge", "Bart" ]


simpsonsZiplist : Ziplist.Ziplist String
simpsonsZiplist =
    Ziplist.init simpsons


emptyZiplist : Ziplist.Ziplist String
emptyZiplist =
    Ziplist.init []


positionsOf : Ziplist.Ziplist String -> List ( Position, String )
positionsOf =
    Ziplist.map Tuple.pair


suite : Test
suite =
    describe "Ziplist"
        [ describe "init"
            [ test "an empty list gives an empty ziplist" <|
                \_ -> emptyZiplist |> Expect.equal Ziplist.Empty
            , test "focuses the head of the list" <|
                \_ -> Ziplist.current simpsonsZiplist |> Expect.equal (Just "Homer")
            , test "a fresh ziplist is reset" <|
                \_ -> Ziplist.isReset simpsonsZiplist |> Expect.equal True
            ]
        , describe "current"
            [ test "an empty ziplist has no current item" <|
                \_ -> Ziplist.current emptyZiplist |> Expect.equal Nothing
            ]
        , describe "toList"
            [ test "an empty ziplist gives an empty list" <|
                \_ -> Ziplist.toList emptyZiplist |> Expect.equal []
            , test "keeps the original order wherever the focus is" <|
                \_ -> simpsonsZiplist |> Ziplist.next |> Ziplist.next |> Ziplist.toList |> Expect.equal simpsons
            ]
        , describe "next"
            [ test "moves the focus forward" <|
                \_ -> Ziplist.next simpsonsZiplist |> Ziplist.current |> Expect.equal (Just "Marge")
            , test "is no longer reset after moving" <|
                \_ -> Ziplist.next simpsonsZiplist |> Ziplist.isReset |> Expect.equal False
            , test "wraps back to the first item after the last one" <|
                \_ ->
                    simpsonsZiplist
                        |> Ziplist.next
                        |> Ziplist.next
                        |> Ziplist.next
                        |> Expect.equal simpsonsZiplist
            , test "a single item ziplist stays on its item" <|
                \_ ->
                    Ziplist.init [ "Maggie" ]
                        |> Ziplist.next
                        |> Ziplist.current
                        |> Expect.equal (Just "Maggie")
            , test "an empty ziplist stays empty" <|
                \_ -> Ziplist.next emptyZiplist |> Expect.equal Ziplist.Empty
            , test "an empty ziplist is not reset" <|
                \_ -> Ziplist.isReset emptyZiplist |> Expect.equal False
            ]
        , describe "map"
            [ test "tags items with their position, preserving order" <|
                \_ ->
                    Ziplist.next simpsonsZiplist
                        |> positionsOf
                        |> Expect.equal [ ( Before, "Homer" ), ( Current, "Marge" ), ( After, "Bart" ) ]
            , test "preserves order when several items are before the focus" <|
                \_ ->
                    Ziplist.init [ "Homer", "Marge", "Bart", "Lisa" ]
                        |> Ziplist.next
                        |> Ziplist.next
                        |> positionsOf
                        |> Expect.equal [ ( Before, "Homer" ), ( Before, "Marge" ), ( Current, "Bart" ), ( After, "Lisa" ) ]
            , test "an empty ziplist maps to an empty list" <|
                \_ -> emptyZiplist |> positionsOf |> Expect.equal []
            ]
        , describe "positionEquals"
            [ test "is true only for identical positions" <|
                \_ ->
                    let
                        positions =
                            [ Before, Current, After ]
                    in
                    positions
                        |> List.concatMap (\left -> List.map (\right -> ( left, right )) positions)
                        |> List.filter (\( left, right ) -> Ziplist.positionEquals left right)
                        |> Expect.equal (List.map (\position -> ( position, position )) positions)
            ]
        ]
