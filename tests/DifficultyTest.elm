module DifficultyTest exposing (suite)

{-| Unit tests for the `Difficulty` module: serialization, labels and decoders.
-}

import Difficulty exposing (Difficulty(..))
import Expect
import Json.Decode as JsonDecode
import Test exposing (Test, describe, test)
import Yaml.Decode as YamlDecode


allDifficulties : List Difficulty
allDifficulties =
    [ Unset, Easy, Medium, Hard, Extreme ]


unknownDifficulty : String
unknownDifficulty =
    "duff-drunk"


suite : Test
suite =
    describe "Difficulty"
        [ test "default is unset" <|
            \_ -> Difficulty.default |> Expect.equal Unset
        , test "fromString reverses toString" <|
            \_ ->
                allDifficulties
                    |> List.map (Difficulty.toString >> Difficulty.fromString)
                    |> Expect.equal allDifficulties
        , test "toString returns lowercase names" <|
            \_ ->
                allDifficulties
                    |> List.map Difficulty.toString
                    |> Expect.equal [ "unset", "easy", "medium", "hard", "extreme" ]
        , test "fromString falls back to unset for unknown values" <|
            \_ -> Difficulty.fromString unknownDifficulty |> Expect.equal Unset
        , test "label is human readable" <|
            \_ ->
                allDifficulties
                    |> List.map Difficulty.label
                    |> Expect.equal [ "Unrated", "Easy", "Medium", "Hard", "Extreme" ]
        , test "every difficulty has distinct dot and badge classes" <|
            \_ ->
                [ List.map Difficulty.dotClasses allDifficulties, List.map Difficulty.badgeClasses allDifficulties ]
                    |> List.map
                        (List.foldl
                            (\classes accumulator ->
                                if List.member classes accumulator then
                                    accumulator

                                else
                                    classes :: accumulator
                            )
                            []
                            >> List.length
                        )
                    |> Expect.equal [ List.length allDifficulties, List.length allDifficulties ]
        , describe "jsonDecoder"
            [ test "decodes a known difficulty" <|
                \_ -> JsonDecode.decodeString Difficulty.jsonDecoder "\"hard\"" |> Expect.equal (Ok Hard)
            , test "decodes an unknown difficulty as unset" <|
                \_ -> JsonDecode.decodeString Difficulty.jsonDecoder ("\"" ++ unknownDifficulty ++ "\"") |> Expect.equal (Ok Unset)
            ]
        , describe "yamlDecoder"
            [ test "decodes a known difficulty" <|
                \_ -> YamlDecode.fromString Difficulty.yamlDecoder "easy" |> Expect.equal (Ok Easy)
            , test "decodes an unknown difficulty as unset" <|
                \_ -> YamlDecode.fromString Difficulty.yamlDecoder unknownDifficulty |> Expect.equal (Ok Unset)
            ]
        ]
