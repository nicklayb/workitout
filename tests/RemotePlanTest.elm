module RemotePlanTest exposing (suite)

{-| Unit tests for the `RemotePlan` JSON decoder.
-}

import Difficulty exposing (Difficulty(..))
import Expect
import Fixtures
import Json.Decode as JsonDecode
import RemotePlan
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RemotePlan.decoder"
        [ test "decodes every field" <|
            \_ ->
                JsonDecode.decodeString RemotePlan.decoder Fixtures.remotePlanJson
                    |> Expect.equal
                        (Ok
                            { description = Fixtures.planDescription
                            , author_name = Fixtures.authorName
                            , download_url = "https://raw.githubusercontent.com/chunkylover53/plans/main/duff.yml"
                            , sha = "742evergreenterrace"
                            , path = "plans/duff.yml"
                            , name = "duff.yml"
                            , difficulty = Extreme
                            }
                        )
        , test "decodes a list of remote plans" <|
            \_ ->
                JsonDecode.decodeString (JsonDecode.list RemotePlan.decoder) Fixtures.remotePlansJson
                    |> Result.map (List.map .author_name)
                    |> Expect.equal (Ok [ Fixtures.authorName, "Lisa Simpson" ])
        , test "defaults the difficulty when it is missing" <|
            \_ ->
                JsonDecode.decodeString (JsonDecode.list RemotePlan.decoder) Fixtures.remotePlansJson
                    |> Result.map (List.map .difficulty)
                    |> Expect.equal (Ok [ Extreme, Difficulty.default ])
        , test "fails when a field is missing" <|
            \_ ->
                JsonDecode.decodeString RemotePlan.decoder Fixtures.remotePlanMissingFieldJson
                    |> Expect.err
        ]
