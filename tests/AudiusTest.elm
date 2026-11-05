module AudiusTest exposing (suite)

{-| Unit tests for the `Audius` module: decoding playlist tracks and building
URLs.
-}

import Audius
import Expect
import Fixtures
import Json.Decode as Decode
import Test exposing (Test, describe, test)


babyOnBoard : Audius.Track
babyOnBoard =
    { id = "babyOnBoard", title = "Baby on Board", artist = "The Be Sharps", permalink = "/besharps/baby-on-board" }


suite : Test
suite =
    describe "Audius"
        [ describe "playlistTracksDecoder"
            [ test "keeps streamable tracks only, defaulting missing flags" <|
                \_ ->
                    Decode.decodeString Audius.playlistTracksDecoder Fixtures.audiusPlaylistJson
                        |> Result.map (List.map .id)
                        |> Expect.equal (Ok [ "babyOnBoard", "monorail" ])
            , test "decodes the track details" <|
                \_ ->
                    Decode.decodeString Audius.playlistTracksDecoder Fixtures.audiusPlaylistJson
                        |> Result.map List.head
                        |> Expect.equal (Ok (Just babyOnBoard))
            , test "fails on an empty response" <|
                \_ ->
                    Decode.decodeString Audius.playlistTracksDecoder """{ "data": [] }"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , test "streamUrl points to the stream endpoint" <|
            \_ -> Audius.streamUrl babyOnBoard |> Expect.equal "https://api.audius.co/v1/tracks/babyOnBoard/stream?app_name=workitout"
        , test "pageUrl points to the track page" <|
            \_ -> Audius.pageUrl babyOnBoard |> Expect.equal "https://audius.co/besharps/baby-on-board"
        ]
