module Audius.PlaylistTest exposing (suite)

{-| Unit tests for the `Audius.Playlist` module: parsing playlist page URLs.
-}

import Audius.Playlist as Playlist
import Expect
import Fixtures
import Test exposing (Test, describe, test)


parsedParts : String -> Maybe ( String, String )
parsedParts url =
    Playlist.fromUrl url
        |> Maybe.map (\playlist -> ( Playlist.handle playlist, Playlist.slug playlist ))


expectedParts : Maybe ( String, String )
expectedParts =
    Just ( Fixtures.playlistHandle, Fixtures.playlistSlug )


suite : Test
suite =
    describe "Audius.Playlist"
        [ describe "fromUrl"
            [ test "parses a playlist URL" <|
                \_ -> parsedParts Fixtures.playlistUrl |> Expect.equal expectedParts
            , test "accepts the www host" <|
                \_ -> parsedParts "https://www.audius.co/besharps/playlist/barbershop-hits" |> Expect.equal expectedParts
            , test "ignores a trailing slash, query, fragment and spaces" <|
                \_ -> parsedParts "  https://audius.co/besharps/playlist/barbershop-hits/?ref=moe#top " |> Expect.equal expectedParts
            , test "rejects another host" <|
                \_ -> parsedParts "https://soundcloud.com/besharps/playlist/barbershop-hits" |> Expect.equal Nothing
            , test "rejects a track URL" <|
                \_ -> parsedParts "https://audius.co/besharps/baby-on-board" |> Expect.equal Nothing
            , test "rejects an album URL" <|
                \_ -> parsedParts "https://audius.co/besharps/album/barbershop-hits" |> Expect.equal Nothing
            , test "rejects text that isn't a URL" <|
                \_ -> parsedParts "Baby on Board" |> Expect.equal Nothing
            ]
        , describe "toUrl"
            [ test "round trips through fromUrl" <|
                \_ ->
                    Playlist.fromUrl Fixtures.playlistUrl
                        |> Maybe.map Playlist.toUrl
                        |> Expect.equal (Just Fixtures.playlistUrl)
            ]
        ]
