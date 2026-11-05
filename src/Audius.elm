module Audius exposing (Track, fetchPlaylistTracks, pageUrl, playlistTracksDecoder, streamUrl)

{-| Unauthenticated access to the public Audius API: the tracks of a playlist
and the URLs streaming them.
-}

import Audius.Playlist as Playlist exposing (Playlist)
import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder


{-| Track of a playlist. `permalink` is the path of its page on Audius.
-}
type alias Track =
    { id : String
    , title : String
    , artist : String
    , permalink : String
    }


apiHost : String
apiHost =
    "https://api.audius.co"


webHost : String
webHost =
    "https://audius.co"


{-| Name identifying the application to the API, which requires it.
-}
applicationName : String
applicationName =
    "workitout"


applicationParameter : Url.Builder.QueryParameter
applicationParameter =
    Url.Builder.string "app_name" applicationName


{-| Fetches the tracks of a playlist that can be streamed for free.
-}
fetchPlaylistTracks : (Result Http.Error (List Track) -> msg) -> Playlist -> Cmd msg
fetchPlaylistTracks toMsg playlist =
    Http.get
        { url =
            Url.Builder.crossOrigin apiHost
                [ "v1", "playlists", "by_permalink", Playlist.handle playlist, Playlist.slug playlist ]
                [ applicationParameter ]
        , expect = Http.expectJson toMsg playlistTracksDecoder
        }


{-| URL of the audio of a track. The API redirects it to a freshly signed URL,
so it never expires.
-}
streamUrl : Track -> String
streamUrl track =
    Url.Builder.crossOrigin apiHost [ "v1", "tracks", track.id, "stream" ] [ applicationParameter ]


{-| URL of the page of a track on Audius.
-}
pageUrl : Track -> String
pageUrl track =
    webHost ++ track.permalink


{-| Decodes the response of the playlist endpoint into its streamable tracks.
Tracks that are private, gated or not streamable are left out.
-}
playlistTracksDecoder : Decoder (List Track)
playlistTracksDecoder =
    Decode.field "data" (Decode.index 0 (Decode.field "tracks" (Decode.list maybeTrackDecoder)))
        |> Decode.map (List.filterMap identity)


maybeTrackDecoder : Decoder (Maybe Track)
maybeTrackDecoder =
    Decode.map2
        (\isPlayable track ->
            if isPlayable then
                Just track

            else
                Nothing
        )
        isPlayableDecoder
        trackDecoder


isPlayableDecoder : Decoder Bool
isPlayableDecoder =
    Decode.map2 (&&)
        (flagDecoder "is_streamable" True)
        (Decode.map not (flagDecoder "is_stream_gated" False))


{-| Decodes an optional boolean field, using `fallback` when it is absent or
null.
-}
flagDecoder : String -> Bool -> Decoder Bool
flagDecoder field fallback =
    Decode.maybe (Decode.field field Decode.bool)
        |> Decode.map (Maybe.withDefault fallback)


trackDecoder : Decoder Track
trackDecoder =
    Decode.map4 Track
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.at [ "user", "name" ] Decode.string)
        (Decode.field "permalink" Decode.string)
