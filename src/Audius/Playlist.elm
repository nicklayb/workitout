module Audius.Playlist exposing (Playlist, fromUrl, handle, init, slug, toUrl)

{-| Reference to a public Audius playlist, as found in its page URL
(`https://audius.co/<handle>/playlist/<slug>`). The type is opaque so only
references parsed from a valid URL exist.
-}

import Url


type Playlist
    = Playlist { handle : String, slug : String }


webHost : String
webHost =
    "audius.co"


acceptedHosts : List String
acceptedHosts =
    [ webHost, "www." ++ webHost ]


playlistSegment : String
playlistSegment =
    "playlist"


{-| Builds a reference from the owner's handle and the playlist slug.
-}
init : { handle : String, slug : String } -> Playlist
init parts =
    Playlist parts


{-| Parses the URL of a playlist page. Query strings, fragments and a trailing
slash are ignored; any other host or path shape gives `Nothing`.
-}
fromUrl : String -> Maybe Playlist
fromUrl text =
    Url.fromString (String.trim text)
        |> Maybe.andThen fromParsedUrl


fromParsedUrl : Url.Url -> Maybe Playlist
fromParsedUrl url =
    if List.member (String.toLower url.host) acceptedHosts then
        case String.split "/" url.path |> List.filter (not << String.isEmpty) of
            [ owner, segment, playlistSlug ] ->
                if segment == playlistSegment then
                    Just (Playlist { handle = owner, slug = playlistSlug })

                else
                    Nothing

            _ ->
                Nothing

    else
        Nothing


{-| URL of the playlist page on Audius.
-}
toUrl : Playlist -> String
toUrl (Playlist playlist) =
    String.join "/" [ "https:/", webHost, playlist.handle, playlistSegment, playlist.slug ]


{-| Handle of the user owning the playlist.
-}
handle : Playlist -> String
handle (Playlist playlist) =
    playlist.handle


{-| Identifier of the playlist among its owner's playlists.
-}
slug : Playlist -> String
slug (Playlist playlist) =
    playlist.slug
