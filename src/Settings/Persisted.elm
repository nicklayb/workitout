module Settings.Persisted exposing (Loaded(..), currentVersion, decodeString, encode, exportFileName, mimeType, toString)

{-| Versioned JSON representation of the settings, used both for the local
storage and for the exported file.

Every payload carries a `version` field. Payloads of older versions are
migrated to the current one while decoding.

To introduce a new version:

1.  bump `currentVersion`;
2.  add a `VersionN` record with its decoder describing the new shape and a
    `fromVersionN` conversion to `Settings`;
3.  turn the previous `fromVersion` conversion into a `migrateVersionToN`
    function producing the new record, and chain it in `versionDecoder`;
4.  update `encode` to write the new shape.

-}

import Audius.Playlist as Playlist exposing (Playlist)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)


{-| Outcome of a successful decoding: either the payload already was at the
current version, or it was migrated from an older one and should be saved
back.
-}
type Loaded
    = UpToDate Settings
    | Migrated Settings


type alias Version1 =
    { soundEffects : Bool
    , autoNextRound : Bool
    , preRoundDelaySeconds : Int
    }


type alias Version2 =
    { soundEffects : Bool
    , autoNextRound : Bool
    , preRoundDelaySeconds : Int
    , effortMusic : EffortMusic
    , effortPlaylist : Playlist
    , breakMusic : BreakMusic
    , breakPlaylist : Playlist
    , dimmedEffortVolumePercent : Int
    }


type alias Version3 =
    { soundEffects : Bool
    , autoNextRound : Bool
    , preRoundDelaySeconds : Int
    , effortMusic : EffortMusic
    , effortPlaylist : Playlist
    , breakMusic : BreakMusic
    , breakPlaylist : Playlist
    , dimmedEffortVolumePercent : Int
    , wakeLock : Bool
    }


{-| Version written by `encode`.
-}
currentVersion : Int
currentVersion =
    3


effortMusicKeys : List ( EffortMusic, String )
effortMusicKeys =
    [ ( EffortSilent, "none" )
    , ( EffortFromPlaylist, "playlist" )
    ]


breakMusicKeys : List ( BreakMusic, String )
breakMusicKeys =
    [ ( BreakSilent, "none" )
    , ( BreakDimmedEffort, "dimmedEffort" )
    , ( BreakFromPlaylist, "playlist" )
    ]


jsonIndentation : Int
jsonIndentation =
    2


{-| MIME type of the exported file.
-}
mimeType : String
mimeType =
    "application/json"


{-| Name of the file the settings are exported to.
-}
exportFileName : String
exportFileName =
    "workitout-settings.json"


{-| Encodes the settings at the current version.
-}
encode : Settings -> Encode.Value
encode settings =
    Encode.object
        [ ( "version", Encode.int currentVersion )
        , ( "soundEffects", Encode.bool (Settings.soundEffects settings) )
        , ( "autoNextRound", Encode.bool (Settings.autoNextRound settings) )
        , ( "preRoundDelaySeconds", Encode.int (Settings.preRoundDelaySeconds settings) )
        , ( "effortMusic", encodeKey effortMusicKeys (Settings.effortMusic settings) )
        , ( "effortPlaylistUrl", Encode.string (Playlist.toUrl (Settings.effortPlaylist settings)) )
        , ( "breakMusic", encodeKey breakMusicKeys (Settings.breakMusic settings) )
        , ( "breakPlaylistUrl", Encode.string (Playlist.toUrl (Settings.breakPlaylist settings)) )
        , ( "dimmedEffortVolumePercent", Encode.int (Settings.dimmedEffortVolumePercent settings) )
        , ( "wakeLock", Encode.bool (Settings.wakeLock settings) )
        ]


{-| Encodes a value as its key in `keys`.
-}
encodeKey : List ( a, String ) -> a -> Encode.Value
encodeKey keys value =
    keys
        |> List.filter (\( candidate, _ ) -> candidate == value)
        |> List.head
        |> Maybe.map (Tuple.second >> Encode.string)
        |> Maybe.withDefault Encode.null


{-| Encodes the settings at the current version into an indented JSON string.
-}
toString : Settings -> String
toString settings =
    Encode.encode jsonIndentation (encode settings)


{-| Decodes a JSON string of any supported version, migrating it to the current
one. Fails on malformed JSON, missing or mistyped fields and unknown versions.
-}
decodeString : String -> Result String Loaded
decodeString json =
    Decode.decodeString decoder json
        |> Result.mapError Decode.errorToString


decoder : Decoder Loaded
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen versionDecoder


versionDecoder : Int -> Decoder Loaded
versionDecoder version =
    let
        settingsDecoder =
            case version of
                1 ->
                    Decode.map (migrateVersion1To2 >> migrateVersion2To3 >> fromVersion3) version1Decoder

                2 ->
                    Decode.map (migrateVersion2To3 >> fromVersion3) version2Decoder

                3 ->
                    Decode.map fromVersion3 version3Decoder

                _ ->
                    Decode.fail ("Unsupported settings version " ++ String.fromInt version)
    in
    Decode.map (toLoaded version) settingsDecoder


toLoaded : Int -> Settings -> Loaded
toLoaded version settings =
    if version < currentVersion then
        Migrated settings

    else
        UpToDate settings


version1Decoder : Decoder Version1
version1Decoder =
    Decode.map3 Version1
        (Decode.field "soundEffects" Decode.bool)
        (Decode.field "autoNextRound" Decode.bool)
        (Decode.field "preRoundDelaySeconds" Decode.int)


{-| Version 1 had no music: the default music settings are used.
-}
migrateVersion1To2 : Version1 -> Version2
migrateVersion1To2 version1 =
    { soundEffects = version1.soundEffects
    , autoNextRound = version1.autoNextRound
    , preRoundDelaySeconds = version1.preRoundDelaySeconds
    , effortMusic = Settings.effortMusic Settings.default
    , effortPlaylist = Settings.effortPlaylist Settings.default
    , breakMusic = Settings.breakMusic Settings.default
    , breakPlaylist = Settings.breakPlaylist Settings.default
    , dimmedEffortVolumePercent = Settings.dimmedEffortVolumePercent Settings.default
    }


version2Decoder : Decoder Version2
version2Decoder =
    Decode.succeed Version2
        |> andMap (Decode.field "soundEffects" Decode.bool)
        |> andMap (Decode.field "autoNextRound" Decode.bool)
        |> andMap (Decode.field "preRoundDelaySeconds" Decode.int)
        |> andMap (Decode.field "effortMusic" (keyDecoder effortMusicKeys))
        |> andMap (Decode.field "effortPlaylistUrl" playlistDecoder)
        |> andMap (Decode.field "breakMusic" (keyDecoder breakMusicKeys))
        |> andMap (Decode.field "breakPlaylistUrl" playlistDecoder)
        |> andMap (Decode.field "dimmedEffortVolumePercent" Decode.int)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


{-| Decodes a key of `keys` into its value, failing on unknown keys.
-}
keyDecoder : List ( a, String ) -> Decoder a
keyDecoder keys =
    Decode.string
        |> Decode.andThen
            (\key ->
                case List.filter (\( _, candidate ) -> candidate == key) keys of
                    ( value, _ ) :: _ ->
                        Decode.succeed value

                    [] ->
                        Decode.fail ("Unknown key " ++ key)
            )


playlistDecoder : Decoder Playlist
playlistDecoder =
    Decode.string
        |> Decode.andThen
            (\url ->
                case Playlist.fromUrl url of
                    Just playlist ->
                        Decode.succeed playlist

                    Nothing ->
                        Decode.fail ("Not an Audius playlist URL: " ++ url)
            )


{-| Version 2 had no wake lock setting: the default one is used.
-}
migrateVersion2To3 : Version2 -> Version3
migrateVersion2To3 version2 =
    { soundEffects = version2.soundEffects
    , autoNextRound = version2.autoNextRound
    , preRoundDelaySeconds = version2.preRoundDelaySeconds
    , effortMusic = version2.effortMusic
    , effortPlaylist = version2.effortPlaylist
    , breakMusic = version2.breakMusic
    , breakPlaylist = version2.breakPlaylist
    , dimmedEffortVolumePercent = version2.dimmedEffortVolumePercent
    , wakeLock = Settings.wakeLock Settings.default
    }


version3Decoder : Decoder Version3
version3Decoder =
    Decode.succeed Version3
        |> andMap (Decode.field "soundEffects" Decode.bool)
        |> andMap (Decode.field "autoNextRound" Decode.bool)
        |> andMap (Decode.field "preRoundDelaySeconds" Decode.int)
        |> andMap (Decode.field "effortMusic" (keyDecoder effortMusicKeys))
        |> andMap (Decode.field "effortPlaylistUrl" playlistDecoder)
        |> andMap (Decode.field "breakMusic" (keyDecoder breakMusicKeys))
        |> andMap (Decode.field "breakPlaylistUrl" playlistDecoder)
        |> andMap (Decode.field "dimmedEffortVolumePercent" Decode.int)
        |> andMap (Decode.field "wakeLock" Decode.bool)


fromVersion3 : Version3 -> Settings
fromVersion3 version3 =
    Settings.default
        |> Settings.setSoundEffects version3.soundEffects
        |> Settings.setAutoNextRound version3.autoNextRound
        |> Settings.setPreRoundDelay version3.preRoundDelaySeconds
        |> Settings.setEffortMusic version3.effortMusic
        |> Settings.setEffortPlaylist version3.effortPlaylist
        |> Settings.setBreakMusic version3.breakMusic
        |> Settings.setBreakPlaylist version3.breakPlaylist
        |> Settings.setDimmedEffortVolume version3.dimmedEffortVolumePercent
        |> Settings.setWakeLock version3.wakeLock
