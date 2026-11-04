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

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Settings exposing (Settings)


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


{-| Version written by `encode`.
-}
currentVersion : Int
currentVersion =
    1


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
        ]


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
                    Decode.map fromVersion1 version1Decoder

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


fromVersion1 : Version1 -> Settings
fromVersion1 version1 =
    Settings.default
        |> Settings.setSoundEffects version1.soundEffects
        |> Settings.setAutoNextRound version1.autoNextRound
        |> Settings.setPreRoundDelay version1.preRoundDelaySeconds
