module Settings.PersistedTest exposing (suite)

{-| Unit tests for the `Settings.Persisted` module: versioned JSON encoding,
strict decoding and capping.
-}

import Expect
import Settings exposing (Settings)
import Settings.Persisted as Persisted exposing (Loaded(..))
import Test exposing (Test, describe, test)


customSettings : Settings
customSettings =
    Settings.default
        |> Settings.setSoundEffects False
        |> Settings.setAutoNextRound False
        |> Settings.setPreRoundDelay 30


version1Json : String
version1Json =
    """{ "version": 1, "soundEffects": false, "autoNextRound": false, "preRoundDelaySeconds": 30 }"""


{-| Version 1 JSON with the given pre-round delay, as raw JSON.
-}
version1JsonWithDelay : String -> String
version1JsonWithDelay delay =
    """{ "version": 1, "soundEffects": true, "autoNextRound": true, "preRoundDelaySeconds": """ ++ delay ++ " }"


expectInvalid : String -> Expect.Expectation
expectInvalid json =
    case Persisted.decodeString json of
        Ok _ ->
            Expect.fail ("Expected invalid settings: " ++ json)

        Err _ ->
            Expect.pass


decodedDelay : String -> Result String Int
decodedDelay json =
    Persisted.decodeString json
        |> Result.map (loadedSettings >> Settings.preRoundDelaySeconds)


loadedSettings : Loaded -> Settings
loadedSettings loaded =
    case loaded of
        UpToDate settings ->
            settings

        Migrated settings ->
            settings


suite : Test
suite =
    describe "Settings.Persisted"
        [ describe "decodeString"
            [ test "decodes the current version as up to date" <|
                \_ -> Persisted.decodeString version1Json |> Expect.equal (Ok (UpToDate customSettings))
            , test "round trips through toString" <|
                \_ -> Persisted.toString customSettings |> Persisted.decodeString |> Expect.equal (Ok (UpToDate customSettings))
            , test "writes the current version" <|
                \_ ->
                    Persisted.toString Settings.default
                        |> String.contains ("\"version\": " ++ String.fromInt Persisted.currentVersion)
                        |> Expect.equal True
            , test "caps a delay above the maximum" <|
                \_ -> decodedDelay (version1JsonWithDelay "3600") |> Expect.equal (Ok 60)
            , test "caps a delay below the minimum" <|
                \_ -> decodedDelay (version1JsonWithDelay "-10") |> Expect.equal (Ok 0)
            , test "rejects a non integer delay" <|
                \_ -> expectInvalid (version1JsonWithDelay "2.5")
            , test "rejects malformed JSON" <|
                \_ -> expectInvalid "{ \"version\": 1, "
            , test "rejects a payload without version" <|
                \_ -> expectInvalid """{ "soundEffects": true, "autoNextRound": true, "preRoundDelaySeconds": 0 }"""
            , test "rejects an unknown version" <|
                \_ -> expectInvalid """{ "version": 742, "soundEffects": true, "autoNextRound": true, "preRoundDelaySeconds": 0 }"""
            , test "rejects a missing field" <|
                \_ -> expectInvalid """{ "version": 1, "soundEffects": true, "preRoundDelaySeconds": 0 }"""
            , test "rejects a mistyped field" <|
                \_ -> expectInvalid """{ "version": 1, "soundEffects": "yes", "autoNextRound": true, "preRoundDelaySeconds": 0 }"""
            ]
        ]
