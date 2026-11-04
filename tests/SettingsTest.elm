module SettingsTest exposing (suite)

{-| Unit tests for the `Settings` module: defaults, setters and capping.
-}

import BoundedInt
import Expect
import Settings
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Settings"
        [ describe "default"
            [ test "enables sound effects" <|
                \_ -> Settings.soundEffects Settings.default |> Expect.equal True
            , test "moves to the next round automatically" <|
                \_ -> Settings.autoNextRound Settings.default |> Expect.equal True
            , test "has no pre-round delay" <|
                \_ -> Settings.preRoundDelaySeconds Settings.default |> Expect.equal 0
            ]
        , describe "setters"
            [ test "setSoundEffects" <|
                \_ -> Settings.default |> Settings.setSoundEffects False |> Settings.soundEffects |> Expect.equal False
            , test "setAutoNextRound" <|
                \_ -> Settings.default |> Settings.setAutoNextRound False |> Settings.autoNextRound |> Expect.equal False
            , test "setPreRoundDelay keeps a value within bounds" <|
                \_ -> Settings.default |> Settings.setPreRoundDelay 15 |> Settings.preRoundDelaySeconds |> Expect.equal 15
            , test "setPreRoundDelay caps above one minute" <|
                \_ -> Settings.default |> Settings.setPreRoundDelay 61 |> Settings.preRoundDelaySeconds |> Expect.equal 60
            , test "setPreRoundDelay caps below zero" <|
                \_ -> Settings.default |> Settings.setPreRoundDelay -1 |> Settings.preRoundDelaySeconds |> Expect.equal 0
            ]
        , test "preRoundDelay exposes the bounds" <|
            \_ ->
                Settings.preRoundDelay Settings.default
                    |> BoundedInt.bounds
                    |> Expect.equal Settings.preRoundDelayBounds
        ]
