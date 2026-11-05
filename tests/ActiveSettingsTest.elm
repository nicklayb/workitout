module ActiveSettingsTest exposing (suite)

{-| Unit tests for the `ActiveSettings` module: which settings are shown as
indicators.
-}

import ActiveSettings exposing (Indicator(..))
import Expect
import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)
import Test exposing (Test, describe, test)


{-| Settings with everything disabled, as Ned Flanders would want them.
-}
quietSettings : Settings
quietSettings =
    Settings.default
        |> Settings.setAutoNextRound False
        |> Settings.setSoundEffects False
        |> Settings.setEffortMusic EffortSilent
        |> Settings.setBreakMusic BreakSilent
        |> Settings.setWakeLock False


suite : Test
suite =
    describe "ActiveSettings.fromSettings"
        [ test "shows every enabled default setting, without an empty delay" <|
            \_ -> ActiveSettings.fromSettings Settings.default |> Expect.equal [ AutoNextRound, SoundEffects, Music, WakeLock ]
        , test "shows nothing when everything is disabled" <|
            \_ -> ActiveSettings.fromSettings quietSettings |> Expect.equal []
        , test "shows the pre-round delay with its value" <|
            \_ -> quietSettings |> Settings.setPreRoundDelay 15 |> ActiveSettings.fromSettings |> Expect.equal [ PreRoundDelay 15 ]
        , test "shows music played during efforts only" <|
            \_ -> quietSettings |> Settings.setEffortMusic EffortFromPlaylist |> ActiveSettings.fromSettings |> Expect.equal [ Music ]
        , test "shows music played during breaks only" <|
            \_ -> quietSettings |> Settings.setBreakMusic BreakFromPlaylist |> ActiveSettings.fromSettings |> Expect.equal [ Music ]
        , test "dimmed breaks alone play no music" <|
            \_ -> quietSettings |> Settings.setBreakMusic BreakDimmedEffort |> ActiveSettings.fromSettings |> Expect.equal []
        ]
