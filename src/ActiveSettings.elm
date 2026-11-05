module ActiveSettings exposing (Indicator(..), fromSettings)

{-| Settings worth reminding during a workout, as discreet indicators.
-}

import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)


{-| A setting in effect. `PreRoundDelay` holds the delay in seconds.
-}
type Indicator
    = PreRoundDelay Int
    | AutoNextRound
    | SoundEffects
    | Music
    | WakeLock


{-| Indicators of the settings in effect, in display order. Disabled settings
and an empty pre-round delay are left out.
-}
fromSettings : Settings -> List Indicator
fromSettings settings =
    let
        delaySeconds =
            Settings.preRoundDelaySeconds settings

        isMusicEnabled =
            Settings.effortMusic settings == EffortFromPlaylist || Settings.breakMusic settings == BreakFromPlaylist
    in
    List.filterMap identity
        [ keepIf (delaySeconds > 0) (PreRoundDelay delaySeconds)
        , keepIf (Settings.autoNextRound settings) AutoNextRound
        , keepIf (Settings.soundEffects settings) SoundEffects
        , keepIf isMusicEnabled Music
        , keepIf (Settings.wakeLock settings) WakeLock
        ]


keepIf : Bool -> a -> Maybe a
keepIf isKept value =
    if isKept then
        Just value

    else
        Nothing
