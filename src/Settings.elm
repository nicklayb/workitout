module Settings exposing
    ( BreakMusic(..)
    , EffortMusic(..)
    , Settings
    , autoNextRound
    , breakMusic
    , breakPlaylist
    , default
    , defaultBreakPlaylist
    , defaultEffortPlaylist
    , dimmedEffortVolume
    , dimmedEffortVolumeBounds
    , dimmedEffortVolumePercent
    , effortMusic
    , effortPlaylist
    , preRoundDelay
    , preRoundDelayBounds
    , preRoundDelaySeconds
    , setAutoNextRound
    , setBreakMusic
    , setBreakPlaylist
    , setDimmedEffortVolume
    , setEffortMusic
    , setEffortPlaylist
    , setPreRoundDelay
    , setSoundEffects
    , soundEffects
    )

{-| User preferences of the application. The type is opaque so every value
goes through setters that keep it valid (integers are capped to their bounds).
-}

import Audius.Playlist as Playlist exposing (Playlist)
import BoundedInt exposing (BoundedInt, Bounds)


type Settings
    = Settings
        { soundEffects : Bool
        , autoNextRound : Bool
        , preRoundDelay : BoundedInt
        , effortMusic : EffortMusic
        , effortPlaylist : Playlist
        , breakMusic : BreakMusic
        , breakPlaylist : Playlist
        , dimmedEffortVolume : BoundedInt
        }


{-| Music played during effort steps.
-}
type EffortMusic
    = EffortSilent
    | EffortFromPlaylist


{-| Music played during break steps. `BreakDimmedEffort` keeps the effort
music going at a lower volume.
-}
type BreakMusic
    = BreakSilent
    | BreakDimmedEffort
    | BreakFromPlaylist


maximumPreRoundDelaySeconds : Int
maximumPreRoundDelaySeconds =
    60


maximumVolumePercent : Int
maximumVolumePercent =
    100


defaultDimmedEffortVolumePercent : Int
defaultDimmedEffortVolumePercent =
    30


{-| Allowed range, in seconds, of the extra delay added before a round starts.
-}
preRoundDelayBounds : Bounds
preRoundDelayBounds =
    { minimum = 0, maximum = maximumPreRoundDelaySeconds }


{-| Allowed range, in percent, of the volume of the dimmed effort music.
-}
dimmedEffortVolumeBounds : Bounds
dimmedEffortVolumeBounds =
    { minimum = 0, maximum = maximumVolumePercent }


{-| Upbeat playlist suggested for effort steps.
-}
defaultEffortPlaylist : Playlist
defaultEffortPlaylist =
    Playlist.init { handle = "flymoonmusic", slug = "gym-motivation" }


{-| Calm playlist suggested for break steps.
-}
defaultBreakPlaylist : Playlist
defaultBreakPlaylist =
    Playlist.init { handle = "Byte", slug = "lofi-chillhop-beats-906" }


{-| Settings used when the user never saved any.
-}
default : Settings
default =
    Settings
        { soundEffects = True
        , autoNextRound = True
        , preRoundDelay = BoundedInt.init preRoundDelayBounds 0
        , effortMusic = EffortFromPlaylist
        , effortPlaylist = defaultEffortPlaylist
        , breakMusic = BreakFromPlaylist
        , breakPlaylist = defaultBreakPlaylist
        , dimmedEffortVolume = BoundedInt.init dimmedEffortVolumeBounds defaultDimmedEffortVolumePercent
        }


{-| Whether sound effects should be played.
-}
soundEffects : Settings -> Bool
soundEffects (Settings settings) =
    settings.soundEffects


{-| Whether the next round starts without the user pressing Start.
-}
autoNextRound : Settings -> Bool
autoNextRound (Settings settings) =
    settings.autoNextRound


{-| Extra delay added to the timer of the first step of a round.
-}
preRoundDelay : Settings -> BoundedInt
preRoundDelay (Settings settings) =
    settings.preRoundDelay


{-| Extra delay added to the timer of the first step of a round, in seconds.
-}
preRoundDelaySeconds : Settings -> Int
preRoundDelaySeconds settings =
    BoundedInt.value (preRoundDelay settings)


{-| Music played during effort steps.
-}
effortMusic : Settings -> EffortMusic
effortMusic (Settings settings) =
    settings.effortMusic


{-| Playlist of the effort music. Kept even when the effort music is silent so
switching back restores it.
-}
effortPlaylist : Settings -> Playlist
effortPlaylist (Settings settings) =
    settings.effortPlaylist


{-| Music played during break steps.
-}
breakMusic : Settings -> BreakMusic
breakMusic (Settings settings) =
    settings.breakMusic


{-| Playlist of the break music. Kept even when another break music is picked
so switching back restores it.
-}
breakPlaylist : Settings -> Playlist
breakPlaylist (Settings settings) =
    settings.breakPlaylist


{-| Volume of the effort music while it plays dimmed during breaks.
-}
dimmedEffortVolume : Settings -> BoundedInt
dimmedEffortVolume (Settings settings) =
    settings.dimmedEffortVolume


{-| Volume of the effort music while it plays dimmed during breaks, in percent.
-}
dimmedEffortVolumePercent : Settings -> Int
dimmedEffortVolumePercent settings =
    BoundedInt.value (dimmedEffortVolume settings)


{-| Enables or disables sound effects.
-}
setSoundEffects : Bool -> Settings -> Settings
setSoundEffects isEnabled (Settings settings) =
    Settings { settings | soundEffects = isEnabled }


{-| Enables or disables starting the next round automatically.
-}
setAutoNextRound : Bool -> Settings -> Settings
setAutoNextRound isEnabled (Settings settings) =
    Settings { settings | autoNextRound = isEnabled }


{-| Sets the pre-round delay in seconds, capped to `preRoundDelayBounds`.
-}
setPreRoundDelay : Int -> Settings -> Settings
setPreRoundDelay seconds (Settings settings) =
    Settings { settings | preRoundDelay = BoundedInt.set seconds settings.preRoundDelay }


{-| Sets the music played during effort steps.
-}
setEffortMusic : EffortMusic -> Settings -> Settings
setEffortMusic music (Settings settings) =
    Settings { settings | effortMusic = music }


{-| Sets the playlist of the effort music.
-}
setEffortPlaylist : Playlist -> Settings -> Settings
setEffortPlaylist playlist (Settings settings) =
    Settings { settings | effortPlaylist = playlist }


{-| Sets the music played during break steps.
-}
setBreakMusic : BreakMusic -> Settings -> Settings
setBreakMusic music (Settings settings) =
    Settings { settings | breakMusic = music }


{-| Sets the playlist of the break music.
-}
setBreakPlaylist : Playlist -> Settings -> Settings
setBreakPlaylist playlist (Settings settings) =
    Settings { settings | breakPlaylist = playlist }


{-| Sets the volume of the dimmed effort music in percent, capped to
`dimmedEffortVolumeBounds`.
-}
setDimmedEffortVolume : Int -> Settings -> Settings
setDimmedEffortVolume percent (Settings settings) =
    Settings { settings | dimmedEffortVolume = BoundedInt.set percent settings.dimmedEffortVolume }
