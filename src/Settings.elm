module Settings exposing
    ( Settings
    , autoNextRound
    , default
    , preRoundDelay
    , preRoundDelayBounds
    , preRoundDelaySeconds
    , setAutoNextRound
    , setPreRoundDelay
    , setSoundEffects
    , soundEffects
    )

{-| User preferences of the application. The type is opaque so every value
goes through setters that keep it valid (integers are capped to their bounds).
-}

import BoundedInt exposing (BoundedInt, Bounds)


type Settings
    = Settings
        { soundEffects : Bool
        , autoNextRound : Bool
        , preRoundDelay : BoundedInt
        }


maximumPreRoundDelaySeconds : Int
maximumPreRoundDelaySeconds =
    60


{-| Allowed range, in seconds, of the extra delay added before a round starts.
-}
preRoundDelayBounds : Bounds
preRoundDelayBounds =
    { minimum = 0, maximum = maximumPreRoundDelaySeconds }


{-| Settings used when the user never saved any.
-}
default : Settings
default =
    Settings
        { soundEffects = True
        , autoNextRound = True
        , preRoundDelay = BoundedInt.init preRoundDelayBounds 0
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
