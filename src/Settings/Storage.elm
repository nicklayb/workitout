port module Settings.Storage exposing (clear, save)

{-| Side effects persisting the settings into the browser's local storage.
-}

import Settings exposing (Settings)
import Settings.Persisted as Persisted


port storeSettings : Maybe String -> Cmd msg


{-| Saves the settings, at the current version, into the local storage.
-}
save : Settings -> Cmd msg
save settings =
    storeSettings (Just (Persisted.toString settings))


{-| Removes the settings from the local storage.
-}
clear : Cmd msg
clear =
    storeSettings Nothing
