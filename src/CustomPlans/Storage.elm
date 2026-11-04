port module CustomPlans.Storage exposing (clear, save)

{-| Side effects persisting the user's plans into the browser's local storage.
-}

import CustomPlans exposing (CustomPlans)


port storeCustomPlans : Maybe String -> Cmd msg


{-| Saves the plans, at the current version, into the local storage.
-}
save : CustomPlans -> Cmd msg
save plans =
    storeCustomPlans (Just (CustomPlans.toString plans))


{-| Removes the plans from the local storage.
-}
clear : Cmd msg
clear =
    storeCustomPlans Nothing
