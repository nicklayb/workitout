port module WakeLock exposing (keepScreenAwake, release)

{-| Side effects keeping the screen awake through the Screen Wake Lock API of
the browser (see `src/index.js`). Browsers without the API ignore them.
-}


port setScreenWakeLock : Bool -> Cmd msg


{-| Keeps the screen awake when `isWanted`, lets it sleep otherwise.
-}
keepScreenAwake : Bool -> Cmd msg
keepScreenAwake isWanted =
    setScreenWakeLock isWanted


{-| Lets the screen sleep again.
-}
release : Cmd msg
release =
    setScreenWakeLock False
