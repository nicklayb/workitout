module StepProgress exposing (StepProgress, idle, isAwaitingDone, isFinished, isIdle, isTicking, remaining, start, tick)

{-| Progress of the step being run. Timed steps count down to zero, while
repetition steps wait for the user to tell they are done.
-}

import Plan exposing (Amount(..))
import Timer exposing (Timer)


type StepProgress
    = Idle
    | CountingDown Timer
    | Preparing Timer
    | AwaitingDone


{-| No step in progress, the user has to start it.
-}
idle : StepProgress
idle =
    Idle


{-| Starts a step, adding `extraSeconds` before it. Timed steps count the
extra seconds with their duration; repetition steps count them down first
then wait for the user.
-}
start : Int -> Amount -> StepProgress
start extraSeconds amount =
    case amount of
        Duration seconds ->
            CountingDown (startedTimer (seconds + extraSeconds))

        Count _ ->
            if extraSeconds > 0 then
                Preparing (startedTimer extraSeconds)

            else
                AwaitingDone


startedTimer : Int -> Timer
startedTimer seconds =
    Timer.start (Timer.init seconds)


{-| Advances the countdown by one second. A finished preparation waits for the
user; other progresses are left unchanged.
-}
tick : StepProgress -> StepProgress
tick progress =
    case progress of
        CountingDown timer ->
            CountingDown (Timer.tick timer)

        Preparing timer ->
            let
                ticked =
                    Timer.tick timer
            in
            if Timer.isFinished ticked then
                AwaitingDone

            else
                Preparing ticked

        _ ->
            progress


{-| Whether a countdown is running, so the progress needs ticks.
-}
isTicking : StepProgress -> Bool
isTicking progress =
    case progress of
        CountingDown timer ->
            Timer.isRunning timer

        Preparing timer ->
            Timer.isRunning timer

        _ ->
            False


{-| Whether a timed step reached the end of its countdown.
-}
isFinished : StepProgress -> Bool
isFinished progress =
    case progress of
        CountingDown timer ->
            Timer.isFinished timer

        _ ->
            False


isIdle : StepProgress -> Bool
isIdle progress =
    progress == Idle


isAwaitingDone : StepProgress -> Bool
isAwaitingDone progress =
    progress == AwaitingDone


{-| Seconds left on the countdown, if one is running or just finished.
-}
remaining : StepProgress -> Maybe Int
remaining progress =
    case progress of
        CountingDown timer ->
            Just (Timer.remaining timer)

        Preparing timer ->
            Just (Timer.remaining timer)

        _ ->
            Nothing
