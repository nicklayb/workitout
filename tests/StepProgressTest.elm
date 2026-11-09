module StepProgressTest exposing (suite)

{-| Unit tests for the `StepProgress` module.
-}

import Expect
import Plan exposing (Amount(..))
import StepProgress exposing (StepProgress)
import Test exposing (Test, describe, test)


stepSeconds : Int
stepSeconds =
    3


extraSeconds : Int
extraSeconds =
    2


stepCount : Int
stepCount =
    12


tickTimes : Int -> StepProgress -> StepProgress
tickTimes count progress =
    List.foldl (\_ accumulator -> StepProgress.tick accumulator) progress (List.range 1 count)


suite : Test
suite =
    describe "StepProgress"
        [ describe "idle"
            [ test "is idle" <|
                \_ -> StepProgress.isIdle StepProgress.idle |> Expect.equal True
            , test "doesn't tick" <|
                \_ -> StepProgress.isTicking StepProgress.idle |> Expect.equal False
            , test "has no countdown" <|
                \_ -> StepProgress.remaining StepProgress.idle |> Expect.equal Nothing
            ]
        , describe "timed step"
            [ test "counts down the duration and the extra seconds" <|
                \_ -> StepProgress.start extraSeconds (Duration stepSeconds) |> StepProgress.remaining |> Expect.equal (Just (stepSeconds + extraSeconds))
            , test "ticks until the end of the countdown" <|
                \_ -> StepProgress.start 0 (Duration stepSeconds) |> tickTimes (stepSeconds - 1) |> StepProgress.isTicking |> Expect.equal True
            , test "isn't finished before the end of the countdown" <|
                \_ -> StepProgress.start 0 (Duration stepSeconds) |> tickTimes (stepSeconds - 1) |> StepProgress.isFinished |> Expect.equal False
            , test "is finished at the end of the countdown" <|
                \_ -> StepProgress.start 0 (Duration stepSeconds) |> tickTimes stepSeconds |> StepProgress.isFinished |> Expect.equal True
            , test "never awaits the user" <|
                \_ -> StepProgress.start 0 (Duration stepSeconds) |> tickTimes stepSeconds |> StepProgress.isAwaitingDone |> Expect.equal False
            ]
        , describe "repetition step"
            [ test "awaits the user right away without extra seconds" <|
                \_ -> StepProgress.start 0 (Count stepCount) |> StepProgress.isAwaitingDone |> Expect.equal True
            , test "doesn't tick while awaiting the user" <|
                \_ -> StepProgress.start 0 (Count stepCount) |> StepProgress.isTicking |> Expect.equal False
            , test "counts down the extra seconds first" <|
                \_ -> StepProgress.start extraSeconds (Count stepCount) |> StepProgress.remaining |> Expect.equal (Just extraSeconds)
            , test "doesn't await the user during the extra seconds" <|
                \_ -> StepProgress.start extraSeconds (Count stepCount) |> tickTimes (extraSeconds - 1) |> StepProgress.isAwaitingDone |> Expect.equal False
            , test "awaits the user once the extra seconds are over" <|
                \_ -> StepProgress.start extraSeconds (Count stepCount) |> tickTimes extraSeconds |> StepProgress.isAwaitingDone |> Expect.equal True
            , test "is never finished by ticks" <|
                \_ -> StepProgress.start extraSeconds (Count stepCount) |> tickTimes (extraSeconds + stepCount) |> StepProgress.isFinished |> Expect.equal False
            ]
        ]
