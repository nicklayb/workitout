module TimerTest exposing (suite)

{-| Unit tests for the `Timer` module.
-}

import Expect
import Test exposing (Test, describe, test)
import Timer exposing (Timer)


targetSeconds : Int
targetSeconds =
    3


tickTimes : Int -> Timer -> Timer
tickTimes count timer =
    List.foldl (\_ accumulator -> Timer.tick accumulator) timer (List.range 1 count)


suite : Test
suite =
    describe "Timer"
        [ test "init has the whole target remaining" <|
            \_ -> Timer.init targetSeconds |> Timer.remaining |> Expect.equal targetSeconds
        , test "init is not running" <|
            \_ -> Timer.init targetSeconds |> Timer.isRunning |> Expect.equal False
        , test "start makes it run" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> Timer.isRunning |> Expect.equal True
        , test "start does not consume time" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> Timer.remaining |> Expect.equal targetSeconds
        , test "tick decreases the remaining time by one" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> Timer.tick |> Timer.remaining |> Expect.equal (targetSeconds - 1)
        , test "keeps running before reaching the target" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> tickTimes (targetSeconds - 1) |> Timer.isRunning |> Expect.equal True
        , test "stops running once the target is reached" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> tickTimes targetSeconds |> Timer.isRunning |> Expect.equal False
        , test "has nothing remaining once the target is reached" <|
            \_ -> Timer.init targetSeconds |> Timer.start |> tickTimes targetSeconds |> Timer.remaining |> Expect.equal 0
        ]
