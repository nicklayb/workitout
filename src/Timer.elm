module Timer exposing (Timer, init, isFinished, isRunning, remaining, start, tick)


type alias Timer =
    { status : Status
    , target : Int
    , current : Int
    }


type Status
    = Ready
    | Running
    | Finished


init : Int -> Timer
init target =
    { status = Ready, target = target, current = 0 }


remaining : Timer -> Int
remaining { target, current } =
    target - current


isRunning : Timer -> Bool
isRunning timer =
    case timer.status of
        Running ->
            True

        _ ->
            False


{-| Whether the timer ran until its target.
-}
isFinished : Timer -> Bool
isFinished timer =
    timer.status == Finished


start : Timer -> Timer
start timer =
    { timer | status = Running }


{-| Advances a running timer by one second. Other timers are left unchanged.
-}
tick : Timer -> Timer
tick timer =
    if isRunning timer then
        tickRunning timer

    else
        timer


tickRunning : Timer -> Timer
tickRunning timer =
    let
        newCurrent =
            timer.current + 1

        status =
            if newCurrent == timer.target then
                Finished

            else
                Running
    in
    { timer | current = newCurrent, status = status }
