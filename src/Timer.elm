module Timer exposing (Timer, init, isRunning, remaining, start, tick)


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


start : Timer -> Timer
start timer =
    { timer | status = Running }


tick : Timer -> Timer
tick timer =
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
