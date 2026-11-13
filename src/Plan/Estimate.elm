module Plan.Estimate exposing (amountSeconds, daysMapSeconds, format, secondsPerRepetition, workoutSeconds)

{-| Estimation of how long a workout takes. Durations count as they are and
repetitions are estimated at `secondsPerRepetition` each.
-}

import Plan exposing (Amount(..))
import Ziplist


{-| Estimated time to perform a single repetition.
-}
secondsPerRepetition : Int
secondsPerRepetition =
    2


secondsPerMinute : Int
secondsPerMinute =
    60


minutesPerHour : Int
minutesPerHour =
    60


{-| Estimated seconds an amount takes.
-}
amountSeconds : Amount -> Int
amountSeconds amount =
    case amount of
        Duration seconds ->
            seconds

        Count count ->
            count * secondsPerRepetition


{-| Estimated seconds of a workout made of `rounds` rounds of the given step
amounts, each round lasting `extraSecondsPerRound` more (such as the
pre-round delay).
-}
workoutSeconds : { rounds : Int, amounts : List Amount, extraSecondsPerRound : Int } -> Int
workoutSeconds { rounds, amounts, extraSecondsPerRound } =
    rounds * (extraSecondsPerRound + List.sum (List.map amountSeconds amounts))


{-| Estimated seconds of a day of a plan, with `extraSecondsPerRound` added to
each round.
-}
daysMapSeconds : Int -> Plan.DaysMap -> Int
daysMapSeconds extraSecondsPerRound daysMap =
    workoutSeconds
        { rounds = daysMap.rounds
        , amounts = List.map Plan.stepAmount (Ziplist.toList daysMap.steps)
        , extraSecondsPerRound = extraSecondsPerRound
        }


{-| Human readable duration: seconds under a minute (`45 s`), rounded minutes
under an hour (`12 min`) and hours with minutes beyond (`1 h 05 min`).
-}
format : Int -> String
format seconds =
    let
        minutes =
            round (toFloat seconds / toFloat secondsPerMinute)
    in
    if seconds < secondsPerMinute then
        String.fromInt seconds ++ " s"

    else if minutes < minutesPerHour then
        String.fromInt minutes ++ " min"

    else
        String.fromInt (minutes // minutesPerHour)
            ++ " h "
            ++ String.padLeft 2 '0' (String.fromInt (modBy minutesPerHour minutes))
            ++ " min"
