module Plan.EstimateTest exposing (suite)

{-| Unit tests for the `Plan.Estimate` module.
-}

import Expect
import Fixtures
import Plan exposing (Amount(..), Day(..))
import Plan.Estimate
import Test exposing (Test, describe, test)


secondsPerMinute : Int
secondsPerMinute =
    60


secondsPerHour : Int
secondsPerHour =
    3600


preRoundDelaySeconds : Int
preRoundDelaySeconds =
    10


{-| Estimate of a day of a fixture plan, `Nothing` when it doesn't decode or
has nothing planned that day.
-}
estimateOfDay : String -> Day -> Int -> Maybe Int
estimateOfDay yaml day extraSecondsPerRound =
    Plan.decode yaml
        |> Result.toMaybe
        |> Maybe.andThen (Plan.daysMap day)
        |> Maybe.map (Plan.Estimate.daysMapSeconds extraSecondsPerRound)


suite : Test
suite =
    describe "Plan.Estimate"
        [ describe "amountSeconds"
            [ test "durations count as they are" <|
                \_ -> Plan.Estimate.amountSeconds (Duration 40) |> Expect.equal 40
            , test "repetitions count two seconds each" <|
                \_ -> Plan.Estimate.amountSeconds (Count 12) |> Expect.equal 24
            ]
        , describe "workoutSeconds"
            [ test "multiplies one round by the rounds" <|
                \_ ->
                    Plan.Estimate.workoutSeconds { rounds = 3, amounts = [ Duration 30, Count 10, Duration 15 ], extraSecondsPerRound = 0 }
                        |> Expect.equal 195
            , test "adds the extra seconds to every round" <|
                \_ ->
                    Plan.Estimate.workoutSeconds { rounds = 2, amounts = [ Duration 30 ], extraSecondsPerRound = preRoundDelaySeconds }
                        |> Expect.equal 80
            , test "a workout without steps takes no time" <|
                \_ -> Plan.Estimate.workoutSeconds { rounds = 5, amounts = [], extraSecondsPerRound = 0 } |> Expect.equal 0
            ]
        , describe "daysMapSeconds"
            [ test "sums efforts and breaks of every round" <|
                \_ -> estimateOfDay Fixtures.duffPlanYaml Monday 0 |> Expect.equal (Just 315)
            , test "estimates repetitions" <|
                \_ -> estimateOfDay Fixtures.repetitionsPlanYaml Tuesday 0 |> Expect.equal (Just 78)
            , test "adds the extra seconds to every round" <|
                \_ -> estimateOfDay Fixtures.duffPlanYaml Monday preRoundDelaySeconds |> Expect.equal (Just 345)
            ]
        , describe "format"
            [ test "shows seconds under a minute" <|
                \_ -> Plan.Estimate.format 45 |> Expect.equal "45 s"
            , test "rounds to the nearest minute" <|
                \_ -> [ secondsPerMinute, 89, 90, 315 ] |> List.map Plan.Estimate.format |> Expect.equal [ "1 min", "1 min", "2 min", "5 min" ]
            , test "shows hours with padded minutes" <|
                \_ -> [ secondsPerHour, secondsPerHour + 5 * secondsPerMinute ] |> List.map Plan.Estimate.format |> Expect.equal [ "1 h 00 min", "1 h 05 min" ]
            , test "a duration rounding up to an hour shows in hours" <|
                \_ -> Plan.Estimate.format (secondsPerHour - 10) |> Expect.equal "1 h 00 min"
            ]
        ]
