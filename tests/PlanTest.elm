module PlanTest exposing (suite)

{-| Unit tests for the `Plan` module: YAML decoding, step accessors, author
naming and day helpers.
-}

import Dict
import Difficulty exposing (Difficulty(..))
import Expect
import Fixtures
import Plan exposing (Amount(..), Day(..), Plan, Step)
import Test exposing (Test, describe, test)
import Time
import Ziplist


allDays : List Day
allDays =
    [ Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday ]


emptyAuthor : { github : Maybe String, name : Maybe String, email : Maybe String }
emptyAuthor =
    { github = Nothing, name = Nothing, email = Nothing }


fallbackAuthorName : String
fallbackAuthorName =
    "Anonymous Springfieldian"


defaultBreakName : String
defaultBreakName =
    "Break"


decodedPlan : Result String Plan
decodedPlan =
    Plan.decode Fixtures.duffPlanYaml


stepsOfDay : Day -> Plan -> List Step
stepsOfDay day plan =
    Plan.daysMap day plan
        |> Maybe.map (.steps >> Ziplist.map (\_ step -> step))
        |> Maybe.withDefault []


mondaySteps : List Step
mondaySteps =
    decodedPlan
        |> Result.map (stepsOfDay Monday)
        |> Result.withDefault []


{-| Runs an expectation on a decoded plan, failing with the decoding error otherwise.
-}
expectDecodedPlan : (Plan -> Expect.Expectation) -> Expect.Expectation
expectDecodedPlan expectation =
    case decodedPlan of
        Ok plan ->
            expectation plan

        Err error ->
            Expect.fail error


suite : Test
suite =
    describe "Plan"
        [ decodeTests
        , stepTests
        , authorNameTests
        , dayTests
        , weekdaysTests
        , daysMapTests
        ]


decodeTests : Test
decodeTests =
    describe "decode"
        [ test "decodes the plan description" <|
            \_ -> expectDecodedPlan (.description >> Expect.equal Fixtures.planDescription)
        , test "has no forkedFrom when absent" <|
            \_ -> expectDecodedPlan (.forkedFrom >> Expect.equal Nothing)
        , test "decodes forkedFrom" <|
            \_ ->
                Plan.decode (Fixtures.duffPlanYaml ++ "forkedFrom: chunkylover53/duff.yml\n")
                    |> Result.map .forkedFrom
                    |> Expect.equal (Ok (Just "chunkylover53/duff.yml"))
        , test "decodes the author" <|
            \_ ->
                expectDecodedPlan
                    (.author
                        >> Expect.equal
                            { github = Just Fixtures.authorGithub
                            , name = Just Fixtures.authorName
                            , email = Just Fixtures.authorEmail
                            }
                    )
        , test "decodes the difficulty" <|
            \_ -> expectDecodedPlan (.difficulty >> Expect.equal Hard)
        , test "defaults the difficulty when it is missing" <|
            \_ ->
                Plan.decode Fixtures.planWithoutDifficultyYaml
                    |> Result.map .difficulty
                    |> Expect.equal (Ok Difficulty.default)
        , test "decodes rounds of scheduled days" <|
            \_ ->
                expectDecodedPlan
                    (\plan ->
                        [ Plan.daysMap Monday plan, Plan.daysMap Wednesday plan ]
                            |> List.map (Maybe.map .rounds)
                            |> Expect.equal [ Just Fixtures.mondayRounds, Just Fixtures.wednesdayRounds ]
                    )
        , test "leaves unscheduled days empty and ignores unknown day keys" <|
            \_ ->
                expectDecodedPlan
                    (\plan ->
                        [ Tuesday, Thursday, Friday, Saturday, Sunday ]
                            |> List.filterMap (\day -> Plan.daysMap day plan)
                            |> Expect.equal []
                    )
        , test "decodes every step of a day in order" <|
            \_ ->
                mondaySteps
                    |> List.map Plan.stepName
                    |> Expect.equal [ Fixtures.firstEffortName, "Duff break", defaultBreakName, Fixtures.secondEffortName ]
        , test "a plan with a planning is scheduled weekly" <|
            \_ -> expectDecodedPlan (Plan.isScheduledWeekly >> Expect.equal True)
        , test "decodes a workout as the same workout every day" <|
            \_ ->
                Plan.decode Fixtures.everyDayPlanYaml
                    |> Result.map (\plan -> List.map (\day -> Plan.daysMap day plan |> Maybe.map (\daysMap -> ( daysMap.rounds, List.map Plan.stepName (Ziplist.toList daysMap.steps) ))) allDays)
                    |> Expect.equal (Ok (List.repeat (List.length allDays) (Just ( Fixtures.everyDayRounds, [ "Squishee squats", defaultBreakName ] ))))
        , test "a plan with a workout isn't scheduled weekly" <|
            \_ ->
                Plan.decode Fixtures.everyDayPlanYaml
                    |> Result.map Plan.isScheduledWeekly
                    |> Expect.equal (Ok False)
        , test "fails when both a planning and a workout are given" <|
            \_ -> Plan.decode Fixtures.planningAndWorkoutYaml |> Expect.err
        , test "fails when neither a planning nor a workout is given" <|
            \_ -> Plan.decode Fixtures.planWithoutScheduleYaml |> Expect.err
        , test "fails when the required description is missing" <|
            \_ ->
                Plan.decode Fixtures.planMissingDescriptionYaml
                    |> Expect.err
        , test "drops a day whose effort has both seconds and count" <|
            \_ ->
                Plan.decode Fixtures.effortWithSecondsAndCountYaml
                    |> Result.map (Plan.daysMap Monday)
                    |> Expect.equal (Ok Nothing)
        , test "drops a day whose effort has neither seconds nor count" <|
            \_ ->
                Plan.decode Fixtures.effortWithoutAmountYaml
                    |> Result.map (Plan.daysMap Monday)
                    |> Expect.equal (Ok Nothing)
        , test "fails on malformed YAML" <|
            \_ ->
                Plan.decode Fixtures.invalidYaml
                    |> Expect.err
        ]


stepTests : Test
stepTests =
    describe "steps"
        [ test "stepAmount returns the duration of efforts and breaks" <|
            \_ ->
                mondaySteps
                    |> List.map Plan.stepAmount
                    |> Expect.equal (List.map Duration [ Fixtures.firstEffortSeconds, Fixtures.breakSeconds, Fixtures.unnamedBreakSeconds, Fixtures.secondEffortSeconds ])
        , test "stepAmount returns the repetitions of counted efforts" <|
            \_ ->
                Plan.decode Fixtures.repetitionsPlanYaml
                    |> Result.map (stepsOfDay Tuesday >> List.map Plan.stepAmount)
                    |> Expect.equal (Ok [ Count Fixtures.repetitionsEffortCount, Duration Fixtures.breakSeconds ])
        , test "stepDescription returns the optional description" <|
            \_ ->
                mondaySteps
                    |> List.map Plan.stepDescription
                    |> Expect.equal [ Just Fixtures.effortDescription, Just Fixtures.breakDescription, Nothing, Nothing ]
        , test "isBreakStep only matches steps typed as break" <|
            \_ ->
                mondaySteps
                    |> List.map Plan.isBreakStep
                    |> Expect.equal [ False, True, True, False ]
        , test "stepName falls back to the default break name" <|
            \_ ->
                mondaySteps
                    |> List.filter Plan.isBreakStep
                    |> List.map Plan.stepName
                    |> Expect.equal [ "Duff break", defaultBreakName ]
        ]


authorNameTests : Test
authorNameTests =
    describe "authorName"
        [ test "prefers the name" <|
            \_ ->
                Plan.authorName fallbackAuthorName
                    { github = Just Fixtures.authorGithub, name = Just Fixtures.authorName, email = Just Fixtures.authorEmail }
                    |> Expect.equal Fixtures.authorName
        , test "falls back to the github handle" <|
            \_ ->
                Plan.authorName fallbackAuthorName
                    { emptyAuthor | github = Just Fixtures.authorGithub, email = Just Fixtures.authorEmail }
                    |> Expect.equal Fixtures.authorGithub
        , test "falls back to the email" <|
            \_ ->
                Plan.authorName fallbackAuthorName { emptyAuthor | email = Just Fixtures.authorEmail }
                    |> Expect.equal Fixtures.authorEmail
        , test "falls back to the given default" <|
            \_ ->
                Plan.authorName fallbackAuthorName emptyAuthor
                    |> Expect.equal fallbackAuthorName
        ]


dayTests : Test
dayTests =
    describe "days"
        [ test "dayFromString reverses dayToString for every day" <|
            \_ ->
                allDays
                    |> List.map (Plan.dayToString >> Plan.dayFromString)
                    |> Expect.equal (List.map Just allDays)
        , test "dayToString returns lowercase english names" <|
            \_ ->
                allDays
                    |> List.map Plan.dayToString
                    |> Expect.equal [ "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday" ]
        , test "dayFromString rejects unknown and capitalized days" <|
            \_ ->
                [ "krustyday", "Monday", "" ]
                    |> List.map Plan.dayFromString
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "dayEnum agrees with dayToString" <|
            \_ ->
                allDays
                    |> List.map (\day -> Dict.get (Plan.dayToString day) Plan.dayEnum)
                    |> Expect.equal (List.map Just allDays)
        , test "dayFromTimeDay maps every Time.Weekday" <|
            \_ ->
                [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat, Time.Sun ]
                    |> List.map Plan.dayFromTimeDay
                    |> Expect.equal allDays
        , test "weekdayEquals is true only for identical days" <|
            \_ ->
                allDays
                    |> List.concatMap (\left -> List.map (\right -> ( left, right )) allDays)
                    |> List.filter (\( left, right ) -> Plan.weekdayEquals left right)
                    |> Expect.equal (List.map (\day -> ( day, day )) allDays)
        ]


weekdaysTests : Test
weekdaysTests =
    describe "weekdays"
        [ test "every day has a workout in a plan scheduled every day" <|
            \_ ->
                Plan.decode Fixtures.everyDayPlanYaml
                    |> Result.map (Plan.weekdays >> List.all Tuple.second)
                    |> Expect.equal (Ok True)
        , test "lists every day starting on Sunday with its scheduled flag" <|
            \_ ->
                expectDecodedPlan
                    (Plan.weekdays
                        >> Expect.equal
                            [ ( Sunday, False )
                            , ( Monday, True )
                            , ( Tuesday, False )
                            , ( Wednesday, True )
                            , ( Thursday, False )
                            , ( Friday, False )
                            , ( Saturday, False )
                            ]
                    )
        ]


daysMapTests : Test
daysMapTests =
    describe "daysMapNextStep"
        [ test "advances to the next step" <|
            \_ ->
                expectDecodedPlan
                    (Plan.daysMap Monday
                        >> Maybe.map (Plan.daysMapNextStep >> .steps >> Ziplist.current >> Maybe.map Plan.stepName)
                        >> Expect.equal (Just (Just "Duff break"))
                    )
        , test "keeps the rounds untouched" <|
            \_ ->
                expectDecodedPlan
                    (Plan.daysMap Monday
                        >> Maybe.map (Plan.daysMapNextStep >> .rounds)
                        >> Expect.equal (Just Fixtures.mondayRounds)
                    )
        , test "wraps back to the first step after the last one" <|
            \_ ->
                expectDecodedPlan
                    (Plan.daysMap Monday
                        >> Maybe.map
                            (\daysMap ->
                                List.foldl (\_ map -> Plan.daysMapNextStep map) daysMap (List.range 1 (List.length mondaySteps))
                                    |> .steps
                                    |> Ziplist.isReset
                            )
                        >> Expect.equal (Just True)
                    )
        ]
