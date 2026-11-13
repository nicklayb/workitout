module PlanDraftTest exposing (suite)

{-| Unit tests for the `PlanDraft` module: building from a plan, editing,
sanitizing and capping.
-}

import BoundedInt
import Difficulty exposing (Difficulty(..))
import Expect
import Fixtures
import Plan exposing (Amount(..), Day(..))
import PlanDraft exposing (Measure(..), PlanDraft, Schedule(..), StepDraft, StepKind(..))
import Test exposing (Test, describe, test)


duffDraft : PlanDraft
duffDraft =
    Plan.decode Fixtures.duffPlanYaml
        |> Result.map PlanDraft.fromPlan
        |> Result.withDefault PlanDraft.empty


{-| A Monday made of three efforts named after the Simpsons kids.
-}
kidsDraft : PlanDraft
kidsDraft =
    PlanDraft.empty
        |> PlanDraft.addStep Monday Effort
        |> PlanDraft.addStep Monday Effort
        |> PlanDraft.addStep Monday Effort
        |> PlanDraft.setStepName Monday 0 "Bart"
        |> PlanDraft.setStepName Monday 1 "Lisa"
        |> PlanDraft.setStepName Monday 2 "Maggie"


mondayStepNames : PlanDraft -> List String
mondayStepNames draft =
    PlanDraft.day Monday draft |> .steps |> List.map .name


mondayRounds : PlanDraft -> Int
mondayRounds draft =
    PlanDraft.day Monday draft |> .rounds |> BoundedInt.value


firstMondaySeconds : PlanDraft -> Maybe Int
firstMondaySeconds draft =
    PlanDraft.day Monday draft |> .steps |> List.head |> Maybe.map (.seconds >> BoundedInt.value)


firstMondayStep : PlanDraft -> Maybe StepDraft
firstMondayStep draft =
    PlanDraft.day Monday draft |> .steps |> List.head


firstMondayCount : PlanDraft -> Maybe Int
firstMondayCount draft =
    firstMondayStep draft |> Maybe.map (.count >> BoundedInt.value)


repetitionsDraft : PlanDraft
repetitionsDraft =
    Plan.decode Fixtures.repetitionsPlanYaml
        |> Result.map PlanDraft.fromPlan
        |> Result.withDefault PlanDraft.empty


everyDayDraft : PlanDraft
everyDayDraft =
    Plan.decode Fixtures.everyDayPlanYaml
        |> Result.map PlanDraft.fromPlan
        |> Result.withDefault PlanDraft.empty


stepNames : Day -> PlanDraft -> List String
stepNames day draft =
    PlanDraft.day day draft |> .steps |> List.map .name


scheduleTests : Test
scheduleTests =
    describe "schedule"
        [ test "an empty draft is scheduled weekly" <|
            \_ -> PlanDraft.schedule PlanDraft.empty |> Expect.equal Weekly
        , test "fromPlan keeps a workout for every day" <|
            \_ -> ( PlanDraft.schedule everyDayDraft, stepNames Thursday everyDayDraft ) |> Expect.equal ( EveryDay, [ "Squishee squats", "" ] )
        , test "every day plans give the same workout whatever the day" <|
            \_ -> List.map (\day -> stepNames day everyDayDraft) [ Monday, Saturday ] |> Expect.equal [ [ "Squishee squats", "" ], [ "Squishee squats", "" ] ]
        , test "edits of an every day plan apply to every day" <|
            \_ -> PlanDraft.setStepName Monday 0 "Kwik-E squats" everyDayDraft |> stepNames Friday |> Expect.equal [ "Kwik-E squats", "" ]
        , test "switching to every day starts from the given day's workout" <|
            \_ -> PlanDraft.setSchedule Monday EveryDay kidsDraft |> stepNames Sunday |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
        , test "switching to every day keeps a workout that already has steps" <|
            \_ ->
                PlanDraft.setSchedule Monday Weekly everyDayDraft
                    |> PlanDraft.setStepName Monday 0 "Homer"
                    |> PlanDraft.setSchedule Monday EveryDay
                    |> stepNames Monday
                    |> Expect.equal [ "Squishee squats", "" ]
        , test "switching back to weekly keeps the weekday workouts" <|
            \_ ->
                kidsDraft
                    |> PlanDraft.setSchedule Monday EveryDay
                    |> PlanDraft.removeStep Monday 0
                    |> PlanDraft.setSchedule Monday Weekly
                    |> stepNames Monday
                    |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
        , test "switching to weekly starts the given day from the every day workout" <|
            \_ -> PlanDraft.setSchedule Wednesday Weekly everyDayDraft |> stepNames Wednesday |> Expect.equal [ "Squishee squats", "" ]
        , test "switching to weekly leaves the other days empty" <|
            \_ -> PlanDraft.setSchedule Wednesday Weekly everyDayDraft |> stepNames Thursday |> Expect.equal []
        ]


suite : Test
suite =
    describe "PlanDraft"
        [ scheduleTests
        , describe "empty"
            [ test "has no step on any day" <|
                \_ -> List.map (\day -> PlanDraft.effortCount day PlanDraft.empty) Plan.allDays |> Expect.equal (List.repeat 7 0)
            , test "a day defaults to a single round" <|
                \_ -> mondayRounds PlanDraft.empty |> Expect.equal 1
            ]
        , describe "fromPlan"
            [ test "keeps the plan information" <|
                \_ ->
                    ( ( PlanDraft.description duffDraft, PlanDraft.authorName duffDraft, PlanDraft.authorEmail duffDraft )
                    , ( PlanDraft.authorGithub duffDraft, PlanDraft.difficulty duffDraft )
                    )
                        |> Expect.equal
                            ( ( Fixtures.planDescription, Fixtures.authorName, Fixtures.authorEmail )
                            , ( Just Fixtures.authorGithub, Hard )
                            )
            , test "keeps the steps in order, unnamed breaks having an empty name" <|
                \_ -> mondayStepNames duffDraft |> Expect.equal [ Fixtures.firstEffortName, "Duff break", "", Fixtures.secondEffortName ]
            , test "keeps the rounds" <|
                \_ -> mondayRounds duffDraft |> Expect.equal Fixtures.mondayRounds
            , test "counts only efforts" <|
                \_ -> PlanDraft.effortCount Monday duffDraft |> Expect.equal 2
            , test "keeps efforts counted in repetitions" <|
                \_ ->
                    PlanDraft.day Tuesday repetitionsDraft
                        |> .steps
                        |> List.map (\step -> ( step.measure, BoundedInt.value step.count ))
                        |> List.head
                        |> Expect.equal (Just ( Repetitions, Fixtures.repetitionsEffortCount ))
            , test "keeps step descriptions" <|
                \_ -> PlanDraft.day Monday duffDraft |> .steps |> List.head |> Maybe.andThen .description |> Expect.equal (Just Fixtures.effortDescription)
            ]
        , describe "forkFrom"
            [ test "an empty draft isn't forked" <|
                \_ -> PlanDraft.forkedFrom PlanDraft.empty |> Expect.equal Nothing
            , test "points to the forked plan path" <|
                \_ -> PlanDraft.forkFrom "nicklayb/duff.yml" duffDraft |> PlanDraft.forkedFrom |> Expect.equal (Just "nicklayb/duff.yml")
            ]
        , describe "steps"
            [ test "addStep appends with a default duration" <|
                \_ -> firstMondaySeconds kidsDraft |> Expect.equal (Just 30)
            , test "moveStepUp swaps with the previous step" <|
                \_ -> PlanDraft.moveStepUp Monday 1 kidsDraft |> mondayStepNames |> Expect.equal [ "Lisa", "Bart", "Maggie" ]
            , test "moveStepUp ignores the first step" <|
                \_ -> PlanDraft.moveStepUp Monday 0 kidsDraft |> mondayStepNames |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
            , test "moveStepDown swaps with the next step" <|
                \_ -> PlanDraft.moveStepDown Monday 1 kidsDraft |> mondayStepNames |> Expect.equal [ "Bart", "Maggie", "Lisa" ]
            , test "moveStepDown ignores the last step" <|
                \_ -> PlanDraft.moveStepDown Monday 2 kidsDraft |> mondayStepNames |> Expect.equal [ "Bart", "Lisa", "Maggie" ]
            , test "removeStep removes only the given step" <|
                \_ -> PlanDraft.removeStep Monday 1 kidsDraft |> mondayStepNames |> Expect.equal [ "Bart", "Maggie" ]
            , test "setStepKind turns an effort into a break" <|
                \_ -> PlanDraft.setStepKind Monday 0 Break kidsDraft |> PlanDraft.effortCount Monday |> Expect.equal 2
            , test "addStep measures efforts in seconds" <|
                \_ -> firstMondayStep kidsDraft |> Maybe.map .measure |> Expect.equal (Just Seconds)
            , test "setStepMeasure switches an effort to repetitions" <|
                \_ -> PlanDraft.setStepMeasure Monday 0 Repetitions kidsDraft |> firstMondayStep |> Maybe.map .measure |> Expect.equal (Just Repetitions)
            , test "setStepMeasure keeps the duration for when switching back" <|
                \_ ->
                    kidsDraft
                        |> PlanDraft.setStepSeconds Monday 0 45
                        |> PlanDraft.setStepMeasure Monday 0 Repetitions
                        |> PlanDraft.setStepMeasure Monday 0 Seconds
                        |> firstMondaySeconds
                        |> Expect.equal (Just 45)
            , test "setStepCount sets the repetitions" <|
                \_ -> PlanDraft.setStepCount Monday 0 15 kidsDraft |> firstMondayCount |> Expect.equal (Just 15)
            , test "stepAmount of an effort follows its measure" <|
                \_ ->
                    kidsDraft
                        |> PlanDraft.setStepCount Monday 0 15
                        |> PlanDraft.setStepMeasure Monday 0 Repetitions
                        |> firstMondayStep
                        |> Maybe.map PlanDraft.stepAmount
                        |> Expect.equal (Just (Count 15))
            , test "stepAmount of a break is always a duration" <|
                \_ ->
                    kidsDraft
                        |> PlanDraft.setStepMeasure Monday 0 Repetitions
                        |> PlanDraft.setStepKind Monday 0 Break
                        |> firstMondayStep
                        |> Maybe.map PlanDraft.stepAmount
                        |> Expect.equal (Just (Duration 30))
            , test "other days are left untouched" <|
                \_ -> PlanDraft.effortCount Tuesday kidsDraft |> Expect.equal 0
            ]
        , describe "capping"
            [ test "step seconds are capped to one hour" <|
                \_ -> PlanDraft.setStepSeconds Monday 0 7200 kidsDraft |> firstMondaySeconds |> Expect.equal (Just 3600)
            , test "step seconds can't go below one" <|
                \_ -> PlanDraft.setStepSeconds Monday 0 0 kidsDraft |> firstMondaySeconds |> Expect.equal (Just 1)
            , test "step repetitions are capped" <|
                \_ -> [ 0, 5000 ] |> List.map (\count -> PlanDraft.setStepCount Monday 0 count kidsDraft |> firstMondayCount) |> Expect.equal [ Just 1, Just 999 ]
            , test "rounds are capped" <|
                \_ -> [ 0, 500 ] |> List.map (\rounds -> PlanDraft.setRounds Monday rounds kidsDraft |> mondayRounds) |> Expect.equal [ 1, 99 ]
            ]
        , describe "sanitizeText"
            [ test "keeps regular text" <|
                \_ -> PlanDraft.sanitizeText "Moe's: #1 tavern" |> Expect.equal "Moe's: #1 tavern"
            , test "turns double quotes into single quotes and backslashes into slashes" <|
                \_ -> PlanDraft.sanitizeText "\"Duff\" \\ Fudd" |> Expect.equal "'Duff' / Fudd"
            , test "turns line breaks and tabs into spaces and merges spaces" <|
                \_ -> PlanDraft.sanitizeText "Krusty\n\tthe   clown " |> Expect.equal "Krusty the clown "
            , test "is applied by setters" <|
                \_ -> PlanDraft.setDescription "Itchy\nand \"Scratchy\"" PlanDraft.empty |> PlanDraft.description |> Expect.equal "Itchy and 'Scratchy'"
            ]
        ]
