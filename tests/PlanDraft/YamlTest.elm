module PlanDraft.YamlTest exposing (suite)

{-| Unit tests for the `PlanDraft.Yaml` module: plan files written by the
editor must be read back identically by the plan parser.
-}

import Difficulty exposing (Difficulty(..))
import Expect
import Fixtures
import Plan exposing (Amount(..), Day(..))
import PlanDraft exposing (Measure(..), PlanDraft, Schedule(..), StepKind(..))
import PlanDraft.Yaml
import Test exposing (Test, describe, test)
import Ziplist


{-| Texts that are not plain YAML scalars.
-}
trickyTexts : List String
trickyTexts =
    [ "Push: ups", "yes", "null", "#1 'Homer'", "- Bart", "Moe's tavern", "Ay caramba! 💪 é", "[Duff]", "{Duff}", "&anchor *alias", "% 100", "@itchy", "`scratchy`", "42", " Ned " ]


trickyDraft : String -> PlanDraft
trickyDraft text =
    PlanDraft.empty
        |> PlanDraft.setDescription text
        |> PlanDraft.setAuthorName text
        |> PlanDraft.setAuthorEmail text
        |> PlanDraft.setDifficulty Extreme
        |> PlanDraft.setRounds Friday 4
        |> PlanDraft.addStep Friday Effort
        |> PlanDraft.setStepName Friday 0 text
        |> PlanDraft.addStep Friday Break
        |> PlanDraft.setStepName Friday 1 text
        |> PlanDraft.addStep Friday Break
        |> PlanDraft.setStepSeconds Friday 2 45


roundTrip : PlanDraft -> Result String PlanDraft
roundTrip draft =
    PlanDraft.Yaml.decode (PlanDraft.Yaml.encode draft)


duffDraft : Result String PlanDraft
duffDraft =
    PlanDraft.Yaml.decode Fixtures.duffPlanYaml


suite : Test
suite =
    describe "PlanDraft.Yaml"
        [ describe "round trip"
            [ test "an empty draft" <|
                \_ -> roundTrip PlanDraft.empty |> Expect.equal (Ok PlanDraft.empty)
            , test "an every day plan" <|
                \_ ->
                    let
                        draft =
                            PlanDraft.Yaml.decode Fixtures.everyDayPlanYaml
                    in
                    Result.andThen roundTrip draft |> Expect.equal draft
            , test "an every day plan without steps" <|
                \_ ->
                    let
                        draft =
                            PlanDraft.setSchedule Monday EveryDay PlanDraft.empty
                    in
                    roundTrip draft |> Result.map PlanDraft.schedule |> Expect.equal (Ok EveryDay)
            , test "an every day plan is written as a workout" <|
                \_ ->
                    PlanDraft.Yaml.decode Fixtures.everyDayPlanYaml
                        |> Result.map (PlanDraft.Yaml.encode >> (\yaml -> ( String.contains "workout:" yaml, String.contains "planning:" yaml )))
                        |> Expect.equal (Ok ( True, False ))
            , test "a hosted plan" <|
                \_ -> Result.andThen roundTrip duffDraft |> Expect.equal duffDraft
            , test "a plan with an unnamed effort" <|
                \_ ->
                    let
                        draft =
                            PlanDraft.addStep Sunday Effort PlanDraft.empty
                    in
                    roundTrip draft |> Expect.equal (Ok draft)
            , test "a plan with efforts counted in repetitions" <|
                \_ ->
                    let
                        repetitionsDraft =
                            PlanDraft.Yaml.decode Fixtures.repetitionsPlanYaml
                    in
                    Result.andThen roundTrip repetitionsDraft |> Expect.equal repetitionsDraft
            , describe "texts that need quoting"
                (List.map
                    (\text -> test text <| \_ -> roundTrip (trickyDraft text) |> Expect.equal (Ok (trickyDraft text)))
                    trickyTexts
                )
            ]
        , describe "encode"
            [ test "leaves out days without steps" <|
                \_ ->
                    PlanDraft.empty
                        |> PlanDraft.setRounds Monday 3
                        |> PlanDraft.Yaml.encode
                        |> String.contains "monday"
                        |> Expect.equal False
            , test "writes the count of efforts measured in repetitions" <|
                \_ ->
                    PlanDraft.empty
                        |> PlanDraft.addStep Monday Effort
                        |> PlanDraft.setStepMeasure Monday 0 Repetitions
                        |> PlanDraft.setStepCount Monday 0 20
                        |> PlanDraft.Yaml.encode
                        |> (\yaml -> ( String.contains "count: 20" yaml, String.contains "seconds" yaml ))
                        |> Expect.equal ( True, False )
            , test "writes the seconds of breaks whatever their measure" <|
                \_ ->
                    PlanDraft.empty
                        |> PlanDraft.addStep Monday Effort
                        |> PlanDraft.setStepMeasure Monday 0 Repetitions
                        |> PlanDraft.setStepKind Monday 0 Break
                        |> PlanDraft.Yaml.toPlan
                        |> Result.map (\plan -> Plan.daysMap Monday plan |> Maybe.map (.steps >> Ziplist.toList >> List.map Plan.stepAmount))
                        |> Expect.equal (Ok (Just [ Duration 30 ]))
            , test "writes forkedFrom when forked" <|
                \_ ->
                    Fixtures.duffPlanYaml
                        ++ "forkedFrom: nicklayb/duff.yml\n"
                        |> PlanDraft.Yaml.decode
                        |> Result.map PlanDraft.Yaml.encode
                        |> Result.map (String.contains "forkedFrom: \"nicklayb/duff.yml\"")
                        |> Expect.equal (Ok True)
            ]
        , describe "toPlan"
            [ test "keeps the steps and rounds" <|
                \_ ->
                    trickyDraft "Bart"
                        |> PlanDraft.Yaml.toPlan
                        |> Result.map (\plan -> Plan.daysMap Friday plan |> Maybe.map (\daysMap -> ( daysMap.rounds, List.map Plan.stepName (Ziplist.toList daysMap.steps) )))
                        |> Expect.equal (Ok (Just ( 4, [ "Bart", "Bart", "Break" ] )))
            , test "leaves days without steps unplanned" <|
                \_ -> PlanDraft.Yaml.toPlan PlanDraft.empty |> Result.map (Plan.daysMap Monday) |> Expect.equal (Ok Nothing)
            ]
        , describe "fileName"
            [ test "is a slug of the description" <|
                \_ -> PlanDraft.setDescription "Krusty's Krazy Kardio!" PlanDraft.empty |> PlanDraft.Yaml.fileName |> Expect.equal "krusty-s-krazy-kardio.yml"
            , test "falls back when the description is empty" <|
                \_ -> PlanDraft.Yaml.fileName PlanDraft.empty |> Expect.equal "plan.yml"
            ]
        , test "decode rejects invalid YAML" <|
            \_ ->
                case PlanDraft.Yaml.decode Fixtures.invalidYaml of
                    Ok _ ->
                        Expect.fail "Expected invalid YAML to be rejected"

                    Err _ ->
                        Expect.pass
        ]
