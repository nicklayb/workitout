module CustomPlansTest exposing (suite)

{-| Unit tests for the `CustomPlans` module: identifiers and versioned
persistence.
-}

import CustomPlans exposing (CustomPlans)
import Expect
import PlanDraft exposing (PlanDraft)
import Test exposing (Test, describe, test)


draftNamed : String -> PlanDraft
draftNamed description =
    PlanDraft.setDescription description PlanDraft.empty


{-| Three plans with identifiers 1, 2 and 3.
-}
springfieldPlans : CustomPlans
springfieldPlans =
    [ "Homer", "Marge", "Bart" ]
        |> List.foldl (\name plans -> CustomPlans.insert (draftNamed name) plans |> Tuple.second) CustomPlans.empty


descriptions : CustomPlans -> List String
descriptions plans =
    CustomPlans.toList plans |> List.map (.draft >> PlanDraft.description)


expectInvalid : String -> Expect.Expectation
expectInvalid json =
    case CustomPlans.decodeString json of
        Ok _ ->
            Expect.fail ("Expected invalid plans: " ++ json)

        Err _ ->
            Expect.pass


suite : Test
suite =
    describe "CustomPlans"
        [ describe "insert"
            [ test "starts identifiers at one" <|
                \_ -> CustomPlans.insert PlanDraft.empty CustomPlans.empty |> Tuple.first |> Expect.equal 1
            , test "keeps the creation order" <|
                \_ -> descriptions springfieldPlans |> Expect.equal [ "Homer", "Marge", "Bart" ]
            , test "never reuses the highest identifier after a removal" <|
                \_ -> springfieldPlans |> CustomPlans.remove 2 |> CustomPlans.insert PlanDraft.empty |> Tuple.first |> Expect.equal 4
            ]
        , test "get finds a plan by identifier" <|
            \_ -> CustomPlans.get 2 springfieldPlans |> Maybe.map PlanDraft.description |> Expect.equal (Just "Marge")
        , test "update changes only the given plan" <|
            \_ -> CustomPlans.update 3 (PlanDraft.setDescription "Lisa") springfieldPlans |> descriptions |> Expect.equal [ "Homer", "Marge", "Lisa" ]
        , test "remove drops only the given plan" <|
            \_ -> CustomPlans.remove 1 springfieldPlans |> descriptions |> Expect.equal [ "Marge", "Bart" ]
        , describe "decodeString"
            [ test "round trips through toString" <|
                \_ -> CustomPlans.toString springfieldPlans |> CustomPlans.decodeString |> Expect.equal (Ok springfieldPlans)
            , test "rejects an unknown version" <|
                \_ -> expectInvalid """{ "version": 742, "plans": [] }"""
            , test "rejects an invalid plan" <|
                \_ -> expectInvalid """{ "version": 1, "plans": [ { "id": 1, "yaml": "description: [Duff" } ] }"""
            , test "rejects duplicated identifiers" <|
                \_ ->
                    CustomPlans.toString springfieldPlans
                        |> String.replace "\"id\":2" "\"id\":1"
                        |> expectInvalid
            , test "rejects malformed JSON" <|
                \_ -> expectInvalid "{ \"version\": 1"
            ]
        ]
