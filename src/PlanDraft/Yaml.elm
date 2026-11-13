module PlanDraft.Yaml exposing (decode, encode, fileName, mimeType, toPlan)

{-| Conversion of a draft from and to the YAML format of the plans hosted on
the `plans` branch.
-}

import BoundedInt
import Difficulty
import Plan exposing (Amount(..), Plan)
import PlanDraft exposing (DayDraft, PlanDraft, Schedule(..), StepDraft, StepKind(..))
import Yaml.Encode as Encode exposing (Encoder)


indentation : Int
indentation =
    2


fallbackFileName : String
fallbackFileName =
    "plan"


{-| MIME type of an exported plan.
-}
mimeType : String
mimeType =
    "application/yaml"


{-| Parses a plan file into a draft.
-}
decode : String -> Result String PlanDraft
decode yaml =
    Plan.decode yaml
        |> Result.map PlanDraft.fromPlan


{-| Converts a draft into a runnable plan, going through the plan file format
so it gets exactly what a hosted plan would.
-}
toPlan : PlanDraft -> Result String Plan
toPlan draft =
    Plan.decode (encode draft)


{-| Writes a draft as a plan file. Days without any step are left out, as is
the schedule not in use.
-}
encode : PlanDraft -> String
encode draft =
    Encode.toString indentation (planEncoder draft) ++ "\n"


{-| Name of the exported file, derived from the description.
-}
fileName : PlanDraft -> String
fileName draft =
    let
        slug =
            PlanDraft.description draft
                |> String.toLower
                |> String.map
                    (\character ->
                        if Char.isAlphaNum character then
                            character

                        else
                            ' '
                    )
                |> String.words
                |> String.join "-"
    in
    (if String.isEmpty slug then
        fallbackFileName

     else
        slug
    )
        ++ ".yml"


{-| Strings are always double quoted so values such as `yes`, `null` or
`Push: ups` stay strings. `PlanDraft.sanitizeText` guarantees there is
nothing to escape.
-}
quoted : String -> Encoder
quoted text =
    Encode.string ("\"" ++ PlanDraft.sanitizeText text ++ "\"")


planEncoder : PlanDraft -> Encoder
planEncoder draft =
    Encode.record
        (List.concat
            [ [ ( "description", quoted (PlanDraft.description draft) )
              , ( "difficulty", Encode.string (Difficulty.toString (PlanDraft.difficulty draft)) )
              ]
            , optionalField "forkedFrom" (PlanDraft.forkedFrom draft)
            , [ ( "author", authorEncoder draft )
              , scheduleField draft
              ]
            ]
        )


authorEncoder : PlanDraft -> Encoder
authorEncoder draft =
    Encode.record
        (List.concat
            [ optionalField "name" (nonEmpty (PlanDraft.authorName draft))
            , optionalField "email" (nonEmpty (PlanDraft.authorEmail draft))
            , optionalField "github" (PlanDraft.authorGithub draft)
            ]
        )


{-| A weekly `planning`, or the `workout` of every day.
-}
scheduleField : PlanDraft -> ( String, Encoder )
scheduleField draft =
    case PlanDraft.schedule draft of
        Weekly ->
            ( "planning", Encode.record [ ( "days", daysEncoder draft ) ] )

        EveryDay ->
            ( "workout", dayEncoder (PlanDraft.everyDay draft) )


daysEncoder : PlanDraft -> Encoder
daysEncoder draft =
    Plan.allDays
        |> List.filterMap
            (\day ->
                let
                    dayDraft =
                        PlanDraft.day day draft
                in
                if List.isEmpty dayDraft.steps then
                    Nothing

                else
                    Just ( Plan.dayToString day, dayEncoder dayDraft )
            )
        |> Encode.record


dayEncoder : DayDraft -> Encoder
dayEncoder dayDraft =
    Encode.record
        [ ( "rounds", Encode.int (BoundedInt.value dayDraft.rounds) )
        , ( "steps", Encode.list stepEncoder dayDraft.steps )
        ]


stepEncoder : StepDraft -> Encoder
stepEncoder step =
    let
        kindField =
            case step.kind of
                Effort ->
                    []

                Break ->
                    [ ( "type", Encode.string "break" ) ]

        nameField =
            case ( step.kind, nonEmpty step.name ) of
                ( Break, Nothing ) ->
                    []

                _ ->
                    [ ( "name", quoted step.name ) ]
    in
    Encode.record
        (List.concat
            [ nameField
            , kindField
            , [ amountField step ]
            , optionalField "description" step.description
            ]
        )


amountField : StepDraft -> ( String, Encoder )
amountField step =
    case PlanDraft.stepAmount step of
        Count count ->
            ( "count", Encode.int count )

        Duration seconds ->
            ( "seconds", Encode.int seconds )


optionalField : String -> Maybe String -> List ( String, Encoder )
optionalField key maybeValue =
    case maybeValue of
        Just value ->
            [ ( key, quoted value ) ]

        Nothing ->
            []


nonEmpty : String -> Maybe String
nonEmpty text =
    if String.isEmpty (String.trim text) then
        Nothing

    else
        Just text
