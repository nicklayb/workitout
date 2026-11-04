module PlanDraft exposing
    ( DayDraft
    , PlanDraft
    , StepDraft
    , StepKind(..)
    , addStep
    , authorEmail
    , authorGithub
    , authorName
    , day
    , description
    , difficulty
    , effortCount
    , empty
    , forkFrom
    , forkedFrom
    , fromPlan
    , moveStepDown
    , moveStepUp
    , removeStep
    , roundsBounds
    , sanitizeText
    , secondsBounds
    , setAuthorEmail
    , setAuthorName
    , setDescription
    , setDifficulty
    , setRounds
    , setStepKind
    , setStepName
    , setStepSeconds
    )

{-| Editable version of a plan, built by the plan editor.

The type is opaque so every change goes through setters keeping it valid:
texts are sanitized so they survive a YAML round trip and integers are capped
to their bounds.

-}

import BoundedInt exposing (BoundedInt, Bounds)
import Dict exposing (Dict)
import Difficulty exposing (Difficulty)
import Plan exposing (Day, Plan, Step(..))
import Ziplist


type PlanDraft
    = PlanDraft
        { description : String
        , authorName : String
        , authorEmail : String
        , authorGithub : Maybe String
        , difficulty : Difficulty
        , forkedFrom : Maybe String
        , dayByName : Dict String DayDraft
        }


{-| Planning of a single day: its steps are repeated `rounds` times.
-}
type alias DayDraft =
    { rounds : BoundedInt
    , steps : List StepDraft
    }


{-| A step of a day. `description` is not editable, it is only kept from
imported or forked plans.
-}
type alias StepDraft =
    { kind : StepKind
    , name : String
    , seconds : BoundedInt
    , description : Maybe String
    }


type StepKind
    = Effort
    | Break


maximumRounds : Int
maximumRounds =
    99


secondsPerHour : Int
secondsPerHour =
    3600


defaultEffortSeconds : Int
defaultEffortSeconds =
    30


defaultBreakSeconds : Int
defaultBreakSeconds =
    10


{-| Allowed number of rounds of a day.
-}
roundsBounds : Bounds
roundsBounds =
    { minimum = 1, maximum = maximumRounds }


{-| Allowed duration of a step, in seconds. A step can't last zero seconds
since the timer would never finish.
-}
secondsBounds : Bounds
secondsBounds =
    { minimum = 1, maximum = secondsPerHour }


{-| A plan without any information nor step.
-}
empty : PlanDraft
empty =
    PlanDraft
        { description = ""
        , authorName = ""
        , authorEmail = ""
        , authorGithub = Nothing
        , difficulty = Difficulty.default
        , forkedFrom = Nothing
        , dayByName = Dict.empty
        }


emptyDay : DayDraft
emptyDay =
    { rounds = BoundedInt.init roundsBounds roundsBounds.minimum, steps = [] }


{-| Builds a draft out of a plan, sanitizing its texts and capping its values.
-}
fromPlan : Plan -> PlanDraft
fromPlan plan =
    let
        dayByName =
            Plan.allDays
                |> List.filterMap (\planDay -> Plan.daysMap planDay plan |> Maybe.map (\daysMap -> ( Plan.dayToString planDay, dayFromDaysMap daysMap )))
                |> Dict.fromList
    in
    PlanDraft
        { description = sanitizeText plan.description
        , authorName = sanitizeMaybe plan.author.name
        , authorEmail = sanitizeMaybe plan.author.email
        , authorGithub = Maybe.map sanitizeText plan.author.github
        , difficulty = plan.difficulty
        , forkedFrom = Maybe.map sanitizeText plan.forkedFrom
        , dayByName = dayByName
        }


sanitizeMaybe : Maybe String -> String
sanitizeMaybe maybeText =
    Maybe.map sanitizeText maybeText |> Maybe.withDefault ""


dayFromDaysMap : Plan.DaysMap -> DayDraft
dayFromDaysMap daysMap =
    { rounds = BoundedInt.init roundsBounds daysMap.rounds
    , steps = List.map stepFromPlanStep (Ziplist.toList daysMap.steps)
    }


stepFromPlanStep : Step -> StepDraft
stepFromPlanStep step =
    case step of
        Plan.Effort spec ->
            newStep Effort (sanitizeText spec.name) spec.seconds spec.description

        Plan.Break spec ->
            newStep Break (sanitizeMaybe spec.name) spec.seconds spec.description


newStep : StepKind -> String -> Int -> Maybe String -> StepDraft
newStep kind name seconds maybeDescription =
    { kind = kind
    , name = name
    , seconds = BoundedInt.init secondsBounds seconds
    , description = Maybe.map sanitizeText maybeDescription
    }


{-| Makes a text representable in the plan YAML files: the YAML parser in use
supports neither escape sequences nor line breaks in strings and merges
consecutive spaces, so double quotes become single quotes, backslashes become
slashes, line breaks and tabs become spaces and consecutive spaces are merged.
-}
sanitizeText : String -> String
sanitizeText text =
    text
        |> String.replace "\"" "'"
        |> String.replace "\\" "/"
        |> String.map whitespaceToSpace
        |> mergeConsecutiveSpaces


whitespaceToSpace : Char -> Char
whitespaceToSpace character =
    if List.member character [ '\n', '\u{000D}', '\t' ] then
        ' '

    else
        character


mergeConsecutiveSpaces : String -> String
mergeConsecutiveSpaces text =
    let
        keepUnlessRepeatedSpace character kept =
            case kept of
                ' ' :: _ ->
                    if character == ' ' then
                        kept

                    else
                        character :: kept

                _ ->
                    character :: kept
    in
    String.foldl keepUnlessRepeatedSpace [] text
        |> List.reverse
        |> String.fromList


description : PlanDraft -> String
description (PlanDraft draft) =
    draft.description


authorName : PlanDraft -> String
authorName (PlanDraft draft) =
    draft.authorName


authorEmail : PlanDraft -> String
authorEmail (PlanDraft draft) =
    draft.authorEmail


{-| GitHub handle of the author, not editable but kept from imported or forked
plans.
-}
authorGithub : PlanDraft -> Maybe String
authorGithub (PlanDraft draft) =
    draft.authorGithub


difficulty : PlanDraft -> Difficulty
difficulty (PlanDraft draft) =
    draft.difficulty


{-| Path of the community plan this one was forked from.
-}
forkedFrom : PlanDraft -> Maybe String
forkedFrom (PlanDraft draft) =
    draft.forkedFrom


{-| Planning of a day, empty when nothing is planned.
-}
day : Day -> PlanDraft -> DayDraft
day planDay (PlanDraft draft) =
    Dict.get (Plan.dayToString planDay) draft.dayByName
        |> Maybe.withDefault emptyDay


{-| Number of effort steps (breaks excluded) planned on a day.
-}
effortCount : Day -> PlanDraft -> Int
effortCount planDay draft =
    day planDay draft
        |> .steps
        |> List.filter (\step -> step.kind == Effort)
        |> List.length


{-| Marks the draft as a copy of the community plan at `path`.
-}
forkFrom : String -> PlanDraft -> PlanDraft
forkFrom path (PlanDraft draft) =
    PlanDraft { draft | forkedFrom = Just (sanitizeText path) }


setDescription : String -> PlanDraft -> PlanDraft
setDescription text (PlanDraft draft) =
    PlanDraft { draft | description = sanitizeText text }


setAuthorName : String -> PlanDraft -> PlanDraft
setAuthorName text (PlanDraft draft) =
    PlanDraft { draft | authorName = sanitizeText text }


setAuthorEmail : String -> PlanDraft -> PlanDraft
setAuthorEmail text (PlanDraft draft) =
    PlanDraft { draft | authorEmail = sanitizeText text }


setDifficulty : Difficulty -> PlanDraft -> PlanDraft
setDifficulty newDifficulty (PlanDraft draft) =
    PlanDraft { draft | difficulty = newDifficulty }


{-| Sets the rounds of a day, capped to `roundsBounds`.
-}
setRounds : Day -> Int -> PlanDraft -> PlanDraft
setRounds planDay rounds =
    updateDay planDay (\dayDraft -> { dayDraft | rounds = BoundedInt.set rounds dayDraft.rounds })


{-| Appends a step of the given kind, unnamed and with a default duration.
-}
addStep : Day -> StepKind -> PlanDraft -> PlanDraft
addStep planDay kind =
    let
        seconds =
            case kind of
                Effort ->
                    defaultEffortSeconds

                Break ->
                    defaultBreakSeconds
    in
    updateSteps planDay (\steps -> steps ++ [ newStep kind "" seconds Nothing ])


removeStep : Day -> Int -> PlanDraft -> PlanDraft
removeStep planDay index =
    updateSteps planDay (\steps -> List.take index steps ++ List.drop (index + 1) steps)


{-| Swaps a step with the previous one, does nothing for the first step.
-}
moveStepUp : Day -> Int -> PlanDraft -> PlanDraft
moveStepUp planDay index =
    updateSteps planDay (swapWithNext (index - 1))


{-| Swaps a step with the next one, does nothing for the last step.
-}
moveStepDown : Day -> Int -> PlanDraft -> PlanDraft
moveStepDown planDay index =
    updateSteps planDay (swapWithNext index)


setStepName : Day -> Int -> String -> PlanDraft -> PlanDraft
setStepName planDay index name =
    updateStep planDay index (\step -> { step | name = sanitizeText name })


{-| Sets the duration of a step, capped to `secondsBounds`.
-}
setStepSeconds : Day -> Int -> Int -> PlanDraft -> PlanDraft
setStepSeconds planDay index seconds =
    updateStep planDay index (\step -> { step | seconds = BoundedInt.set seconds step.seconds })


setStepKind : Day -> Int -> StepKind -> PlanDraft -> PlanDraft
setStepKind planDay index kind =
    updateStep planDay index (\step -> { step | kind = kind })


updateDay : Day -> (DayDraft -> DayDraft) -> PlanDraft -> PlanDraft
updateDay planDay update ((PlanDraft draft) as planDraft) =
    PlanDraft { draft | dayByName = Dict.insert (Plan.dayToString planDay) (update (day planDay planDraft)) draft.dayByName }


updateSteps : Day -> (List StepDraft -> List StepDraft) -> PlanDraft -> PlanDraft
updateSteps planDay update =
    updateDay planDay (\dayDraft -> { dayDraft | steps = update dayDraft.steps })


updateStep : Day -> Int -> (StepDraft -> StepDraft) -> PlanDraft -> PlanDraft
updateStep planDay index update =
    updateSteps planDay
        (List.indexedMap
            (\stepIndex step ->
                if stepIndex == index then
                    update step

                else
                    step
            )
        )


{-| Swaps the item at `index` with the following one, leaving the list as is
when either doesn't exist.
-}
swapWithNext : Int -> List a -> List a
swapWithNext index items =
    case List.drop index items of
        first :: second :: rest ->
            if index < 0 then
                items

            else
                List.take index items ++ second :: first :: rest

        _ ->
            items
