module PlanDraft exposing
    ( DayDraft
    , Measure(..)
    , PlanDraft
    , Schedule(..)
    , StepDraft
    , StepKind(..)
    , addStep
    , authorEmail
    , authorGithub
    , authorName
    , countBounds
    , day
    , description
    , difficulty
    , effortCount
    , empty
    , everyDay
    , forkFrom
    , forkedFrom
    , fromPlan
    , moveStepDown
    , moveStepUp
    , removeStep
    , roundsBounds
    , sanitizeText
    , schedule
    , secondsBounds
    , setAuthorEmail
    , setAuthorName
    , setDescription
    , setDifficulty
    , setRounds
    , setSchedule
    , setStepCount
    , setStepKind
    , setStepMeasure
    , setStepName
    , setStepSeconds
    , stepAmount
    )

{-| Editable version of a plan, built by the plan editor.

The type is opaque so every change goes through setters keeping it valid:
texts are sanitized so they survive a YAML round trip and integers are capped
to their bounds.

-}

import BoundedInt exposing (BoundedInt, Bounds)
import Dict exposing (Dict)
import Difficulty exposing (Difficulty)
import Plan exposing (Amount(..), Day, Plan, Step(..))
import Ziplist


type PlanDraft
    = PlanDraft
        { description : String
        , authorName : String
        , authorEmail : String
        , authorGithub : Maybe String
        , difficulty : Difficulty
        , forkedFrom : Maybe String
        , schedule : Schedule
        , dayByName : Dict String DayDraft
        , everyDay : DayDraft
        }


{-| Whether the plan has a workout per weekday or the same workout every day.
Both are kept so switching back and forth doesn't lose them; only the one
matching the schedule is used and exported.
-}
type Schedule
    = Weekly
    | EveryDay


{-| Planning of a single day: its steps are repeated `rounds` times.
-}
type alias DayDraft =
    { rounds : BoundedInt
    , steps : List StepDraft
    }


{-| A step of a day. Both the duration and the repetitions are kept so
switching the measure back and forth doesn't lose them; only the one matching
`measure` is used, and breaks always use the duration. `description` is not
editable, it is only kept from imported or forked plans.
-}
type alias StepDraft =
    { kind : StepKind
    , name : String
    , measure : Measure
    , seconds : BoundedInt
    , count : BoundedInt
    , description : Maybe String
    }


type StepKind
    = Effort
    | Break


{-| How an effort is measured: held for a duration or performed a number of
times.
-}
type Measure
    = Seconds
    | Repetitions


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


defaultCount : Int
defaultCount =
    10


maximumCount : Int
maximumCount =
    999


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


{-| Allowed number of repetitions of an effort.
-}
countBounds : Bounds
countBounds =
    { minimum = 1, maximum = maximumCount }


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
        , schedule = Weekly
        , dayByName = Dict.empty
        , everyDay = emptyDay
        }


emptyDay : DayDraft
emptyDay =
    { rounds = BoundedInt.init roundsBounds roundsBounds.minimum, steps = [] }


{-| Builds a draft out of a plan, sanitizing its texts and capping its values.
-}
fromPlan : Plan -> PlanDraft
fromPlan plan =
    let
        ( planSchedule, dayByName, everyDayWorkout ) =
            case plan.schedule of
                Plan.Weekly _ ->
                    ( Weekly, weeklyDays plan, emptyDay )

                Plan.EveryDay workout ->
                    ( EveryDay, Dict.empty, dayFromDaysMap workout )
    in
    PlanDraft
        { description = sanitizeText plan.description
        , authorName = sanitizeMaybe plan.author.name
        , authorEmail = sanitizeMaybe plan.author.email
        , authorGithub = Maybe.map sanitizeText plan.author.github
        , difficulty = plan.difficulty
        , forkedFrom = Maybe.map sanitizeText plan.forkedFrom
        , schedule = planSchedule
        , dayByName = dayByName
        , everyDay = everyDayWorkout
        }


weeklyDays : Plan -> Dict String DayDraft
weeklyDays plan =
    Plan.allDays
        |> List.filterMap (\planDay -> Plan.daysMap planDay plan |> Maybe.map (\daysMap -> ( Plan.dayToString planDay, dayFromDaysMap daysMap )))
        |> Dict.fromList


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
            newStep Effort (sanitizeText spec.name) spec.amount spec.description

        Plan.Break spec ->
            newStep Break (sanitizeMaybe spec.name) (Duration spec.seconds) spec.description


newStep : StepKind -> String -> Amount -> Maybe String -> StepDraft
newStep kind name amount maybeDescription =
    let
        ( measure, seconds, count ) =
            case amount of
                Duration stepSeconds ->
                    ( Seconds, stepSeconds, defaultCount )

                Count stepCount ->
                    ( Repetitions, defaultEffortSeconds, stepCount )
    in
    { kind = kind
    , name = name
    , measure = measure
    , seconds = BoundedInt.init secondsBounds seconds
    , count = BoundedInt.init countBounds count
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


{-| Workout of the plan when scheduled every day, kept while scheduled weekly.
-}
everyDay : PlanDraft -> DayDraft
everyDay (PlanDraft draft) =
    draft.everyDay


schedule : PlanDraft -> Schedule
schedule (PlanDraft draft) =
    draft.schedule


{-| Planning of a day, empty when nothing is planned. Plans scheduled every
day ignore the day and give their single workout; every day-based function
of this module behaves the same.
-}
day : Day -> PlanDraft -> DayDraft
day planDay (PlanDraft draft) =
    case draft.schedule of
        Weekly ->
            Dict.get (Plan.dayToString planDay) draft.dayByName
                |> Maybe.withDefault emptyDay

        EveryDay ->
            draft.everyDay


{-| What a step asks for: breaks always last their duration while efforts
follow their measure.
-}
stepAmount : StepDraft -> Amount
stepAmount step =
    case ( step.kind, step.measure ) of
        ( Effort, Repetitions ) ->
            Count (BoundedInt.value step.count)

        _ ->
            Duration (BoundedInt.value step.seconds)


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


{-| Switches the schedule. When the workout of `planDay` under the new
schedule has no step yet, it starts as a copy of the one under the previous
schedule, so turning a weekday into an every day plan keeps its steps.
-}
setSchedule : Day -> Schedule -> PlanDraft -> PlanDraft
setSchedule planDay newSchedule ((PlanDraft draft) as planDraft) =
    let
        previousWorkout =
            day planDay planDraft

        switched =
            PlanDraft { draft | schedule = newSchedule }
    in
    if List.isEmpty (day planDay switched).steps then
        updateDay planDay (\_ -> previousWorkout) switched

    else
        switched


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
    updateSteps planDay (\steps -> steps ++ [ newStep kind "" (Duration seconds) Nothing ])


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


{-| Sets the repetitions of a step, capped to `countBounds`.
-}
setStepCount : Day -> Int -> Int -> PlanDraft -> PlanDraft
setStepCount planDay index count =
    updateStep planDay index (\step -> { step | count = BoundedInt.set count step.count })


setStepMeasure : Day -> Int -> Measure -> PlanDraft -> PlanDraft
setStepMeasure planDay index measure =
    updateStep planDay index (\step -> { step | measure = measure })


setStepKind : Day -> Int -> StepKind -> PlanDraft -> PlanDraft
setStepKind planDay index kind =
    updateStep planDay index (\step -> { step | kind = kind })


updateDay : Day -> (DayDraft -> DayDraft) -> PlanDraft -> PlanDraft
updateDay planDay update ((PlanDraft draft) as planDraft) =
    case draft.schedule of
        Weekly ->
            PlanDraft { draft | dayByName = Dict.insert (Plan.dayToString planDay) (update (day planDay planDraft)) draft.dayByName }

        EveryDay ->
            PlanDraft { draft | everyDay = update draft.everyDay }


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
