module Page.PlanEditor exposing (Model, Msg, init, toSession, update, view)

{-| Page creating and editing the user's plans. Every change is saved right
away into the local storage; new and forked plans are only saved on their
first change.
-}

import BoundedInt
import CustomPlans exposing (CustomPlan, CustomPlans)
import CustomPlans.Storage as Storage
import Difficulty exposing (Difficulty)
import File exposing (File)
import File.Select
import GitHub
import Html exposing (Html, a, aside, button, div, h2, li, p, section, span, text, ul)
import Html.Attributes exposing (class, href, title, type_)
import Html.Events exposing (onClick)
import Plan exposing (Day)
import Plan.Estimate
import PlanDraft exposing (Measure(..), PlanDraft, Schedule(..), StepDraft, StepKind(..))
import PlanDraft.Yaml
import Route exposing (EditorTarget(..))
import Session exposing (Session)
import Task
import Ui.Button as Button
import Ui.Card as Card
import Ui.DownloadLink as DownloadLink
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.IconButton as IconButton
import Ui.Notice as Notice
import Ui.NumberInput as NumberInput
import Ui.Select as Select
import Ui.TextInput as TextInput


type alias Model =
    { session : Session
    , selection : Selection
    , selectedDay : Day
    , pendingDeletion : Maybe Int
    , hasImportFailed : Bool
    }


{-| Plan being edited. An `Unsaved` plan is new or forked and gets stored on
its first change.
-}
type Selection
    = Unsaved PlanDraft
    | Stored Int
    | FetchingFork
    | ForkFailed


{-| Changes to the edited plan. Step changes refer to steps of the selected
day by their position.
-}
type Edit
    = ChangedDescription String
    | ChangedAuthorName String
    | ChangedAuthorEmail String
    | ChangedDifficulty Difficulty
    | ChangedSchedule Schedule
    | ChangedRounds Int
    | AddedStep StepKind
    | RemovedStep Int
    | MovedStepUp Int
    | MovedStepDown Int
    | ChangedStepKind Int StepKind
    | ChangedStepName Int String
    | ChangedStepSeconds Int Int
    | ChangedStepCount Int Int
    | ChangedStepMeasure Int Measure


type Msg
    = Edited Edit
    | SelectedDay Day
    | SelectedPlan Int
    | ClickedNewPlan
    | ClickedImport
    | SelectedImportFile File
    | ReadImportFile String
    | DismissedImportError
    | RequestedDeletion Int
    | CancelledDeletion
    | ConfirmedDeletion Int
    | ForkFetched String (GitHub.HttpResponse String)


untitledPlan : String
untitledPlan =
    "Untitled plan"


weekdayAbbreviationLength : Int
weekdayAbbreviationLength =
    3


{-| Opens a new plan, a copy of a community plan or one of the user's plans.
-}
init : EditorTarget -> Session -> ( Model, Cmd Msg )
init target session =
    let
        model =
            { session = session
            , selection = Unsaved PlanDraft.empty
            , selectedDay = Plan.dayFromTimeDay (Session.currentDay session)
            , pendingDeletion = Nothing
            , hasImportFailed = False
            }
    in
    case target of
        Fork path ->
            ( { model | selection = FetchingFork }, GitHub.fetchContent (ForkFetched path) path )

        EditPlan id ->
            ( { model | selection = Stored id }, Cmd.none )

        NewPlan ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edited edit ->
            commitEdit edit model

        SelectedDay day ->
            ( { model | selectedDay = day }, Cmd.none )

        SelectedPlan id ->
            ( { model | selection = Stored id, pendingDeletion = Nothing }, Cmd.none )

        ClickedNewPlan ->
            ( { model | selection = Unsaved PlanDraft.empty, pendingDeletion = Nothing }, Cmd.none )

        ClickedImport ->
            ( model, File.Select.file [ PlanDraft.Yaml.mimeType, ".yml", ".yaml" ] SelectedImportFile )

        SelectedImportFile file ->
            ( model, Task.perform ReadImportFile (File.toString file) )

        ReadImportFile content ->
            importPlan content model

        DismissedImportError ->
            ( { model | hasImportFailed = False }, Cmd.none )

        RequestedDeletion id ->
            ( { model | pendingDeletion = Just id }, Cmd.none )

        CancelledDeletion ->
            ( { model | pendingDeletion = Nothing }, Cmd.none )

        ConfirmedDeletion id ->
            deletePlan id model

        ForkFetched path (Ok content) ->
            case PlanDraft.Yaml.decode content of
                Ok draft ->
                    ( { model | selection = Unsaved (PlanDraft.forkFrom path draft) }, Cmd.none )

                Err _ ->
                    ( { model | selection = ForkFailed }, Cmd.none )

        ForkFetched _ (Err _) ->
            ( { model | selection = ForkFailed }, Cmd.none )


applyEdit : Day -> Edit -> PlanDraft -> PlanDraft
applyEdit day edit =
    case edit of
        ChangedDescription text ->
            PlanDraft.setDescription text

        ChangedAuthorName text ->
            PlanDraft.setAuthorName text

        ChangedAuthorEmail text ->
            PlanDraft.setAuthorEmail text

        ChangedDifficulty difficulty ->
            PlanDraft.setDifficulty difficulty

        ChangedSchedule schedule ->
            PlanDraft.setSchedule day schedule

        ChangedRounds rounds ->
            PlanDraft.setRounds day rounds

        AddedStep kind ->
            PlanDraft.addStep day kind

        RemovedStep index ->
            PlanDraft.removeStep day index

        MovedStepUp index ->
            PlanDraft.moveStepUp day index

        MovedStepDown index ->
            PlanDraft.moveStepDown day index

        ChangedStepKind index kind ->
            PlanDraft.setStepKind day index kind

        ChangedStepName index name ->
            PlanDraft.setStepName day index name

        ChangedStepSeconds index seconds ->
            PlanDraft.setStepSeconds day index seconds

        ChangedStepCount index count ->
            PlanDraft.setStepCount day index count

        ChangedStepMeasure index measure ->
            PlanDraft.setStepMeasure day index measure


{-| Applies the edit to the selected plan and saves it, storing it first when
it wasn't yet.
-}
commitEdit : Edit -> Model -> ( Model, Cmd Msg )
commitEdit edit model =
    let
        change =
            applyEdit model.selectedDay edit
    in
    case model.selection of
        Unsaved draft ->
            let
                ( id, plans ) =
                    CustomPlans.insert (change draft) (customPlans model)
            in
            storePlans plans { model | selection = Stored id }

        Stored id ->
            storePlans (CustomPlans.update id change (customPlans model)) model

        _ ->
            ( model, Cmd.none )


{-| Stores a valid plan file right away and selects it; keeps everything as is
otherwise.
-}
importPlan : String -> Model -> ( Model, Cmd Msg )
importPlan content model =
    case PlanDraft.Yaml.decode content of
        Ok draft ->
            let
                ( id, plans ) =
                    CustomPlans.insert draft (customPlans model)
            in
            storePlans plans { model | selection = Stored id, hasImportFailed = False }

        Err _ ->
            ( { model | hasImportFailed = True }, Cmd.none )


deletePlan : Int -> Model -> ( Model, Cmd Msg )
deletePlan id model =
    let
        selection =
            if model.selection == Stored id then
                Unsaved PlanDraft.empty

            else
                model.selection
    in
    storePlans (CustomPlans.remove id (customPlans model)) { model | selection = selection, pendingDeletion = Nothing }


storePlans : CustomPlans -> Model -> ( Model, Cmd Msg )
storePlans plans model =
    ( { model | session = Session.putCustomPlans plans model.session }, Storage.save plans )


customPlans : Model -> CustomPlans
customPlans model =
    Session.customPlans model.session


selectedDraft : Model -> Maybe PlanDraft
selectedDraft model =
    case model.selection of
        Unsaved draft ->
            Just draft

        Stored id ->
            CustomPlans.get id (customPlans model)

        _ ->
            Nothing


planTitle : PlanDraft -> String
planTitle draft =
    if String.isEmpty (String.trim (PlanDraft.description draft)) then
        untitledPlan

    else
        PlanDraft.description draft


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Plan editor"
    }


viewContent : Model -> Html Msg
viewContent model =
    div [ class "bg-gray-50 px-4 py-6 sm:py-10" ]
        [ div [ class "mx-auto flex max-w-6xl flex-col gap-6" ]
            [ viewImportError model.hasImportFailed
            , div [ class "flex flex-col gap-6 lg:flex-row lg:items-start" ]
                [ aside [ class (Card.classes ++ " p-3 lg:w-80 lg:shrink-0") ] [ viewPlanList model ]
                , section [ class (Card.classes ++ " min-w-0 flex-1") ] [ viewSelection model ]
                ]
            ]
        ]


viewImportError : Bool -> Html Msg
viewImportError hasImportFailed =
    if hasImportFailed then
        Notice.view { tone = Notice.Error, content = [ text "This file isn't a valid plan, nothing was imported." ], onDismiss = DismissedImportError }

    else
        text ""



-- PLAN LIST


viewPlanList : Model -> Html Msg
viewPlanList model =
    let
        storedItems =
            List.map (viewStoredPlan model) (CustomPlans.toList (customPlans model))

        unsavedItem =
            case model.selection of
                Unsaved draft ->
                    [ viewUnsavedPlan draft ]

                _ ->
                    []

        items =
            unsavedItem ++ storedItems
    in
    div []
        [ div [ class "flex items-center justify-between gap-2 px-3 pt-1 pb-3" ]
            [ span [ class Field.labelClasses ] [ text "Your plans" ]
            , div [ class "flex gap-2" ]
                [ button [ type_ "button", onClick ClickedImport, class (Button.secondaryClasses ++ " px-3 py-1 text-sm") ] [ text "Import" ]
                , button [ type_ "button", onClick ClickedNewPlan, class (Button.primaryClasses ++ " px-3 py-1 text-sm") ] [ text "New" ]
                ]
            ]
        , if List.isEmpty items then
            p [ class "px-3 py-6 text-center text-gray-400" ] [ text "No saved plans yet" ]

          else
            ul [ class "space-y-1" ] items
        ]


planRowClasses : Bool -> String
planRowClasses isSelected =
    if isSelected then
        "bg-pink-500 text-white shadow-sm"

    else
        "text-gray-600 hover:bg-pink-50"


viewUnsavedPlan : PlanDraft -> Html Msg
viewUnsavedPlan draft =
    li [ class ("flex flex-col rounded-lg px-3 py-2 " ++ planRowClasses True) ]
        [ span [ class "truncate" ] [ text (planTitle draft) ]
        , span [ class "text-xs opacity-80" ] [ text "Saved on your first change" ]
        ]


viewStoredPlan : Model -> CustomPlan -> Html Msg
viewStoredPlan model customPlan =
    if model.pendingDeletion == Just customPlan.id then
        viewDeletionConfirmation customPlan

    else
        let
            isSelected =
                model.selection == Stored customPlan.id
        in
        li [ class ("group flex items-center gap-1 rounded-lg transition-colors " ++ planRowClasses isSelected) ]
            [ button
                [ type_ "button"
                , onClick (SelectedPlan customPlan.id)
                , class "flex min-w-0 flex-1 cursor-pointer flex-col px-3 py-2 text-left"
                ]
                [ span [ class "truncate" ] [ text (planTitle customPlan.draft) ]
                , span [ class "truncate text-xs opacity-70" ] [ text (plannedDaysLabel customPlan.draft) ]
                ]
            , button
                [ type_ "button"
                , onClick (RequestedDeletion customPlan.id)
                , Html.Attributes.attribute "aria-label" "Delete plan"
                , Html.Attributes.title "Delete plan"
                , class "mr-1 flex size-8 shrink-0 cursor-pointer items-center justify-center rounded-full text-lg opacity-60 transition-opacity hover:opacity-100"
                ]
                [ text "×" ]
            ]


plannedDaysLabel : PlanDraft -> String
plannedDaysLabel draft =
    case PlanDraft.schedule draft of
        Weekly ->
            weeklyPlannedDaysLabel draft

        EveryDay ->
            if List.isEmpty (PlanDraft.everyDay draft).steps then
                "Nothing planned yet"

            else
                "Every day"


weeklyPlannedDaysLabel : PlanDraft -> String
weeklyPlannedDaysLabel draft =
    let
        count =
            List.length (List.filter (\day -> not (List.isEmpty (PlanDraft.day day draft).steps)) Plan.allDays)
    in
    case count of
        0 ->
            "Nothing planned yet"

        1 ->
            "1 day planned"

        _ ->
            String.fromInt count ++ " days planned"


viewDeletionConfirmation : CustomPlan -> Html Msg
viewDeletionConfirmation customPlan =
    li [ class "flex flex-col gap-2 rounded-lg bg-red-50 px-3 py-2 ring-1 ring-red-200" ]
        [ span [ class "truncate text-sm text-red-700" ] [ text ("Delete “" ++ planTitle customPlan.draft ++ "”?") ]
        , div [ class "flex gap-2" ]
            [ button [ type_ "button", onClick (ConfirmedDeletion customPlan.id), class "cursor-pointer rounded-full bg-red-500 px-3 py-1 text-sm font-semibold text-white transition-colors hover:bg-red-600" ] [ text "Delete" ]
            , button [ type_ "button", onClick CancelledDeletion, class (Button.secondaryClasses ++ " px-3 py-1 text-sm") ] [ text "Cancel" ]
            ]
        ]



-- EDITOR


viewSelection : Model -> Html Msg
viewSelection model =
    case ( model.selection, selectedDraft model ) of
        ( FetchingFork, _ ) ->
            p [ class "animate-pulse px-6 py-20 text-center text-gray-400" ] [ text "Loading the plan to fork…" ]

        ( ForkFailed, _ ) ->
            p [ class "px-6 py-20 text-center text-red-600" ] [ text "Could not load the plan to fork." ]

        ( _, Just draft ) ->
            viewEditor model draft

        ( _, Nothing ) ->
            p [ class "px-6 py-20 text-center text-gray-400" ] [ text "This plan doesn't exist anymore." ]


viewEditor : Model -> PlanDraft -> Html Msg
viewEditor model draft =
    div []
        [ viewEditorHeader model draft
        , viewDetails draft
        , div [ class "border-t border-gray-100 px-6 py-4" ]
            [ viewWeek model.selectedDay draft
            , viewDay model.selectedDay draft
            ]
        ]


viewEditorHeader : Model -> PlanDraft -> Html Msg
viewEditorHeader model draft =
    let
        saveStatus =
            case model.selection of
                Unsaved _ ->
                    "Not saved yet, it will be on your first change"

                _ ->
                    "Saved automatically"

        status =
            case PlanDraft.forkedFrom draft of
                Just path ->
                    "Forked from " ++ path ++ " · " ++ saveStatus

                Nothing ->
                    saveStatus
    in
    div [ class "flex items-start justify-between gap-4 border-b border-gray-100 px-6 py-4" ]
        [ div [ class "flex min-w-0 flex-col" ]
            [ h2 [ class "truncate text-2xl font-bold tracking-tight text-gray-800" ] [ text (planTitle draft) ]
            , p [ class "truncate text-sm text-gray-400" ] [ text status ]
            ]
        , div [ class "flex shrink-0 gap-2" ]
            [ DownloadLink.view
                { fileName = PlanDraft.Yaml.fileName draft, mimeType = PlanDraft.Yaml.mimeType, content = PlanDraft.Yaml.encode draft }
                [ class (Button.secondaryClasses ++ " px-5 py-2") ]
                [ text "Export" ]
            , viewRunLink model.selection (runDay model.selectedDay draft)
            ]
        ]


{-| Day to run the plan on: the selected one for weekly plans, none for plans
having the same workout every day.
-}
runDay : Day -> PlanDraft -> Maybe Day
runDay selectedDay draft =
    case PlanDraft.schedule draft of
        Weekly ->
            Just selectedDay

        EveryDay ->
            Nothing


{-| Link running the plan, on a given day for weekly plans. A plan has to be
saved to be run, since it is opened by its identifier.
-}
viewRunLink : Selection -> Maybe Day -> Html Msg
viewRunLink selection maybeDay =
    case selection of
        Stored id ->
            a [ href (Route.routeToString (Route.RunPlan (Route.Custom id) maybeDay)), class (Button.primaryClasses ++ " px-5 py-2") ] [ text "Run" ]

        _ ->
            span
                [ title "Make a change to save the plan before running it"
                , class (Button.primaryClasses ++ " cursor-not-allowed px-5 py-2 opacity-40 hover:bg-pink-500")
                ]
                [ text "Run" ]


viewDetails : PlanDraft -> Html Msg
viewDetails draft =
    div [ class "grid gap-4 px-6 py-4 sm:grid-cols-2" ]
        [ div [ class "sm:col-span-2" ]
            [ Field.view "Description"
                (TextInput.view { label = "Description", inputType = "text", placeholder = "Duff-powered HIIT", value = PlanDraft.description draft, onInput = Edited << ChangedDescription })
            ]
        , Field.view "Your name"
            (TextInput.view { label = "Your name", inputType = "text", placeholder = "Homer Simpson", value = PlanDraft.authorName draft, onInput = Edited << ChangedAuthorName })
        , Field.view "Your email"
            (TextInput.view { label = "Your email", inputType = "email", placeholder = "homer@springfieldnuclear.com", value = PlanDraft.authorEmail draft, onInput = Edited << ChangedAuthorEmail })
        , Field.view "Difficulty"
            (Select.view
                { label = "Difficulty"
                , options = Difficulty.all
                , selected = PlanDraft.difficulty draft
                , toKey = Difficulty.toString
                , toLabel = Difficulty.label
                , onSelect = Edited << ChangedDifficulty
                }
            )
        , Field.view "Schedule"
            (Select.view
                { label = "Schedule"
                , options = [ Weekly, EveryDay ]
                , selected = PlanDraft.schedule draft
                , toKey = scheduleKey
                , toLabel = scheduleLabel
                , onSelect = Edited << ChangedSchedule
                }
            )
        ]


scheduleKey : Schedule -> String
scheduleKey schedule =
    case schedule of
        Weekly ->
            "weekly"

        EveryDay ->
            "every-day"


scheduleLabel : Schedule -> String
scheduleLabel schedule =
    case schedule of
        Weekly ->
            "A workout per weekday"

        EveryDay ->
            "Same workout every day"


{-| Days to pick from, only for plans having a workout per weekday.
-}
viewWeek : Day -> PlanDraft -> Html Msg
viewWeek selectedDay draft =
    case PlanDraft.schedule draft of
        Weekly ->
            div [ class "mb-4 flex gap-1 rounded-xl bg-gray-100 p-1" ] (List.map (viewWeekday selectedDay draft) Plan.allDays)

        EveryDay ->
            text ""


viewWeekday : Day -> PlanDraft -> Day -> Html Msg
viewWeekday selectedDay draft day =
    let
        effortCount =
            PlanDraft.effortCount day draft

        ( dayClasses, countClasses ) =
            if Plan.weekdayEquals day selectedDay then
                ( "bg-pink-500 text-white shadow-sm", "bg-white/25 text-white" )

            else if effortCount > 0 then
                ( "text-gray-600 hover:bg-pink-50 hover:text-pink-600", "bg-pink-100 text-pink-600" )

            else
                ( "text-gray-400 hover:bg-pink-50 hover:text-pink-600", "bg-white text-gray-400" )
    in
    button
        [ type_ "button"
        , onClick (SelectedDay day)
        , Html.Attributes.attribute "aria-label" (Plan.dayToString day ++ ", " ++ String.fromInt effortCount ++ " efforts")
        , class ("flex flex-1 cursor-pointer flex-col items-center gap-1 rounded-lg py-1.5 text-sm font-medium transition-colors " ++ dayClasses)
        ]
        [ span [] [ text (String.slice 0 weekdayAbbreviationLength (Plan.dayToString day)) ]
        , span [ class ("min-w-6 rounded-full px-1.5 text-xs tabular-nums " ++ countClasses) ] [ text (String.fromInt effortCount) ]
        ]


viewDay : Day -> PlanDraft -> Html Msg
viewDay day draft =
    let
        dayDraft =
            PlanDraft.day day draft

        stepCount =
            List.length dayDraft.steps
    in
    div [ class "flex flex-col gap-4" ]
        [ div [ class "flex items-center justify-between gap-4" ]
            [ span [ class "font-medium text-gray-800 capitalize" ] [ text (workoutTitle day draft) ]
            , div [ class "flex items-center gap-2" ]
                [ viewEstimate dayDraft
                , span [ class Field.labelClasses ] [ text "Rounds" ]
                , NumberInput.view { label = "Rounds", value = dayDraft.rounds, onChange = Edited << ChangedRounds, extraClasses = "w-20" }
                ]
            ]
        , if List.isEmpty dayDraft.steps then
            p [ class "rounded-lg border border-dashed border-gray-200 px-3 py-6 text-center text-gray-400" ] [ text "No steps yet, add an effort to get started" ]

          else
            ul [ class "space-y-2" ] (List.indexedMap (viewStep stepCount) dayDraft.steps)
        , div [ class "flex gap-2" ]
            [ button [ type_ "button", onClick (Edited (AddedStep Effort)), class (Button.primaryClasses ++ " px-4 py-1.5 text-sm") ] [ text "+ Effort" ]
            , button [ type_ "button", onClick (Edited (AddedStep Break)), class (Button.secondaryClasses ++ " px-4 py-1.5 text-sm") ] [ text "+ Break" ]
            ]
        ]


{-| Estimated duration of a day, hidden while it has no step.
-}
viewEstimate : PlanDraft.DayDraft -> Html Msg
viewEstimate dayDraft =
    if List.isEmpty dayDraft.steps then
        text ""

    else
        let
            seconds =
                Plan.Estimate.workoutSeconds
                    { rounds = BoundedInt.value dayDraft.rounds
                    , amounts = List.map PlanDraft.stepAmount dayDraft.steps
                    , extraSecondsPerRound = 0
                    }

            explanation =
                "Estimated duration of all rounds, counting " ++ String.fromInt Plan.Estimate.secondsPerRepetition ++ " seconds per repetition"
        in
        span [ class "mr-2 flex shrink-0 items-center gap-1 text-sm whitespace-nowrap text-gray-400 tabular-nums", title explanation ]
            [ Icon.clock "size-4 shrink-0", text ("≈ " ++ Plan.Estimate.format seconds) ]


workoutTitle : Day -> PlanDraft -> String
workoutTitle day draft =
    case PlanDraft.schedule draft of
        Weekly ->
            Plan.dayToString day

        EveryDay ->
            "Every day"


stepKindLabel : StepKind -> String
stepKindLabel kind =
    case kind of
        Effort ->
            "Effort"

        Break ->
            "Break"


viewStep : Int -> Int -> StepDraft -> Html Msg
viewStep stepCount index step =
    let
        ( rowClasses, namePlaceholder ) =
            case step.kind of
                Effort ->
                    ( "bg-white ring-gray-200", "Effort name" )

                Break ->
                    ( "bg-pink-50 ring-pink-100", "Break (name optional)" )
    in
    li [ class ("flex flex-wrap items-center gap-2 rounded-lg p-2 ring-1 sm:flex-nowrap " ++ rowClasses) ]
        [ div [ class "w-28 shrink-0" ]
            [ Select.view
                { label = "Step type"
                , options = [ Effort, Break ]
                , selected = step.kind
                , toKey = stepKindLabel
                , toLabel = stepKindLabel
                , onSelect = Edited << ChangedStepKind index
                }
            ]
        , div [ class "min-w-0 flex-1" ]
            [ TextInput.view { label = "Step name", inputType = "text", placeholder = namePlaceholder, value = step.name, onInput = Edited << ChangedStepName index } ]
        , viewStepAmount index step
        , div [ class "flex shrink-0 items-center" ]
            [ IconButton.view { label = "Move up", isDisabled = index == 0, onPress = Edited (MovedStepUp index) } (Icon.chevronUp "size-4")
            , IconButton.view { label = "Move down", isDisabled = index == stepCount - 1, onPress = Edited (MovedStepDown index) } (Icon.chevronDown "size-4")
            , IconButton.view { label = "Remove step", isDisabled = False, onPress = Edited (RemovedStep index) } (text "×")
            ]
        ]


measureLabel : Measure -> String
measureLabel measure =
    case measure of
        Seconds ->
            "sec"

        Repetitions ->
            "reps"


{-| Duration or repetitions of a step. Only efforts can be measured in
repetitions, breaks always last a duration.
-}
viewStepAmount : Int -> StepDraft -> Html Msg
viewStepAmount index step =
    let
        ( numberInput, unit ) =
            case ( step.kind, step.measure ) of
                ( Effort, Repetitions ) ->
                    ( NumberInput.view { label = "Repetitions", value = step.count, onChange = Edited << ChangedStepCount index, extraClasses = "w-20" }
                    , viewMeasureSelect index step.measure
                    )

                ( Effort, Seconds ) ->
                    ( viewSecondsInput index step
                    , viewMeasureSelect index step.measure
                    )

                ( Break, _ ) ->
                    ( viewSecondsInput index step
                    , span [ class "w-24 shrink-0 px-3 text-sm text-gray-400" ] [ text (measureLabel Seconds) ]
                    )
    in
    div [ class "flex shrink-0 items-center gap-1" ] [ numberInput, unit ]


viewSecondsInput : Int -> StepDraft -> Html Msg
viewSecondsInput index step =
    NumberInput.view { label = "Duration in seconds", value = step.seconds, onChange = Edited << ChangedStepSeconds index, extraClasses = "w-20" }


viewMeasureSelect : Int -> Measure -> Html Msg
viewMeasureSelect index measure =
    div [ class "w-24 shrink-0" ]
        [ Select.view
            { label = "Measure"
            , options = [ Seconds, Repetitions ]
            , selected = measure
            , toKey = measureLabel
            , toLabel = measureLabel
            , onSelect = Edited << ChangedStepMeasure index
            }
        ]
