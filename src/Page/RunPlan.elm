port module Page.RunPlan exposing (Model, Msg, init, toSession, update, view)

import Cmd.Extra exposing (delay, send)
import CustomPlans
import File exposing (File)
import GitHub
import Html exposing (Html, a, aside, button, div, h2, h3, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, class, href, title, type_)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (Decoder)
import Plan exposing (Day(..), Plan, Step, stepSeconds)
import PlanDraft.Yaml
import Resource exposing (Resource)
import Route exposing (PlanSource(..))
import Session exposing (Session)
import Settings exposing (Settings)
import Task
import Timer exposing (Timer)
import Ui.Button as Button
import Ui.Card as Card
import Ui.Icon as Icon
import Ziplist exposing (Position(..))


port playSound : String -> Cmd msg


port storeLastPlan : Maybe String -> Cmd msg


type alias Model =
    { session : Session
    , source : PlanSource
    , file : Maybe File
    , plan : Resource Plan
    , dayPlan : Maybe Plan.DaysMap
    , currentTimer : Timer
    , currentRound : Int
    , lastPlan : Maybe String
    , currentDay : Plan.Day
    , isCompleted : Bool
    }


type Msg
    = GotFile (Maybe File)
    | PlanLoaded { result : Result String Plan, storeLastPlan : Maybe String }
    | Start
    | Tick
    | ClosePlan
    | FileFetched (GitHub.HttpResponse String)
    | ChangeDay Plan.Day
    | Restart


init : PlanSource -> Maybe Plan.Day -> Session -> ( Model, Cmd Msg )
init source maybeDay session =
    let
        currentDay =
            case maybeDay of
                Just day ->
                    day

                _ ->
                    Plan.dayFromTimeDay session.currentDay

        model =
            { session = session
            , source = source
            , file = Nothing
            , plan = Resource.init
            , dayPlan = Nothing
            , currentTimer = Timer.init 0
            , currentRound = 0
            , lastPlan = session.appConfig.lastPlan
            , currentDay = currentDay
            , isCompleted = False
            }
    in
    case source of
        Custom customId ->
            ( loadCustomPlan customId model, Cmd.none )

        _ ->
            ( model, initCommand model )


{-| Loads one of the user's plans, which are already in the session.
-}
loadCustomPlan : Int -> Model -> Model
loadCustomPlan customId model =
    let
        result =
            CustomPlans.get customId (Session.customPlans model.session)
                |> Result.fromMaybe "This plan doesn't exist anymore"
                |> Result.andThen PlanDraft.Yaml.toPlan
    in
    case result of
        Ok plan ->
            putLoadedPlan plan model

        Err error ->
            { model | plan = Resource.fail error }


initCommand : Model -> Cmd Msg
initCommand model =
    case ( model.source, model.lastPlan ) of
        ( LastPlan, Just string ) ->
            Cmd.Extra.send (PlanLoaded { result = Plan.decode string, storeLastPlan = Nothing })

        ( Community planPath, _ ) ->
            GitHub.fetchContent FileFetched planPath

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile (Just file) ->
            let
                decodeFile string =
                    { result = Plan.decode string, storeLastPlan = Just string }
            in
            ( { model | file = Just file }, Task.perform PlanLoaded (File.toString file |> Task.map decodeFile) )

        PlanLoaded planLoadedResult ->
            case planLoadedResult.result of
                Ok plan ->
                    let
                        storePlanCmd =
                            case planLoadedResult.storeLastPlan of
                                Just planString ->
                                    storeLastPlan (Just planString)

                                _ ->
                                    Cmd.none
                    in
                    ( putLoadedPlan plan model, storePlanCmd )

                _ ->
                    ( model, Cmd.none )

        Start ->
            let
                isStartingRound =
                    isAtRoundStart model.dayPlan

                stepSeconds =
                    currentStep model
                        |> Maybe.map Plan.stepSeconds
                        |> Maybe.withDefault 0

                seconds =
                    if isStartingRound then
                        stepSeconds + Settings.preRoundDelaySeconds (settings model)

                    else
                        stepSeconds

                timer =
                    seconds
                        |> Timer.init
                        |> Timer.start

                round =
                    if isStartingRound then
                        model.currentRound + 1

                    else
                        model.currentRound

                cmd =
                    Cmd.batch
                        [ delay 1 Tick
                        , playSoundEffect model "long_beep"
                        ]
            in
            ( { model | currentTimer = timer, currentRound = round }, cmd )

        ClosePlan ->
            ( { model | plan = Resource.init, lastPlan = Nothing }, storeLastPlan Nothing )

        ChangeDay day ->
            ( resetRun { model | currentDay = day }, Cmd.none )

        Restart ->
            ( resetRun model, Cmd.none )

        Tick ->
            let
                timer =
                    Timer.tick model.currentTimer

                isCompleted =
                    not (Timer.isRunning timer) && isWorkoutCompleted model.currentRound dayPlan

                cmd =
                    if Timer.isRunning timer then
                        delay 1 Tick

                    else if isCompleted || isWaitingForNextRound model dayPlan then
                        Cmd.none

                    else
                        send Start

                remaining =
                    Timer.remaining timer

                playSoundCmd =
                    if remaining <= 5 then
                        playSoundEffect model "beep"

                    else
                        Cmd.none

                dayPlan =
                    if Timer.isRunning timer then
                        model.dayPlan

                    else
                        Maybe.map Plan.daysMapNextStep model.dayPlan
            in
            ( { model | currentTimer = timer, dayPlan = dayPlan, isCompleted = isCompleted }, Cmd.batch [ cmd, playSoundCmd ] )

        FileFetched (Ok content) ->
            let
                ( newModel, cmd ) =
                    case Plan.decode content of
                        Ok plan ->
                            ( putLoadedPlan plan model, storeLastPlan (Just content) )

                        _ ->
                            ( { model | plan = Resource.init }, Cmd.none )
            in
            ( newModel, cmd )

        FileFetched (Err _) ->
            ( { model | plan = Resource.fail "Could not download file" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Whether the last round has been fully performed, which is when the
current round is the last one and the steps wrapped back to the first one.
-}
isWorkoutCompleted : Int -> Maybe Plan.DaysMap -> Bool
isWorkoutCompleted currentRound maybeDayPlan =
    case maybeDayPlan of
        Just { rounds, steps } ->
            currentRound >= rounds && Ziplist.isReset steps

        Nothing ->
            True


{-| Whether the steps are back to the first one while the user asked to start
each round manually.
-}
isWaitingForNextRound : Model -> Maybe Plan.DaysMap -> Bool
isWaitingForNextRound model maybeDayPlan =
    not (Settings.autoNextRound (settings model)) && isAtRoundStart maybeDayPlan


isAtRoundStart : Maybe Plan.DaysMap -> Bool
isAtRoundStart maybeDayPlan =
    Maybe.map (.steps >> Ziplist.isReset) maybeDayPlan
        |> Maybe.withDefault False


{-| Plays a sound effect, unless the user disabled them.
-}
playSoundEffect : Model -> String -> Cmd msg
playSoundEffect model sound =
    if Settings.soundEffects (settings model) then
        playSound sound

    else
        Cmd.none


settings : Model -> Settings
settings model =
    Session.settings model.session


currentStep : Model -> Maybe Step
currentStep model =
    Maybe.andThen (.steps >> Ziplist.current) model.dayPlan


toSession : Model -> Session
toSession { session } =
    session


unknownAuthorName : String
unknownAuthorName =
    "Unknown"


weekdayAbbreviationLength : Int
weekdayAbbreviationLength =
    3


skeletonRowCount : Int
skeletonRowCount =
    5


headerButtonClasses : String
headerButtonClasses =
    "flex size-10 shrink-0 cursor-pointer items-center justify-center rounded-full text-2xl text-gray-400 transition-colors hover:bg-pink-50 hover:text-pink-600"


viewPlanHeader : PlanSource -> Plan -> Html Msg
viewPlanHeader source plan =
    div [ class "flex items-start justify-between gap-4 border-b border-gray-100 px-6 py-4" ]
        [ div [ class "flex min-w-0 flex-col" ]
            [ h2 [ class "truncate text-2xl font-bold tracking-tight text-gray-800" ] [ text plan.description ]
            , p [ class "truncate text-sm text-gray-400" ] [ text "by ", viewAuthor plan ]
            ]
        , div [ class "flex shrink-0 items-center" ]
            [ viewEditorLink source
            , button [ onClick ClosePlan, attribute "aria-label" "Close plan", title "Close plan", class headerButtonClasses ] [ text "×" ]
            ]
        ]


{-| Link opening the plan in the editor: a copy of a community plan, or the
user's own plan itself. Uploaded plans have nothing to open.
-}
viewEditorLink : PlanSource -> Html Msg
viewEditorLink source =
    case source of
        Community planPath ->
            viewHeaderLink (Route.PlanEditor (Route.Fork planPath)) "Fork this plan" (Icon.fork "size-5")

        Custom customId ->
            viewHeaderLink (Route.PlanEditor (Route.EditPlan customId)) "Edit this plan" (Icon.pencil "size-5")

        LastPlan ->
            text ""


viewHeaderLink : Route.Route -> String -> Html Msg -> Html Msg
viewHeaderLink route label icon =
    a
        [ href (Route.routeToString route)
        , attribute "aria-label" label
        , title label
        , class headerButtonClasses
        ]
        [ icon ]


viewAuthor : Plan -> Html Msg
viewAuthor plan =
    let
        name =
            Plan.authorName unknownAuthorName plan.author
    in
    case plan.author.email of
        Just email ->
            a [ class "font-medium text-pink-500 transition-colors hover:text-pink-600", href ("mailto:" ++ email) ] [ text name ]

        Nothing ->
            span [ class "font-medium text-gray-500" ] [ text name ]


viewRoundCounter : Model -> Html Msg
viewRoundCounter model =
    case model.dayPlan of
        Just { rounds } ->
            div [ class "flex items-baseline justify-between px-3 pt-4 pb-2" ]
                [ span [ class "text-xs font-medium tracking-wide text-gray-400 uppercase" ] [ text "Round" ]
                , span [ class "text-2xl font-bold text-gray-800 tabular-nums" ]
                    [ text (String.fromInt model.currentRound)
                    , span [ class "text-gray-400" ] [ text (" / " ++ String.fromInt rounds) ]
                    ]
                ]

        Nothing ->
            text ""


stepStateClasses : Position -> Step -> { row : String, dot : String }
stepStateClasses position step =
    case ( Ziplist.positionEquals position Current, Plan.isBreakStep step ) of
        ( True, True ) ->
            { row = "bg-pink-100 text-pink-600 ring-1 ring-pink-200", dot = "bg-pink-400 ring-4 ring-pink-200" }

        ( True, False ) ->
            { row = "bg-pink-500 text-white shadow-sm", dot = "bg-white ring-4 ring-pink-400" }

        ( False, True ) ->
            { row = "text-pink-400 hover:bg-pink-50", dot = "bg-pink-200" }

        ( False, False ) ->
            { row = "text-gray-600 hover:bg-pink-50", dot = "bg-gray-300" }


viewStep : Position -> Step -> Html Msg
viewStep position step =
    let
        stateClasses =
            stepStateClasses position step
    in
    li [ class ("flex items-center gap-3 rounded-lg px-3 py-2 transition-colors " ++ stateClasses.row) ]
        [ span [ class ("size-2 shrink-0 rounded-full " ++ stateClasses.dot) ] []
        , span [ class "min-w-0 flex-1 truncate" ] [ text (Plan.stepName step) ]
        , span [ class "text-sm tabular-nums opacity-80" ] [ text (String.fromInt (Plan.stepSeconds step) ++ "s") ]
        ]


viewSteps : Model -> Html Msg
viewSteps model =
    case model.dayPlan of
        Just daysMap ->
            div []
                [ viewRoundCounter model
                , ul [ class "space-y-1" ] (Ziplist.map viewStep daysMap.steps)
                ]

        Nothing ->
            p [ class "px-3 py-6 text-center text-gray-400" ] [ text ("Nothing planned for " ++ Plan.dayToString model.currentDay) ]


viewClock : Model -> Html Msg
viewClock model =
    if Timer.isRunning model.currentTimer then
        div [ class "text-8xl font-bold tracking-tight text-pink-500 tabular-nums sm:text-9xl" ] [ text (String.fromInt (Timer.remaining model.currentTimer)) ]

    else
        button [ onClick Start, class (Button.primaryClasses ++ " px-10 py-3 text-lg") ] [ text "Start" ]


viewCompleted : Model -> Html Msg
viewCompleted model =
    div [ class "flex flex-col items-center justify-center px-6 py-12 text-center sm:py-20" ]
        [ span [ class "mb-1 text-xs font-medium tracking-wide text-gray-400 uppercase" ] [ text "Workout completed" ]
        , h3 [ class "mb-3 text-3xl font-bold tracking-tight text-gray-800" ] [ text "Well done!" ]
        , p [ class "mb-8 max-w-md text-gray-500" ]
            [ text ("You completed all " ++ String.fromInt model.currentRound ++ " rounds of " ++ Plan.dayToString model.currentDay ++ "'s workout.") ]
        , button [ onClick Restart, class (Button.primaryClasses ++ " px-10 py-3 text-lg") ] [ text "Restart" ]
        ]


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    case currentStep model of
        Just step ->
            if model.isCompleted then
                viewCompleted model

            else
                viewActiveStep step model

        Nothing ->
            p [ class "px-6 py-12 text-center text-gray-400" ] [ text "Pick a day with a workout to get started" ]


viewActiveStep : Step -> Model -> Html Msg
viewActiveStep step model =
    div [ class "flex flex-col items-center justify-center px-6 py-12 sm:py-20" ]
        [ span [ class "mb-1 text-xs font-medium tracking-wide text-gray-400 uppercase" ] [ text (stepKindLabel step) ]
        , h3 [ class "mb-3 text-center text-3xl font-bold tracking-tight text-gray-800" ] [ text (Plan.stepName step) ]
        , viewStepDescription step
        , viewClock model
        ]


stepKindLabel : Step -> String
stepKindLabel step =
    if Plan.isBreakStep step then
        "Break"

    else
        "Effort"


viewStepDescription : Step -> Html Msg
viewStepDescription step =
    case Plan.stepDescription step of
        Just description ->
            p [ class "mb-8 max-w-md text-center whitespace-pre-line text-gray-500" ] [ text description ]

        Nothing ->
            div [ class "mb-6" ] []


viewWeekday : Model -> ( Plan.Day, Bool ) -> Html Msg
viewWeekday model ( weekday, _ ) =
    let
        stateClasses =
            if Plan.weekdayEquals weekday model.currentDay then
                "bg-pink-500 text-white shadow-sm"

            else
                "text-gray-500 hover:bg-pink-50 hover:text-pink-600"
    in
    button
        [ class ("flex-1 cursor-pointer rounded-lg py-1.5 text-center text-sm font-medium transition-colors " ++ stateClasses)
        , onClick (ChangeDay weekday)
        ]
        [ text (String.slice 0 weekdayAbbreviationLength (Plan.dayToString weekday)) ]


viewWeek : Plan -> Model -> Html Msg
viewWeek plan model =
    div [ class "flex gap-1 rounded-xl bg-gray-100 p-1" ] (List.map (viewWeekday model) (Plan.weekdays plan))


viewPlan : Plan -> Model -> Html Msg
viewPlan plan model =
    div [ class "flex flex-col gap-6 lg:flex-row lg:items-start" ]
        [ aside [ class (Card.classes ++ " p-3 lg:w-80 lg:shrink-0") ]
            [ viewWeek plan model
            , viewSteps model
            ]
        , section [ class (Card.classes ++ " min-w-0 flex-1") ]
            [ viewPlanHeader model.source plan
            , viewCurrentStep model
            ]
        ]


viewPageContainer : List (Html Msg) -> Html Msg
viewPageContainer children =
    div [ class "bg-gray-50 px-4 py-6 sm:py-10" ]
        [ div [ class "mx-auto max-w-6xl" ] children ]


viewContent : Model -> Html Msg
viewContent model =
    viewPageContainer
        [ case ( model.plan, model.source, model.lastPlan ) of
            ( Resource.Loaded plan, _, _ ) ->
                viewPlan plan model

            ( Resource.Failed error, _, _ ) ->
                viewUploadPath (Just error)

            ( _, LastPlan, Just _ ) ->
                viewLoading

            ( _, _, _ ) ->
                viewUploadPath Nothing
        ]


viewSkeletonRow : Int -> Html Msg
viewSkeletonRow _ =
    div [ class "flex items-center gap-3 px-3 py-2" ]
        [ div [ class "size-2 rounded-full bg-gray-200" ] []
        , div [ class "h-4 flex-1 rounded bg-gray-200" ] []
        , div [ class "h-4 w-8 rounded bg-gray-200" ] []
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "flex animate-pulse flex-col gap-6 lg:flex-row lg:items-start" ]
        [ div [ class (Card.classes ++ " space-y-1 p-3 lg:w-80 lg:shrink-0") ]
            (div [ class "h-8 rounded-xl bg-gray-100" ] [] :: List.map viewSkeletonRow (List.range 1 skeletonRowCount))
        , div [ class (Card.classes ++ " flex min-w-0 flex-1 flex-col items-center gap-4 px-6 py-20") ]
            [ div [ class "h-6 w-48 rounded bg-gray-200" ] []
            , div [ class "h-4 w-64 rounded bg-gray-200" ] []
            , div [ class "h-12 w-36 rounded-full bg-gray-200" ] []
            ]
        ]


viewError : String -> Html Msg
viewError error =
    p [ class "mb-4 rounded-lg bg-red-50 px-3 py-4 text-center text-red-600" ] [ text error ]


viewUploadPath : Maybe String -> Html Msg
viewUploadPath maybeError =
    div [ class "mx-auto max-w-md" ]
        [ Maybe.map viewError maybeError |> Maybe.withDefault (text "")
        , div [ class (Card.classes ++ " flex flex-col items-center px-6 py-10 text-center") ]
            [ h2 [ class "text-2xl font-bold tracking-tight text-gray-800" ] [ text "Upload a plan" ]
            , p [ class "mt-2 mb-6 text-gray-500" ] [ text "Load a plan file from your device" ]
            , label [ class (Button.primaryClasses ++ " px-8 py-2.5") ]
                [ text "Upload"
                , input [ type_ "file", on "change" (Json.Decode.map GotFile filesDecoder), class "hidden" ] []
                ]
            , a [ class "mt-6 text-sm text-gray-400 transition-colors hover:text-pink-500", href "/" ] [ text "Or return home to select a community provided plan →" ]
            ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Run Plan"
    }


putLoadedPlan : Plan -> Model -> Model
putLoadedPlan plan model =
    setCurrentDayPlay { model | plan = Resource.loaded plan }


{-| Puts the current day's workout back to its first step and round.
-}
resetRun : Model -> Model
resetRun model =
    setCurrentDayPlay { model | currentRound = 0, currentTimer = Timer.init 0, isCompleted = False }


setCurrentDayPlay : Model -> Model
setCurrentDayPlay model =
    case model.plan of
        Resource.Loaded plan ->
            { model | dayPlan = Plan.daysMap model.currentDay plan }

        _ ->
            model


filesDecoder : Decoder (Maybe File)
filesDecoder =
    Json.Decode.at [ "target", "files" ] (Json.Decode.list File.decoder)
        |> Json.Decode.map List.head
