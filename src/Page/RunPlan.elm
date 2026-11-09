port module Page.RunPlan exposing (Model, Msg, init, subscriptions, toSession, update, view)

import ActiveSettings
import Audius exposing (Track)
import Browser.Events
import Cmd.Extra exposing (send)
import CustomPlans
import File exposing (File)
import GitHub
import Html exposing (Html, a, aside, button, div, h2, h3, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, class, href, rel, target, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder)
import Music
import Music.Port
import Plan exposing (Amount(..), Day(..), Plan, Step)
import PlanDraft.Yaml
import Random
import Resource exposing (Resource)
import Route exposing (PlanSource(..))
import Session exposing (Session)
import Settings exposing (Settings)
import StepProgress exposing (StepProgress)
import Task
import Time
import Ui.Button as Button
import Ui.Card as Card
import Ui.Icon as Icon
import Ui.IconButton as IconButton
import WakeLock
import Ziplist exposing (Position(..))


port playSound : String -> Cmd msg


port storeLastPlan : Maybe String -> Cmd msg


type alias Model =
    { session : Session
    , source : PlanSource
    , file : Maybe File
    , plan : Resource Plan
    , dayPlan : Maybe Plan.DaysMap
    , stepProgress : StepProgress
    , currentRound : Int
    , lastPlan : Maybe String
    , currentDay : Plan.Day
    , isCompleted : Bool
    , music : Music.Model
    }


type Msg
    = GotFile (Maybe File)
    | PlanLoaded { result : Result String Plan, storeLastPlan : Maybe String }
    | Start
    | Tick
    | Done
    | ClosePlan
    | FileFetched (GitHub.HttpResponse String)
    | ChangeDay Plan.Day
    | Restart
    | GotMusicTracks Music.Channel (Result Http.Error (List Track))
    | GotMusicSeed Random.Seed
    | MusicTrackEnded String
    | MusicTrackFailed String
    | ClickedMusicPlayPause
    | ClickedMusicNext


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
            , stepProgress = StepProgress.idle
            , currentRound = 0
            , lastPlan = session.appConfig.lastPlan
            , currentDay = currentDay
            , isCompleted = False
            , music = Music.init (Session.settings session)
            }
    in
    case source of
        Custom customId ->
            ( loadCustomPlan customId model, initMusicCommand session )

        _ ->
            ( model, Cmd.batch [ initCommand model, initMusicCommand session ] )


{-| Fetches the tracks of the playlists in use and a seed to shuffle them.
-}
initMusicCommand : Session -> Cmd Msg
initMusicCommand session =
    Music.playlistsToFetch (Session.settings session)
        |> List.map (\( channel, playlist ) -> Audius.fetchPlaylistTracks (GotMusicTracks channel) playlist)
        |> (::) (Random.generate GotMusicSeed Random.independentSeed)
        |> Cmd.batch


tickIntervalMilliseconds : Float
tickIntervalMilliseconds =
    1000


{-| Listens to the audio player, ticks every second while a countdown runs and
listens to the space bar while a repetition step waits for the user.
Subscriptions stop as soon as the run is reset or the page is left.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Music.Port.trackEnded MusicTrackEnded
        , Music.Port.trackFailed MusicTrackFailed
        , if StepProgress.isTicking model.stepProgress then
            Time.every tickIntervalMilliseconds (\_ -> Tick)

          else
            Sub.none
        , if StepProgress.isAwaitingDone model.stepProgress then
            Browser.Events.onKeyDown (spaceBarDecoder Done)

          else
            Sub.none
        ]


doneKey : String
doneKey =
    " "


{-| Tags which already react to the space bar when focused, so the key isn't
also handled globally.
-}
interactiveTagNames : List String
interactiveTagNames =
    [ "BUTTON", "INPUT", "TEXTAREA", "SELECT", "A" ]


{-| Succeeds with `msg` on a single press of the space bar, unless the focus is
on an element already reacting to it (pressing space on the focused "Done"
button would otherwise complete two steps).
-}
spaceBarDecoder : msg -> Decoder msg
spaceBarDecoder msg =
    Json.Decode.map3
        (\key isRepeat tagName ->
            key == doneKey && not isRepeat && not (List.member tagName interactiveTagNames)
        )
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "repeat" Json.Decode.bool)
        (Json.Decode.at [ "target", "tagName" ] Json.Decode.string)
        |> Json.Decode.andThen
            (\isDonePress ->
                if isDonePress then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "Not a press of the space bar"
            )


{-| Applies a change of the music, sending its commands to the audio player.
-}
withMusic : ( Music.Model, List Music.Command ) -> Model -> ( Model, Cmd Msg )
withMusic ( music, commands ) model =
    ( { model | music = music }, Music.Port.run commands )


stepKind : Step -> Music.StepKind
stepKind step =
    if Plan.isBreakStep step then
        Music.BreakStep

    else
        Music.EffortStep


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

                extraSeconds =
                    if isStartingRound then
                        Settings.preRoundDelaySeconds (settings model)

                    else
                        0

                stepProgress =
                    currentStep model
                        |> Maybe.map (Plan.stepAmount >> StepProgress.start extraSeconds)
                        |> Maybe.withDefault StepProgress.idle

                round =
                    if isStartingRound then
                        model.currentRound + 1

                    else
                        model.currentRound

                ( startedModel, musicCmd ) =
                    case currentStep model of
                        Just step ->
                            withMusic (Music.enterStep (settings model) (stepKind step) model.music) model

                        Nothing ->
                            ( model, Cmd.none )

                cmd =
                    Cmd.batch
                        [ playSoundEffect model "long_beep"
                        , musicCmd
                        , WakeLock.keepScreenAwake (Settings.wakeLock (settings model))
                        ]
            in
            ( { startedModel | stepProgress = stepProgress, currentRound = round }, cmd )

        ClosePlan ->
            ( resetRun { model | plan = Resource.init, dayPlan = Nothing, lastPlan = Nothing }, storeLastPlan Nothing )
                |> stopRun

        ChangeDay day ->
            ( resetRun { model | currentDay = day }, Cmd.none )
                |> stopRun

        Restart ->
            ( resetRun model, Cmd.none )
                |> stopRun

        GotMusicTracks channel result ->
            withMusic (Music.tracksFetched channel (Result.toMaybe result) model.music) model

        GotMusicSeed seed ->
            ( { model | music = Music.seed seed model.music }, Cmd.none )

        MusicTrackEnded url ->
            withMusic (Music.trackEnded url model.music) model

        MusicTrackFailed url ->
            withMusic (Music.trackFailed url model.music) model

        ClickedMusicPlayPause ->
            withMusic (Music.togglePause model.music) model

        ClickedMusicNext ->
            withMusic (Music.next model.music) model

        Tick ->
            let
                stepProgress =
                    StepProgress.tick model.stepProgress

                ( tickedModel, cmd ) =
                    if StepProgress.isFinished stepProgress then
                        completeStep model

                    else
                        ( { model | stepProgress = stepProgress }, Cmd.none )
            in
            ( tickedModel, Cmd.batch [ cmd, tickSoundEffect model stepProgress ] )

        Done ->
            if StepProgress.isAwaitingDone model.stepProgress then
                completeStep model

            else
                ( model, Cmd.none )

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


{-| Moves to the next step and starts it, unless the workout is completed or
the user starts each round manually.
-}
completeStep : Model -> ( Model, Cmd Msg )
completeStep model =
    let
        dayPlan =
            Maybe.map Plan.daysMapNextStep model.dayPlan

        isCompleted =
            isWorkoutCompleted model.currentRound dayPlan

        completedModel =
            { model | stepProgress = StepProgress.idle, dayPlan = dayPlan, isCompleted = isCompleted }
    in
    if isCompleted then
        stopRun ( completedModel, Cmd.none )

    else if isWaitingForNextRound model dayPlan then
        ( completedModel, Cmd.none )

    else
        ( completedModel, send Start )


countdownBeepSeconds : Int
countdownBeepSeconds =
    5


{-| Beeps during the last seconds of a countdown, and signals with a long beep
that the repetitions can begin once the preparation is over.
-}
tickSoundEffect : Model -> StepProgress -> Cmd msg
tickSoundEffect model stepProgress =
    case StepProgress.remaining stepProgress of
        Just remaining ->
            if remaining <= countdownBeepSeconds then
                playSoundEffect model "beep"

            else
                Cmd.none

        Nothing ->
            if StepProgress.isAwaitingDone stepProgress then
                playSoundEffect model "long_beep"

            else
                Cmd.none


{-| Silences the music and lets the screen sleep, keeping the commands already
produced.
-}
stopRun : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
stopRun ( model, cmd ) =
    let
        ( stoppedModel, musicCmd ) =
            withMusic (Music.stop model.music) model
    in
    ( stoppedModel, Cmd.batch [ cmd, musicCmd, WakeLock.release ] )


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
        , span [ class "text-sm tabular-nums opacity-80" ] [ text (amountLabel (Plan.stepAmount step)) ]
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


{-| Short label of a step amount, such as `40s` or `×12`.
-}
amountLabel : Amount -> String
amountLabel amount =
    case amount of
        Duration seconds ->
            String.fromInt seconds ++ "s"

        Count count ->
            "×" ++ String.fromInt count


bigNumberClasses : String
bigNumberClasses =
    "text-8xl font-bold tracking-tight text-pink-500 tabular-nums sm:text-9xl"


viewClock : Step -> Model -> Html Msg
viewClock step model =
    case ( StepProgress.remaining model.stepProgress, StepProgress.isAwaitingDone model.stepProgress, Plan.stepAmount step ) of
        ( Just remaining, _, _ ) ->
            div [ class bigNumberClasses ] [ text (String.fromInt remaining) ]

        ( Nothing, True, Count count ) ->
            viewRepetitions count

        _ ->
            button [ onClick Start, class (Button.primaryClasses ++ " px-10 py-3 text-lg") ] [ text "Start" ]


{-| Number of repetitions to perform, with the button moving to the next step.
-}
viewRepetitions : Int -> Html Msg
viewRepetitions count =
    div [ class "flex flex-col items-center" ]
        [ div [ class "flex items-baseline gap-2" ]
            [ span [ class bigNumberClasses ] [ text (String.fromInt count) ]
            , span [ class "text-2xl font-medium text-gray-400" ] [ text (repetitionsLabel count) ]
            ]
        , button [ onClick Done, class (Button.primaryClasses ++ " mt-6 px-10 py-3 text-lg") ] [ text "Done" ]
        , p [ class "mt-2 text-xs text-gray-400" ] [ text "or press Space" ]
        ]


repetitionsLabel : Int -> String
repetitionsLabel count =
    if count == 1 then
        "rep"

    else
        "reps"


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
        , viewClock step model
        , viewMusic (Music.status model.music)
        ]


viewMusic : Music.Status -> Html Msg
viewMusic status =
    case status of
        Music.Silent ->
            text ""

        Music.LoadingTracks ->
            viewMusicMessage "Loading music…"

        Music.Unavailable ->
            viewMusicMessage "The playlist couldn't be loaded"

        Music.Playing track ->
            viewMusicBar { track = track, isPaused = False }

        Music.Paused track ->
            viewMusicBar { track = track, isPaused = True }


viewMusicMessage : String -> Html Msg
viewMusicMessage message =
    p [ class "mt-8 text-sm text-gray-400" ] [ text message ]


viewMusicBar : { track : Track, isPaused : Bool } -> Html Msg
viewMusicBar { track, isPaused } =
    let
        ( playPauseLabel, playPauseIcon ) =
            if isPaused then
                ( "Play music", Icon.play "size-5" )

            else
                ( "Pause music", Icon.pause "size-5" )
    in
    div [ class "mt-8 flex w-full max-w-md items-center gap-3 rounded-xl bg-gray-50 px-4 py-2" ]
        [ Icon.musicalNote "size-5 shrink-0 text-pink-500"
        , a
            [ href (Audius.pageUrl track)
            , target "_blank"
            , rel "noopener noreferrer"
            , title "Open on Audius"
            , class "flex min-w-0 flex-1 flex-col text-left transition-colors hover:text-pink-600"
            ]
            [ span [ class "truncate text-sm font-medium text-gray-800" ] [ text track.title ]
            , span [ class "truncate text-xs text-gray-400" ] [ text track.artist ]
            ]
        , IconButton.view { label = playPauseLabel, isDisabled = False, onPress = ClickedMusicPlayPause } playPauseIcon
        , IconButton.view { label = "Next track", isDisabled = False, onPress = ClickedMusicNext } (Icon.forward "size-5")
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
            , viewActiveSettings (settings model)
            ]
        ]


{-| Discreet footer of the timer card reminding the settings in effect. Hovering
an indicator explains it.
-}
viewActiveSettings : Settings -> Html Msg
viewActiveSettings currentSettings =
    case ActiveSettings.fromSettings currentSettings of
        [] ->
            text ""

        indicators ->
            div [ class "flex flex-wrap items-center justify-center gap-4 border-t border-gray-100 px-6 py-3 text-gray-400" ]
                (List.map viewIndicator indicators)


viewIndicator : ActiveSettings.Indicator -> Html Msg
viewIndicator indicator =
    let
        iconClasses =
            "size-4"

        ( description, content ) =
            case indicator of
                ActiveSettings.PreRoundDelay seconds ->
                    ( String.fromInt seconds ++ " seconds added before each round"
                    , [ Icon.clock iconClasses, span [ class "text-xs tabular-nums" ] [ text (String.fromInt seconds ++ "s") ] ]
                    )

                ActiveSettings.AutoNextRound ->
                    ( "Moves to the next round automatically", [ Icon.arrowPath iconClasses ] )

                ActiveSettings.SoundEffects ->
                    ( "Sound effects enabled", [ Icon.speakerWave iconClasses ] )

                ActiveSettings.Music ->
                    ( "Music enabled", [ Icon.musicalNote iconClasses ] )

                ActiveSettings.WakeLock ->
                    ( "Keeps the screen awake", [ Icon.sun iconClasses ] )
    in
    span [ class "flex cursor-help items-center gap-1", title description, attribute "aria-label" description, attribute "role" "img" ] content


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
    setCurrentDayPlay { model | currentRound = 0, stepProgress = StepProgress.idle, isCompleted = False }


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
