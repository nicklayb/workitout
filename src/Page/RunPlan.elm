port module Page.RunPlan exposing (Model, Msg, init, toSession, update, view)

import Cmd.Extra exposing (delay, send)
import File exposing (File)
import GitHub
import Html exposing (Html, a, button, div, h2, h4, input, label, span, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (Decoder)
import Plan exposing (Day(..), Plan, Step, stepSeconds)
import Resource exposing (Resource)
import Session exposing (Session)
import Task
import Timer exposing (Timer)
import Ziplist exposing (Position(..))


port playSound : String -> Cmd msg


port storeLastPlan : Maybe String -> Cmd msg


type alias Model =
    { session : Session
    , planPath : Maybe String
    , file : Maybe File
    , plan : Resource Plan
    , dayPlan : Maybe Plan.DaysMap
    , currentTimer : Timer
    , currentRound : Int
    , lastPlan : Maybe String
    , currentDay : Plan.Day
    }


type Msg
    = GotFile (Maybe File)
    | PlanLoaded { result : Result String Plan, storeLastPlan : Maybe String }
    | Start
    | Tick
    | ClosePlan
    | FileFetched (GitHub.HttpResponse String)
    | ChangeDay Plan.Day


init : Maybe String -> Maybe Plan.Day -> Session -> ( Model, Cmd Msg )
init maybePath maybeDay session =
    let
        currentDay =
            case maybeDay of
                Just day ->
                    day

                _ ->
                    Plan.dayFromTimeDay session.currentDay

        model =
            { session = session
            , planPath = maybePath
            , file = Nothing
            , plan = Resource.init
            , dayPlan = Nothing
            , currentTimer = Timer.init 0
            , currentRound = 0
            , lastPlan = session.appConfig.lastPlan
            , currentDay = currentDay
            }
    in
    ( model, initCommand model )


initCommand : Model -> Cmd Msg
initCommand model =
    let
        maybeDecodeLastPlan =
            case ( model.planPath, model.lastPlan ) of
                ( Nothing, Just string ) ->
                    Cmd.Extra.send (PlanLoaded { result = Plan.decode string, storeLastPlan = Nothing })

                ( Just planPath, _ ) ->
                    GitHub.fetchContent FileFetched planPath

                _ ->
                    Cmd.none
    in
    Cmd.batch [ maybeDecodeLastPlan ]


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
                seconds =
                    currentStep model
                        |> Maybe.map Plan.stepSeconds
                        |> Maybe.withDefault 0

                timer =
                    seconds
                        |> Timer.init
                        |> Timer.start

                round =
                    if Maybe.map (.steps >> Ziplist.isReset) model.dayPlan |> Maybe.withDefault False then
                        model.currentRound + 1

                    else
                        model.currentRound

                cmd =
                    Cmd.batch
                        [ delay 1 Tick
                        , playSound "long_beep"
                        ]
            in
            ( { model | currentTimer = timer, currentRound = round }, cmd )

        ClosePlan ->
            ( { model | plan = Resource.init, lastPlan = Nothing }, storeLastPlan Nothing )

        ChangeDay day ->
            ( setCurrentDayPlay { model | currentDay = day }, Cmd.none )

        Tick ->
            let
                timer =
                    Timer.tick model.currentTimer

                cmd =
                    if Timer.isRunning timer then
                        delay 1 Tick

                    else
                        case model.dayPlan of
                            Just { rounds } ->
                                if rounds == model.currentRound then
                                    Cmd.none

                                else
                                    send Start

                            Nothing ->
                                Cmd.none

                remaining =
                    Timer.remaining timer

                playSoundCmd =
                    if remaining <= 5 then
                        playSound "beep"

                    else
                        Cmd.none

                dayPlan =
                    if Timer.isRunning timer then
                        model.dayPlan

                    else
                        Maybe.map Plan.daysMapNextStep model.dayPlan
            in
            ( { model | currentTimer = timer, dayPlan = dayPlan }, Cmd.batch [ cmd, playSoundCmd ] )

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


currentStep : Model -> Maybe Step
currentStep model =
    Maybe.andThen (.steps >> Ziplist.current) model.dayPlan


toSession : Model -> Session
toSession { session } =
    session


viewPlanHeader : Plan -> Html Msg
viewPlanHeader plan =
    let
        name =
            Plan.authorName "Unknown" plan.author

        viewAuthor author =
            case author.email of
                Just email ->
                    a [ class "text-pink-600", href ("mailto:" ++ email) ] [ text name ]

                Nothing ->
                    text name
    in
    div [ class "flex justify-end items-center pr-3 pt-2" ]
        [ div [ class "flex flex-col" ]
            [ h2 [ class "text-2xl" ] [ text plan.description ]
            , h4 [ class "text-xs text-right" ] [ viewAuthor plan.author ]
            ]
        , div [] [ button [ onClick ClosePlan, class "ml-2 rounded-full text-white w-14 h-14 bg-pink-500 hover:bg-pink-600 flex justify-center items-center cursor-pointer text-5xl" ] [ text "×" ] ]
        ]


viewSteps : Model -> Html Msg
viewSteps model =
    let
        viewRoundCounter =
            case model.dayPlan of
                Just { rounds } ->
                    div [ class "text-4xl text-center py-6" ] [ text (String.fromInt model.currentRound ++ " / " ++ String.fromInt rounds) ]

                Nothing ->
                    div [] []

        viewStep position step =
            let
                isCurrent =
                    Ziplist.positionEquals position Current

                isBreak =
                    Plan.isBreakStep step

                defaultClasses =
                    [ class "px-2 py-3 mb-2 rounded-md flex justify-between" ]

                otherClasses =
                    case ( isCurrent, isBreak ) of
                        ( True, True ) ->
                            [ class "bg-pink-200 text-pink-500" ]

                        ( True, False ) ->
                            [ class "bg-pink-500 text-white" ]

                        ( False, True ) ->
                            [ class "bg-gray-100 text-pink-300" ]

                        ( False, False ) ->
                            [ class "bg-gray-100 text-gray-500" ]
            in
            div (defaultClasses ++ otherClasses)
                [ span [] [ text (Plan.stepName step) ]
                , span [] [ text (String.fromInt (Plan.stepSeconds step) ++ "s") ]
                ]
    in
    case model.dayPlan of
        Just daysMap ->
            div [] (viewRoundCounter :: Ziplist.map viewStep daysMap.steps)

        Nothing ->
            div [ class "text-center mt-6 text-gray-600" ] [ text ("Nothing planned for " ++ Plan.dayToString model.currentDay) ]


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    let
        step =
            currentStep model

        viewClock =
            if Timer.isRunning model.currentTimer then
                div [ class "text-6xl" ] [ text (String.fromInt (Timer.remaining model.currentTimer)) ]

            else
                div [] [ button [ onClick Start, class "rounded-md bg-pink-500 hover:bg-pink-600 text-white px-3 py-2" ] [ text "Start" ] ]
    in
    case step of
        Just stepValue ->
            div [ class "flex flex-col justify-center items-center h-full" ]
                [ div [ class "text-2xl mb-4" ] [ text (Plan.stepName stepValue) ]
                , viewClock
                ]

        Nothing ->
            div [] []


viewWeek : Plan -> Model -> Html Msg
viewWeek plan model =
    let
        viewWeekday ( weekday, hasWorkout ) =
            let
                defaultClasses =
                    "px-1 py-0.5 rounded-md text-pink-500 hover:bg-pink-200 min-w-10 text-center cursor-pointer"

                activeClasses =
                    if Plan.weekdayEquals weekday model.currentDay then
                        "bg-pink-500 text-white shadow-md"

                    else
                        ""
            in
            div [ class (defaultClasses ++ " " ++ activeClasses), onClick (ChangeDay weekday) ] [ text (String.slice 0 3 (Plan.dayToString weekday)) ]
    in
    div [ class "flex justify-evenly mt-4" ] (List.map viewWeekday (Plan.weekdays plan))


viewPlan : Plan -> Model -> Html Msg
viewPlan plan model =
    div [ class "flex h-full" ]
        [ div [ class "w-1/4 px-2" ]
            [ viewWeek plan model
            , viewSteps model
            ]
        , div [ class "w-full" ]
            [ viewPlanHeader plan
            , viewCurrentStep model
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case ( model.plan, model.planPath, model.lastPlan ) of
        ( Resource.Loaded plan, _, _ ) ->
            viewPlan plan model

        ( Resource.Failed error, _, _ ) ->
            div []
                [ div [ class "text-center text-red-500 py-6 bg-red-200" ] [ text error ]
                , viewUploadPath model
                ]

        ( _, Nothing, Just _ ) ->
            viewLoading

        ( _, _, _ ) ->
            viewUploadPath model


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


viewUploadPath : Model -> Html Msg
viewUploadPath model =
    div [ class "flex justify-center items-center h-full flex-col" ]
        [ div [ class "mb-3 text-2xl" ] [ text "Upload a plan" ]
        , label []
            [ div [ class "bg-pink-500 hover:bg-pink-600 px-5 py-2 rounded-md text-white" ] [ text "Upload" ]
            , input [ type_ "file", on "change" (Json.Decode.map GotFile filesDecoder), class "hidden" ] []
            ]
        , div [ class "mt-2" ] [ a [ class "text-pink-500 cursor-pointer", href "/" ] [ text "Or return home to select a community provided plan" ] ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Run Plan"
    }


putLoadedPlan : Plan -> Model -> Model
putLoadedPlan plan model =
    setCurrentDayPlay { model | plan = Resource.loaded plan }


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
