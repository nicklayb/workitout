port module Page.RunPlan exposing (Model, Msg, init, toSession, update, view)

import Cmd.Extra exposing (delay, send)
import File exposing (File)
import Html exposing (Html, a, button, div, h2, h4, input, span, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (Decoder)
import Plan exposing (Day(..), Plan, Step)
import Resource exposing (Resource)
import Session exposing (Session)
import Task
import Timer exposing (Timer)
import Ziplist exposing (Position(..))


port playSound : String -> Cmd msg


type alias Model =
    { session : Session
    , planPath : Maybe String
    , file : Maybe File
    , plan : Resource Plan
    , dayPlan : Maybe Plan.DaysMap
    , currentTimer : Timer
    , currentRound : Int
    }


type Msg
    = GotFile (Maybe File)
    | PlanLoaded (Result String Plan)
    | Start
    | Tick


init : Maybe String -> Session -> ( Model, Cmd Msg )
init maybePath session =
    ( { session = session
      , planPath = maybePath
      , file = Nothing
      , plan = Resource.init
      , dayPlan = Nothing
      , currentTimer = Timer.init 0
      , currentRound = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile (Just file) ->
            ( { model | file = Just file }, Task.perform PlanLoaded (File.toString file |> Task.map Plan.decode) )

        PlanLoaded (Ok plan) ->
            ( { model | plan = Resource.loaded plan, dayPlan = Plan.daysMap Monday plan }, Cmd.none )

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

        _ ->
            ( model, Cmd.none )


currentStep : Model -> Maybe Step
currentStep model =
    Maybe.andThen (.steps >> Ziplist.current) model.dayPlan


toSession : Model -> Session
toSession { session } =
    session


viewPlanHeader : Plan -> Model -> Html Msg
viewPlanHeader plan model =
    let
        viewAuthor author =
            case author.email of
                Just email ->
                    a [ class "text-pink-600", href ("mailto:" ++ email) ] [ text author.name ]

                Nothing ->
                    text author.name
    in
    div [ class "flex flex-col items-end pr-3" ]
        [ h2 [ class "text-2xl" ] [ text plan.description ]
        , h4 [ class "text-xs" ] [ viewAuthor plan.author ]
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
                    [ class "px-2 py-3 mb-2 rounded-md" ]

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
            div (defaultClasses ++ otherClasses) [ text (Plan.stepName step) ]
    in
    case model.dayPlan of
        Just daysMap ->
            div [] (viewRoundCounter :: Ziplist.map viewStep daysMap.steps)

        Nothing ->
            div [] [ text "Nothing planned for today" ]


viewCurrentStep : Model -> Html Msg
viewCurrentStep model =
    let
        step =
            currentStep model

        viewClock =
            if Timer.isRunning model.currentTimer then
                div [ class "text-6xl" ] [ text (String.fromInt (Timer.remaining model.currentTimer)) ]

            else
                div [] [ button [ onClick Start, class "rounded-md bg-pink-600 text-white px-3 py-2" ] [ text "Start" ] ]
    in
    case step of
        Just stepValue ->
            div [ class "flex flex-col justify-center items-center h-full" ]
                [ div [ class "text-2xl mb-4" ] [ text (Plan.stepName stepValue) ]
                , viewClock
                ]

        Nothing ->
            div [] []


viewPlan : Plan -> Model -> Html Msg
viewPlan plan model =
    div [ class "flex h-full" ]
        [ div [ class "w-1/4 px-2" ] [ viewSteps model ]
        , div [ class "w-full" ]
            [ viewPlanHeader plan model
            , viewCurrentStep model
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case ( model.plan, model.planPath ) of
        ( Resource.Loaded plan, _ ) ->
            viewPlan plan model

        ( _, Just path ) ->
            div [] [ text path ]

        ( _, Nothing ) ->
            viewUploadPath model


viewUploadPath : Model -> Html Msg
viewUploadPath model =
    div []
        [ input [ type_ "file", on "change" (Json.Decode.map GotFile filesDecoder) ] []
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Run Plan"
    }


filesDecoder : Decoder (Maybe File)
filesDecoder =
    Json.Decode.at [ "target", "files" ] (Json.Decode.list File.decoder)
        |> Json.Decode.map List.head
