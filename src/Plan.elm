module Plan exposing (Day(..), DaysMap, Plan, Step, authorName, daysMap, daysMapNextStep, decode, isBreakStep, stepName, stepSeconds)

import Dict exposing (Dict)
import Yaml.Decode as YamlDecode exposing (Error(..))
import Ziplist exposing (Ziplist)


type Day
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


dayFromString : String -> Maybe Day
dayFromString string =
    case string of
        "monday" ->
            Just Monday

        "tuesday" ->
            Just Tuesday

        "wednesday" ->
            Just Wednesday

        "thursday" ->
            Just Thursday

        "friday" ->
            Just Friday

        "saturday" ->
            Just Saturday

        "sunday" ->
            Just Sunday

        _ ->
            Nothing


type alias Author =
    { github : Maybe String
    , name : Maybe String
    , email : Maybe String
    }


type alias EffortWorkoutSpec =
    { name : String, seconds : Int }


type alias BreakWorkoutSpec =
    { name : Maybe String, seconds : Int }


type Step
    = Break BreakWorkoutSpec
    | Effort EffortWorkoutSpec


type alias DailyPlanning =
    { monday : Maybe DaysMap
    , tuesday : Maybe DaysMap
    , wednesday : Maybe DaysMap
    , thursday : Maybe DaysMap
    , friday : Maybe DaysMap
    , saturday : Maybe DaysMap
    , sunday : Maybe DaysMap
    }


type alias Steps =
    Ziplist Step


type alias DaysMap =
    { rounds : Int
    , steps : Steps
    }


type alias Planning =
    { days : DailyPlanning
    }


type alias Plan =
    { description : String
    , author : Author
    , planning : Planning
    }


stepSeconds : Step -> Int
stepSeconds step =
    case step of
        Break { seconds } ->
            seconds

        Effort { seconds } ->
            seconds


stepName : Step -> String
stepName step =
    case step of
        Break { name } ->
            Maybe.withDefault "Break" name

        Effort { name } ->
            name


daysMapNextStep : DaysMap -> DaysMap
daysMapNextStep map =
    { map | steps = Ziplist.next map.steps }


newDailyPlanning : DailyPlanning
newDailyPlanning =
    { monday = Nothing
    , tuesday = Nothing
    , wednesday = Nothing
    , thursday = Nothing
    , friday = Nothing
    , saturday = Nothing
    , sunday = Nothing
    }


daysMap : Day -> Plan -> Maybe DaysMap
daysMap day plan =
    case day of
        Monday ->
            plan.planning.days.monday

        Tuesday ->
            plan.planning.days.tuesday

        Wednesday ->
            plan.planning.days.wednesday

        Thursday ->
            plan.planning.days.thursday

        Friday ->
            plan.planning.days.friday

        Saturday ->
            plan.planning.days.saturday

        Sunday ->
            plan.planning.days.sunday


isBreakStep : Step -> Bool
isBreakStep step =
    case step of
        Break _ ->
            True

        _ ->
            False


putWorkout : Day -> Maybe DaysMap -> DailyPlanning -> DailyPlanning
putWorkout day workout dailyPlanning =
    case day of
        Monday ->
            { dailyPlanning | monday = workout }

        Tuesday ->
            { dailyPlanning | tuesday = workout }

        Wednesday ->
            { dailyPlanning | wednesday = workout }

        Thursday ->
            { dailyPlanning | thursday = workout }

        Friday ->
            { dailyPlanning | friday = workout }

        Saturday ->
            { dailyPlanning | saturday = workout }

        Sunday ->
            { dailyPlanning | sunday = workout }


authorName : String -> Author -> String
authorName fallback author =
    case author.name of
        Just name ->
            name

        _ ->
            case author.github of
                Just github ->
                    github

                _ ->
                    case author.email of
                        Just email ->
                            email

                        _ ->
                            fallback


convertDaysMap : Dict String DaysMap -> DailyPlanning
convertDaysMap inputDict =
    let
        folder key value acc =
            case dayFromString key of
                Just day ->
                    putWorkout day (Just value) acc

                Nothing ->
                    acc
    in
    Dict.foldl folder newDailyPlanning inputDict


decoder : YamlDecode.Decoder Plan
decoder =
    let
        authorDecoder =
            YamlDecode.map3 Author
                (YamlDecode.maybe (YamlDecode.field "github" YamlDecode.string))
                (YamlDecode.maybe (YamlDecode.field "name" YamlDecode.string))
                (YamlDecode.maybe (YamlDecode.field "email" YamlDecode.string))

        breakWorkoutDecoder =
            YamlDecode.map2 BreakWorkoutSpec
                (YamlDecode.maybe (YamlDecode.field "name" YamlDecode.string))
                (YamlDecode.field "seconds" YamlDecode.int)

        effortWorkoutDecoder =
            YamlDecode.map2 EffortWorkoutSpec
                (YamlDecode.field "name" YamlDecode.string)
                (YamlDecode.field "seconds" YamlDecode.int)

        stepConverter decodedType =
            case decodedType of
                Just "break" ->
                    YamlDecode.map Break breakWorkoutDecoder

                _ ->
                    YamlDecode.map Effort effortWorkoutDecoder

        stepDecoder =
            YamlDecode.maybe (YamlDecode.field "type" YamlDecode.string)
                |> YamlDecode.andThen stepConverter

        dailyPlanningDecoder =
            YamlDecode.map2 DaysMap
                (YamlDecode.field "rounds" YamlDecode.int)
                (YamlDecode.field "steps" (YamlDecode.list stepDecoder |> YamlDecode.map Ziplist.init))

        daysDecoder =
            YamlDecode.dict dailyPlanningDecoder
                |> YamlDecode.map convertDaysMap

        planningDecoder =
            YamlDecode.map Planning
                (YamlDecode.field "days" daysDecoder)
    in
    YamlDecode.map3 Plan
        (YamlDecode.field "description" YamlDecode.string)
        (YamlDecode.field "author" authorDecoder)
        (YamlDecode.field "planning" planningDecoder)


decode : String -> Result String Plan
decode input =
    case YamlDecode.fromString decoder input of
        Ok plan ->
            Ok plan

        Err (Parsing string) ->
            Err string

        Err (Decoding string) ->
            Err string
