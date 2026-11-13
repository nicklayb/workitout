module Plan exposing (Amount(..), Author, BreakWorkoutSpec, DailyPlanning, Day(..), DaysMap, EffortWorkoutSpec, Plan, Schedule(..), Step(..), allDays, authorName, dayEnum, dayFromString, dayFromTimeDay, dayToString, daysMap, daysMapNextStep, decode, isBreakStep, isScheduledWeekly, stepAmount, stepDescription, stepName, weekdayEquals, weekdays)

import Dict exposing (Dict)
import Difficulty exposing (Difficulty)
import Time exposing (Weekday(..))
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


{-| Every day of the week, starting on Sunday.
-}
allDays : List Day
allDays =
    [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday ]


{-| Every day of the week, starting on Sunday, along with whether the plan has
a workout that day.
-}
weekdays : Plan -> List ( Day, Bool )
weekdays plan =
    List.map (\day -> ( day, daysMap day plan /= Nothing )) allDays


weekdayEquals : Day -> Day -> Bool
weekdayEquals left right =
    case ( left, right ) of
        ( Sunday, Sunday ) ->
            True

        ( Monday, Monday ) ->
            True

        ( Tuesday, Tuesday ) ->
            True

        ( Wednesday, Wednesday ) ->
            True

        ( Thursday, Thursday ) ->
            True

        ( Friday, Friday ) ->
            True

        ( Saturday, Saturday ) ->
            True

        _ ->
            False


dayEnum : Dict String Day
dayEnum =
    Dict.fromList
        [ ( "monday", Monday )
        , ( "tuesday", Tuesday )
        , ( "wednesday", Wednesday )
        , ( "thursday", Thursday )
        , ( "friday", Friday )
        , ( "saturday", Saturday )
        , ( "sunday", Sunday )
        ]


dayToString : Day -> String
dayToString day =
    case day of
        Monday ->
            "monday"

        Tuesday ->
            "tuesday"

        Wednesday ->
            "wednesday"

        Thursday ->
            "thursday"

        Friday ->
            "friday"

        Saturday ->
            "saturday"

        Sunday ->
            "sunday"


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


dayFromTimeDay : Time.Weekday -> Day
dayFromTimeDay weekday =
    case weekday of
        Sun ->
            Sunday

        Mon ->
            Monday

        Tue ->
            Tuesday

        Wed ->
            Wednesday

        Thu ->
            Thursday

        Fri ->
            Friday

        Sat ->
            Saturday


type alias Author =
    { github : Maybe String
    , name : Maybe String
    , email : Maybe String
    }


{-| What an effort asks for: holding it for a number of seconds (`seconds`
in plan files) or performing a number of repetitions (`count`).
-}
type Amount
    = Duration Int
    | Count Int


type alias EffortWorkoutSpec =
    { name : String, amount : Amount, description : Maybe String }


type alias BreakWorkoutSpec =
    { name : Maybe String, seconds : Int, description : Maybe String }


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


{-| When the workouts happen: a different workout per weekday (`planning` in
plan files) or the same workout every day (`workout` in plan files).
-}
type Schedule
    = Weekly DailyPlanning
    | EveryDay DaysMap


type alias Plan =
    { description : String
    , author : Author
    , schedule : Schedule
    , difficulty : Difficulty
    , forkedFrom : Maybe String
    }


{-| What the step asks for. Breaks always last a number of seconds.
-}
stepAmount : Step -> Amount
stepAmount step =
    case step of
        Break { seconds } ->
            Duration seconds

        Effort { amount } ->
            amount


{-| Explanation of how to perform the step, if the plan author provided one.
-}
stepDescription : Step -> Maybe String
stepDescription step =
    case step of
        Break { description } ->
            description

        Effort { description } ->
            description


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


{-| Workout of a day, which is the single workout of plans scheduled every
day.
-}
daysMap : Day -> Plan -> Maybe DaysMap
daysMap day plan =
    case plan.schedule of
        Weekly dailyPlanning ->
            dailyPlanningDay day dailyPlanning

        EveryDay workout ->
            Just workout


{-| Whether the plan has a different workout per weekday rather than the same
one every day.
-}
isScheduledWeekly : Plan -> Bool
isScheduledWeekly plan =
    case plan.schedule of
        Weekly _ ->
            True

        EveryDay _ ->
            False


dailyPlanningDay : Day -> DailyPlanning -> Maybe DaysMap
dailyPlanningDay day dailyPlanning =
    case day of
        Monday ->
            dailyPlanning.monday

        Tuesday ->
            dailyPlanning.tuesday

        Wednesday ->
            dailyPlanning.wednesday

        Thursday ->
            dailyPlanning.thursday

        Friday ->
            dailyPlanning.friday

        Saturday ->
            dailyPlanning.saturday

        Sunday ->
            dailyPlanning.sunday


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
        descriptionDecoder =
            YamlDecode.maybe (YamlDecode.field "description" YamlDecode.string)

        authorDecoder =
            YamlDecode.map3 Author
                (YamlDecode.maybe (YamlDecode.field "github" YamlDecode.string))
                (YamlDecode.maybe (YamlDecode.field "name" YamlDecode.string))
                (YamlDecode.maybe (YamlDecode.field "email" YamlDecode.string))

        breakWorkoutDecoder =
            YamlDecode.map3 BreakWorkoutSpec
                (YamlDecode.maybe (YamlDecode.field "name" YamlDecode.string))
                (YamlDecode.field "seconds" YamlDecode.int)
                descriptionDecoder

        effortWorkoutDecoder =
            YamlDecode.map3 EffortWorkoutSpec
                (YamlDecode.field "name" YamlDecode.string)
                amountDecoder
                descriptionDecoder

        stepConverter decodedType =
            case decodedType of
                Just "break" ->
                    YamlDecode.map Break breakWorkoutDecoder

                _ ->
                    YamlDecode.map Effort effortWorkoutDecoder

        stepDecoder =
            YamlDecode.maybe (YamlDecode.field "type" YamlDecode.string)
                |> YamlDecode.andThen stepConverter

        daysMapDecoder =
            YamlDecode.map2 DaysMap
                (YamlDecode.field "rounds" YamlDecode.int)
                (YamlDecode.field "steps" (YamlDecode.list stepDecoder |> YamlDecode.map Ziplist.init))

        daysDecoder =
            YamlDecode.dict daysMapDecoder
                |> YamlDecode.map convertDaysMap

        planningDecoder =
            YamlDecode.field "days" daysDecoder

        difficultyDecoder =
            YamlDecode.maybe (YamlDecode.field "difficulty" Difficulty.yamlDecoder)
                |> YamlDecode.map (Maybe.withDefault Difficulty.default)
    in
    YamlDecode.map5 Plan
        (YamlDecode.field "description" YamlDecode.string)
        (YamlDecode.field "author" authorDecoder)
        (scheduleDecoder planningDecoder daysMapDecoder)
        difficultyDecoder
        (YamlDecode.maybe (YamlDecode.field "forkedFrom" YamlDecode.string))


{-| Decodes either a weekly `planning` or a single `workout` for every day.
-}
scheduleDecoder : YamlDecode.Decoder DailyPlanning -> YamlDecode.Decoder DaysMap -> YamlDecode.Decoder Schedule
scheduleDecoder planningDecoder workoutDecoder =
    YamlDecode.map2 Tuple.pair
        (YamlDecode.maybe (YamlDecode.field "planning" planningDecoder))
        (YamlDecode.maybe (YamlDecode.field "workout" workoutDecoder))
        |> YamlDecode.andThen
            (\fields ->
                case fields of
                    ( Just dailyPlanning, Nothing ) ->
                        YamlDecode.succeed (Weekly dailyPlanning)

                    ( Nothing, Just workout ) ->
                        YamlDecode.succeed (EveryDay workout)

                    ( Just _, Just _ ) ->
                        YamlDecode.fail "A plan can't have both planning and workout"

                    ( Nothing, Nothing ) ->
                        YamlDecode.fail "A plan needs either a planning or a workout"
            )


{-| Decodes the amount of an effort, which has either `seconds` or `count`.
-}
amountDecoder : YamlDecode.Decoder Amount
amountDecoder =
    YamlDecode.map2 Tuple.pair
        (YamlDecode.maybe (YamlDecode.field "seconds" YamlDecode.int))
        (YamlDecode.maybe (YamlDecode.field "count" YamlDecode.int))
        |> YamlDecode.andThen
            (\fields ->
                case fields of
                    ( Just seconds, Nothing ) ->
                        YamlDecode.succeed (Duration seconds)

                    ( Nothing, Just count ) ->
                        YamlDecode.succeed (Count count)

                    ( Just _, Just _ ) ->
                        YamlDecode.fail "An effort can't have both seconds and count"

                    ( Nothing, Nothing ) ->
                        YamlDecode.fail "An effort needs either seconds or count"
            )


decode : String -> Result String Plan
decode input =
    case YamlDecode.fromString decoder input of
        Ok plan ->
            Ok plan

        Err (Parsing string) ->
            Err string

        Err (Decoding string) ->
            Err string
