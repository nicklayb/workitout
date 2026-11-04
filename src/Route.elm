module Route exposing (EditorTarget(..), PlanSource(..), Route(..), fromUrl, routeToString, urlChanged)

import Plan exposing (Plan)
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query


type Route
    = Home
    | RunPlan PlanSource (Maybe Plan.Day)
    | Settings
    | PlanEditor EditorTarget


{-| Where the plan to run comes from: the last opened plan file, a community
plan by its path or one of the user's plans by its identifier.
-}
type PlanSource
    = LastPlan
    | Community String
    | Custom Int


{-| Reads the source from the `path` and `custom` parameters, `path` winning
when both are given.
-}
planSourceQuery : Query.Parser PlanSource
planSourceQuery =
    Query.map2
        (\maybePath maybeCustomId ->
            case ( maybePath, maybeCustomId ) of
                ( Just path, _ ) ->
                    Community path

                ( Nothing, Just customId ) ->
                    Custom customId

                ( Nothing, Nothing ) ->
                    LastPlan
        )
        (Query.string "path")
        (Query.int "custom")


planSourceParams : PlanSource -> List UrlBuilder.QueryParameter
planSourceParams source =
    case source of
        LastPlan ->
            []

        Community path ->
            [ UrlBuilder.string "path" path ]

        Custom customId ->
            [ UrlBuilder.int "custom" customId ]


{-| What the plan editor opens: a new plan, a copy of a community plan by its
path or one of the user's plans by its identifier.
-}
type EditorTarget
    = NewPlan
    | Fork String
    | EditPlan Int


{-| Reads the target from the `fork` and `plan` parameters, `fork` winning
when both are given.
-}
editorTargetQuery : Query.Parser EditorTarget
editorTargetQuery =
    Query.map2
        (\maybeFork maybePlanId ->
            case ( maybeFork, maybePlanId ) of
                ( Just path, _ ) ->
                    Fork path

                ( Nothing, Just planId ) ->
                    EditPlan planId

                ( Nothing, Nothing ) ->
                    NewPlan
        )
        (Query.string "fork")
        (Query.int "plan")


editorTargetParams : EditorTarget -> List UrlBuilder.QueryParameter
editorTargetParams target =
    case target of
        NewPlan ->
            []

        Fork path ->
            [ UrlBuilder.string "fork" path ]

        EditPlan planId ->
            [ UrlBuilder.int "plan" planId ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map RunPlan (s "plans" </> s "run" <?> planSourceQuery <?> Query.enum "day" Plan.dayEnum)
        , Parser.map Settings (s "settings")
        , Parser.map PlanEditor (s "plans" </> s "editor" <?> editorTargetQuery)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString page =
    let
        ( pieces, query ) =
            case page of
                Home ->
                    ( [], [] )

                RunPlan source maybeDay ->
                    let
                        dayParam =
                            case maybeDay of
                                Just day ->
                                    [ UrlBuilder.string "day" (Plan.dayToString day) ]

                                _ ->
                                    []

                        queryParams =
                            dayParam ++ planSourceParams source
                    in
                    ( [ "plans", "run" ], queryParams )

                Settings ->
                    ( [ "settings" ], [] )

                PlanEditor target ->
                    ( [ "plans", "editor" ], editorTargetParams target )
    in
    "/" ++ UrlBuilder.relative pieces query


urlChanged : Url -> Url -> Bool
urlChanged first second =
    List.any identity
        [ first.host /= second.host
        , first.path /= second.path
        , not <| maybeEquals first.query second.query
        , not <| maybeEquals first.fragment second.fragment
        ]


maybeEquals : Maybe String -> Maybe String -> Bool
maybeEquals first second =
    Maybe.withDefault "" first == Maybe.withDefault "" second
