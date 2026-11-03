module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Dict
import Difficulty exposing (Difficulty)
import GitHub
import Html exposing (Html, a, button, div, h1, li, p, span, text, ul)
import Html.Attributes exposing (class, href, title)
import Html.Events exposing (onClick)
import RemotePlan exposing (RemotePlan)
import Resource exposing (Resource(..))
import Route
import Session exposing (Session)
import Set exposing (Set)
import Tree exposing (Folder(..))


type alias Model =
    { session : Session
    , indexedPlans : Resource (Folder RemotePlan)
    , openedFolders : Set String
    }


type Msg
    = IndexFetched (GitHub.HttpResponse (Folder RemotePlan))
    | ToggleFolder String


pathSeparator : String
pathSeparator =
    "/"


skeletonRowCount : Int
skeletonRowCount =
    4


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , indexedPlans = Loading Nothing
      , openedFolders = Set.empty
      }
    , GitHub.fetchIndex IndexFetched
    )


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet item set =
    if Set.member item set then
        Set.remove item set

    else
        Set.insert item set


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexFetched (Ok plans) ->
            ( { model | indexedPlans = Loaded plans }, Cmd.none )

        IndexFetched (Err _) ->
            ( { model | indexedPlans = Failed "Could not load the plan index." }, Cmd.none )

        ToggleFolder folderKey ->
            ( { model | openedFolders = toggleSet folderKey model.openedFolders }, Cmd.none )


toSession : Model -> Session
toSession { session } =
    session


{-| Renders the content of a folder: sub folders first, then plans, both sorted by name.
-}
viewFolderContent : String -> Folder RemotePlan -> Model -> List (Html Msg)
viewFolderContent parentKey (Folder { files, folders }) model =
    let
        folderItems =
            folders
                |> Dict.toList
                |> List.map (\( name, folder ) -> viewFolder (parentKey ++ pathSeparator ++ name) name folder model)

        fileItems =
            files
                |> Dict.values
                |> List.map viewPlan
    in
    folderItems ++ fileItems


viewFolder : String -> String -> Folder RemotePlan -> Model -> Html Msg
viewFolder folderKey name folder model =
    let
        isOpened =
            Set.member folderKey model.openedFolders

        chevronRotation =
            if isOpened then
                "rotate-45"

            else
                "-rotate-45"

        children =
            if isOpened then
                [ ul [ class "ml-4 mt-1 space-y-1 border-l border-gray-200 pl-3" ] (viewFolderContent folderKey folder model) ]

            else
                []
    in
    li []
        (button
            [ class "group flex w-full items-center gap-3 rounded-lg px-3 py-2 text-left text-gray-700 transition-colors hover:bg-gray-50 cursor-pointer"
            , onClick (ToggleFolder folderKey)
            ]
            [ span [ class ("size-2 border-b-2 border-r-2 border-gray-400 transition-transform duration-200 group-hover:border-pink-500 " ++ chevronRotation) ] []
            , span [ class "flex-1 font-medium capitalize" ] [ text name ]
            , span [ class "rounded-full bg-pink-50 px-2 py-0.5 text-xs font-medium text-pink-600" ] [ text (String.fromInt (Tree.fileCount folder)) ]
            ]
            :: children
        )


viewPlan : RemotePlan -> Html Msg
viewPlan plan =
    li []
        [ a
            [ class "group flex items-center gap-3 rounded-lg px-3 py-2 transition-colors hover:bg-pink-50"
            , href (Route.routeToString (Route.RunPlan (Just plan.path) Nothing))
            ]
            ([ viewDifficultyDot plan.difficulty
             , div [ class "flex min-w-0 flex-1 flex-col" ]
                [ span [ class "truncate text-gray-800 group-hover:text-pink-600" ] [ text plan.description ]
                , span [ class "truncate text-xs text-gray-400" ] [ text (plan.author_name ++ " · " ++ plan.name) ]
                ]
             ]
                ++ viewDifficultyBadge plan.difficulty
                ++ [ span [ class "text-pink-500 opacity-0 transition-opacity group-hover:opacity-100" ] [ text "→" ] ]
            )
        ]


{-| Accessible text describing a difficulty, used as tooltip and screen reader label.
-}
difficultyDescription : Difficulty -> String
difficultyDescription difficulty =
    "Difficulty: " ++ Difficulty.label difficulty


{-| Colored dot indicating the difficulty of a plan, gray when unrated.
-}
viewDifficultyDot : Difficulty -> Html Msg
viewDifficultyDot difficulty =
    span
        [ class ("size-2 shrink-0 rounded-full ring-4 " ++ Difficulty.dotClasses difficulty)
        , title (difficultyDescription difficulty)
        ]
        [ span [ class "sr-only" ] [ text (difficultyDescription difficulty) ] ]


{-| Labeled pill showing the difficulty of a plan, omitted when unrated to keep the list quiet.
-}
viewDifficultyBadge : Difficulty -> List (Html Msg)
viewDifficultyBadge difficulty =
    case difficulty of
        Difficulty.Unset ->
            []

        _ ->
            [ span
                [ class ("shrink-0 rounded-full px-2 py-0.5 text-xs font-medium ring-1 ring-inset " ++ Difficulty.badgeClasses difficulty)
                , title (difficultyDescription difficulty)
                ]
                [ text (Difficulty.label difficulty) ]
            ]


viewTree : Folder RemotePlan -> Model -> Html Msg
viewTree plans model =
    if Tree.isEmpty plans then
        p [ class "px-3 py-6 text-center text-gray-400" ] [ text "No plans available yet." ]

    else
        ul [ class "space-y-1" ] (viewFolderContent "" plans model)


viewSkeleton : Html Msg
viewSkeleton =
    let
        viewSkeletonRow _ =
            div [ class "flex items-center gap-3 px-3 py-2" ]
                [ div [ class "size-2 rounded-full bg-gray-200" ] []
                , div [ class "h-4 flex-1 rounded bg-gray-200" ] []
                ]
    in
    div [ class "animate-pulse space-y-1" ] (List.map viewSkeletonRow (List.range 1 skeletonRowCount))


viewError : String -> Html Msg
viewError error =
    p [ class "rounded-lg bg-red-50 px-3 py-4 text-center text-red-600" ] [ text error ]


viewContent : Model -> Html Msg
viewContent model =
    let
        innerContent =
            case model.indexedPlans of
                Loaded plans ->
                    viewTree plans model

                Failed error ->
                    viewError error

                _ ->
                    viewSkeleton
    in
    div [ class "min-h-full bg-gray-50 px-4 py-10" ]
        [ div [ class "mx-auto max-w-2xl" ]
            [ h1 [ class "text-center text-4xl font-bold tracking-tight text-pink-500" ] [ text "Workitout" ]
            , p [ class "mt-2 mb-8 text-center text-gray-500" ] [ text "Pick a community plan to get started" ]
            , div [ class "rounded-2xl bg-white p-3 shadow-sm ring-1 ring-gray-200" ] [ innerContent ]
            ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Home"
    }
