module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Dict
import GitHub
import Html exposing (Html, a, div, h1, li, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import RemotePlan exposing (RemotePlan)
import Resource exposing (Resource(..))
import Route exposing (Route)
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , indexedPlans = Loading Nothing
      , openedFolders = Set.insert "/" Set.empty
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

        ToggleFolder folderKey ->
            ( { model | openedFolders = toggleSet folderKey model.openedFolders }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession { session } =
    session


viewPlans : ( String, String ) -> Folder RemotePlan -> Model -> Html Msg
viewPlans ( folderKey, title ) (Folder plans) model =
    let
        viewFiles =
            Dict.foldl viewFile []

        viewFile _ file acc =
            a [ class "border border-gray-300 rounded-sm px-2 py-0.5 text-pink-500 hover:text-pink-600 mb-2", href <| Route.routeToString (Route.RunPlan (Just file.path) Nothing) ] [ text file.description ] :: acc

        viewFolders =
            Dict.foldl viewFolder []

        viewFolder key folder acc =
            viewPlans ( folderKey ++ "_" ++ key, key ) folder model :: acc

        isToggled =
            Set.member folderKey model.openedFolders

        ( icon, maybeViewContent ) =
            if isToggled then
                ( "-"
                , [ li [ class "pl-4" ]
                        (viewFiles plans.files
                            ++ viewFolders plans.folders
                        )
                  ]
                )

            else
                ( "+", [] )
    in
    ul [ class "border border-gray-200 rounded-md p-2" ]
        (li [ class "mb-2 cursor-pointer hover:text-pink-700", onClick (ToggleFolder folderKey) ]
            [ span [ class "text-xl mr-2" ] [ text icon ]
            , span [] [ text title ]
            ]
            :: maybeViewContent
        )


viewContent : Model -> Html Msg
viewContent model =
    let
        innerContent =
            case model.indexedPlans of
                Loaded plans ->
                    div [ class "max-w-2xl w-full" ] [ viewPlans ( "/", "/" ) plans model ]

                _ ->
                    div [] [ text "Loading..." ]
    in
    div []
        [ h1 [ class "text-center text-pink-500 text-4xl py-4" ] [ text "Workitout" ]
        , div [ class "flex justify-center" ] [ innerContent ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Home"
    }
