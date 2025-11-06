module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Dict
import GitHub
import Html exposing (Html, a, dd, div, dl, dt, h1, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Json.Decode
import Plan exposing (Plan)
import RemotePlan exposing (RemotePlan)
import Resource exposing (Resource(..))
import Route exposing (Route)
import Session exposing (Session)
import Tree exposing (Folder(..))


type alias Model =
    { session : Session
    , indexedPlans : Resource (Folder RemotePlan)
    }


type Msg
    = IndexFetched (GitHub.HttpResponse (Folder RemotePlan))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , indexedPlans = Loading Nothing
      }
    , GitHub.fetchIndex IndexFetched
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexFetched (Ok plans) ->
            ( { model | indexedPlans = Loaded plans }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession { session } =
    session


viewPlans : String -> Folder RemotePlan -> Model -> Html Msg
viewPlans title (Folder plans) model =
    let
        viewFiles =
            Dict.foldl viewFile []

        viewFile _ file acc =
            a [ href <| Route.routeToString (Route.RunPlan (Just file.path) Nothing) ] [ text file.description ] :: acc

        viewFolders =
            Dict.foldl viewFolder []

        viewFolder key folder acc =
            viewPlans key folder model :: acc
    in
    dl []
        [ dt [] [ text title ]
        , dd [ class "pl-4" ]
            (viewFiles plans.files
                ++ viewFolders plans.folders
            )
        ]


viewContent : Model -> Html Msg
viewContent model =
    let
        innerContent =
            case model.indexedPlans of
                Loaded plans ->
                    viewPlans "/" plans model

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
