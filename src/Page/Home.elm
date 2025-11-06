module Page.Home exposing (Model, Msg, init, toSession, update, view)

import GitHub
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode
import Plan exposing (Plan)
import RemotePlan exposing (RemotePlan)
import Resource exposing (Resource(..))
import Session exposing (Session)
import Tree exposing (Folder)
import Ziplist exposing (Ziplist(..))


type alias Model =
    { session : Session
    , items : Ziplist String
    , indexedPlans : Resource (Folder RemotePlan)
    }


type Msg
    = IndexFetched (GitHub.HttpResponse (Folder RemotePlan))


inputStr =
    "{\"files\":{\"example.yml\":{\"description\":\"ChatGPT generated plan\",\"author_name\":\"Nicolas Boisvert\",\"download_url\":\"https://raw.githubusercontent.com/nicklayb/workitout/plans/plans/example.yml\",\"sha\":\"7798b70a5ac215144491baf00f95a5139f2590a9\"}},\"folders\":{}}"


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , items = Ziplist.init [ "a", "b", "c", "d", "e" ]
      , indexedPlans = NotLoaded
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


viewItems : Model -> Html Msg
viewItems model =
    case model.items of
        Empty ->
            text "empty"

        Ziplist back current front ->
            let
                viewItem isCurrent item =
                    let
                        itemClasses =
                            if isCurrent then
                                [ class "text-pink-500" ]

                            else
                                []
                    in
                    div itemClasses [ text item ]
            in
            div [] (List.map (viewItem False) (List.reverse back) ++ viewItem True current :: List.map (viewItem False) front)


viewContent : Model -> Html Msg
viewContent model =
    div []
        [ viewItems model
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Home"
    }
