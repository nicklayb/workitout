module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Plan exposing (Plan)
import Session exposing (Session)
import Ziplist exposing (Ziplist(..))


type alias Model =
    { session : Session
    , items : Ziplist String
    , plan : Result String Plan
    }


type Msg
    = Next


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, items = Ziplist.init [ "a", "b", "c", "d", "e" ], plan = Plan.decode "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( { model | items = Ziplist.next model.items }, Cmd.none )


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
        , button [ onClick Next ] [ text ">" ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Home"
    }
