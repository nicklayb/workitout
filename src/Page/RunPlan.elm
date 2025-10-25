module Page.RunPlan exposing (Model, Msg, init, toSession, update, view)

import Counter
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Plan exposing (Plan)
import Session exposing (Session)


type alias Model =
    { session : Session
    , plan : Result String Plan
    }


type Msg
    = Noop


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, plan = Plan.decode "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession { session } =
    session


viewContent : Model -> Html Msg
viewContent model =
    div [] []


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Run Plan"
    }
