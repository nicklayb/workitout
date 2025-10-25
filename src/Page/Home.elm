module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Counter
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Session exposing (Session)


type alias Model =
    { session : Session
    , counter : Int
    }


type Msg
    = Increment
    | Decrement


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, counter = 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = Counter.increment model.counter }, Cmd.none )

        Decrement ->
            ( { model | counter = Counter.decrement model.counter }, Cmd.none )


toSession : Model -> Session
toSession { session } =
    session


viewContent : Model -> Html Msg
viewContent model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , span [] [ text (String.fromInt model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Home"
    }
