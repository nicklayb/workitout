module Page exposing (Page(..), view, viewLoading)

import Browser exposing (Document)
import Html exposing (Html, div, text)
import Session exposing (Session)


type Page
    = Other
    | Home
    | RunPlan


view : Session -> Page -> { title : String, content : Html msg } -> Document msg
view _ page { title, content } =
    { title = title ++ " :: WorkItOut"
    , body =
        [ content
        , viewFooter
        ]
    }


viewLoading : Html msg
viewLoading =
    div [] [ text "Loading" ]


viewFooter : Html msg
viewFooter =
    div [] []
