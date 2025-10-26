module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (Html, div)
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


viewFooter : Html msg
viewFooter =
    div [] []
