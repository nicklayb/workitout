module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (Html, div)
import Session exposing (Session)


type Page
    = Other
    | Home


view : Session -> Page -> { title : String, content : Html msg } -> Document msg
view _ page { title, content } =
    { title = title ++ " :: Meep"
    , body =
        [ viewHeader page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html msg
viewHeader _ =
    div [] []


viewFooter : Html msg
viewFooter =
    div [] []
