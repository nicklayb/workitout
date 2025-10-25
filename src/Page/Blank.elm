module Page.Blank exposing (view)

import Html exposing (Html, main_)


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = main_ [] []
    }
