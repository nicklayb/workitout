module Page.Errors.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = viewContent
    }


viewContent : Html msg
viewContent =
    div [] [ text "Not found" ]
