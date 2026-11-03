module Page.Blank exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = div [ class "bg-gray-50" ] []
    }
