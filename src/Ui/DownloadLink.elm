module Ui.DownloadLink exposing (view)

{-| Link downloading a text file built in memory, without any round trip
through `update`.
-}

import Html exposing (Attribute, Html, a)
import Html.Attributes exposing (download, href)
import Url


{-| Renders a link downloading `content` as `fileName` when clicked.
-}
view : { fileName : String, mimeType : String, content : String } -> List (Attribute msg) -> List (Html msg) -> Html msg
view { fileName, mimeType, content } attributes children =
    a
        (href ("data:" ++ mimeType ++ ";charset=utf-8," ++ Url.percentEncode content)
            :: download fileName
            :: attributes
        )
        children
