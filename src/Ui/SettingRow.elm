module Ui.SettingRow exposing (view)

{-| Line of a settings list: a title and description on the left, the control
editing the setting on the right.
-}

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)


{-| Renders a setting with its `control`.
-}
view : { title : String, description : String } -> Html msg -> Html msg
view { title, description } control =
    div [ class "flex items-center justify-between gap-6 px-6 py-4" ]
        [ div [ class "min-w-0" ]
            [ p [ class "font-medium text-gray-800" ] [ text title ]
            , p [ class "text-sm text-gray-500" ] [ text description ]
            ]
        , control
        ]
