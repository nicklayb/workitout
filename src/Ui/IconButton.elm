module Ui.IconButton exposing (classes, view)

{-| Small round button holding an icon or a symbol, labeled for screen readers.
-}

import Html exposing (Html, button)
import Html.Attributes exposing (attribute, class, disabled, title, type_)
import Html.Events exposing (onClick)


{-| Style of the button, also usable on links.
-}
classes : String
classes =
    "flex size-8 shrink-0 cursor-pointer items-center justify-center rounded-full text-gray-400 transition-colors hover:bg-pink-50 hover:text-pink-600 disabled:cursor-default disabled:opacity-30 disabled:hover:bg-transparent disabled:hover:text-gray-400"


{-| Renders the button; a disabled one emits nothing.
-}
view : { label : String, isDisabled : Bool, onPress : msg } -> Html msg -> Html msg
view config icon =
    button
        [ type_ "button"
        , attribute "aria-label" config.label
        , title config.label
        , disabled config.isDisabled
        , onClick config.onPress
        , class classes
        ]
        [ icon ]
