module Ui.TextInput exposing (view)

{-| Single line text input.
-}

import Html exposing (Html, input)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput)
import Ui.Field as Field


{-| Renders a text input of the given HTML `inputType` (`text`, `email`…).
-}
view : { label : String, inputType : String, placeholder : String, value : String, onInput : String -> msg } -> Html msg
view config =
    input
        [ type_ config.inputType
        , attribute "aria-label" config.label
        , placeholder config.placeholder
        , value config.value
        , onInput config.onInput
        , class Field.inputClasses
        ]
        []
