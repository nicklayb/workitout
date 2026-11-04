module Ui.Toggle exposing (view)

{-| On/off switch.
-}

import Html exposing (Html, button, span)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)


{-| Renders a switch; `onToggle` receives the state the user switched to.
-}
view : { label : String, isOn : Bool, onToggle : Bool -> msg } -> Html msg
view { label, isOn, onToggle } =
    let
        ( trackClasses, knobClasses ) =
            if isOn then
                ( "bg-pink-500", "translate-x-5" )

            else
                ( "bg-gray-200", "translate-x-0" )
    in
    button
        [ type_ "button"
        , attribute "role" "switch"
        , attribute "aria-checked" (boolToAttribute isOn)
        , attribute "aria-label" label
        , onClick (onToggle (not isOn))
        , class ("relative inline-flex h-6 w-11 shrink-0 cursor-pointer rounded-full p-0.5 transition-colors " ++ trackClasses)
        ]
        [ span [ class ("size-5 rounded-full bg-white shadow-sm transition-transform " ++ knobClasses) ] [] ]


boolToAttribute : Bool -> String
boolToAttribute bool =
    if bool then
        "true"

    else
        "false"
