module Ui.BoundedIntInput exposing (view)

{-| Slider picking an integer within the bounds of a `BoundedInt`.

A slider is used rather than a number field because the browser keeps it
within its bounds, so the displayed value never diverges from the capped one.

-}

import BoundedInt exposing (BoundedInt)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)


{-| Renders the slider along with its value followed by `unit`.
-}
view : { label : String, unit : String, value : BoundedInt, onChange : Int -> msg } -> Html msg
view { label, unit, value, onChange } =
    let
        { minimum, maximum } =
            BoundedInt.bounds value

        currentValue =
            String.fromInt (BoundedInt.value value)
    in
    div [ class "flex w-56 items-center gap-3" ]
        [ input
            [ type_ "range"
            , attribute "aria-label" label
            , Html.Attributes.min (String.fromInt minimum)
            , Html.Attributes.max (String.fromInt maximum)
            , Html.Attributes.value currentValue
            , on "input" (Decode.map onChange integerValueDecoder)
            , class "flex-1 cursor-pointer accent-pink-500"
            ]
            []
        , span [ class "w-12 text-right text-sm font-medium text-gray-700 tabular-nums" ] [ text (currentValue ++ " " ++ unit) ]
        ]


{-| Fails, hence emits no message, when the field does not hold an integer.
-}
integerValueDecoder : Decoder Int
integerValueDecoder =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.andThen
            (\string ->
                case String.toInt string of
                    Just integer ->
                        Decode.succeed integer

                    Nothing ->
                        Decode.fail ("Not an integer: " ++ string)
            )
