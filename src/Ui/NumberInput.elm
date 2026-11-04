module Ui.NumberInput exposing (view)

{-| Number input editing a `BoundedInt`.

Typed values are sent as is and get capped by the model. When the capped
value equals the previous one the virtual DOM has nothing to patch, so the
field keeps showing the typed value: the `min`/`max` attributes make it
`:invalid`, which `Ui.Field.inputClasses` highlights.

-}

import BoundedInt exposing (BoundedInt)
import Html exposing (Html, input)
import Html.Attributes exposing (attribute, class, type_, value)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Ui.Field as Field


{-| Renders the input, `extraClasses` sizing it.
-}
view : { label : String, value : BoundedInt, onChange : Int -> msg, extraClasses : String } -> Html msg
view config =
    let
        { minimum, maximum } =
            BoundedInt.bounds config.value
    in
    input
        [ type_ "number"
        , attribute "aria-label" config.label
        , attribute "inputmode" "numeric"
        , Html.Attributes.min (String.fromInt minimum)
        , Html.Attributes.max (String.fromInt maximum)
        , value (String.fromInt (BoundedInt.value config.value))
        , on "input" (Decode.map config.onChange integerValueDecoder)
        , class (Field.inputClasses ++ " tabular-nums " ++ config.extraClasses)
        ]
        []


{-| Fails, hence emits no message, while the field doesn't hold an integer
(e.g. while it is being cleared to type another value).
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
