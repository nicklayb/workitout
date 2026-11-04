module Ui.Select exposing (view)

{-| Dropdown picking one value among `options`.
-}

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (attribute, class, selected, value)
import Html.Events exposing (on)
import Json.Decode as Decode
import Ui.Field as Field


{-| Renders the dropdown. `toKey` must give a distinct key to each option, it
is how the picked option is found back.
-}
view : { label : String, options : List a, selected : a, toKey : a -> String, toLabel : a -> String, onSelect : a -> msg } -> Html msg
view config =
    let
        optionByKey key =
            config.options
                |> List.filter (\candidate -> config.toKey candidate == key)
                |> List.head

        selectedDecoder =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.andThen
                    (\key ->
                        case optionByKey key of
                            Just picked ->
                                Decode.succeed (config.onSelect picked)

                            Nothing ->
                                Decode.fail ("Unknown option " ++ key)
                    )

        viewOption candidate =
            option
                [ value (config.toKey candidate)
                , selected (config.toKey candidate == config.toKey config.selected)
                ]
                [ text (config.toLabel candidate) ]
    in
    select
        [ attribute "aria-label" config.label
        , on "change" selectedDecoder
        , class (Field.inputClasses ++ " cursor-pointer")
        ]
        (List.map viewOption config.options)
