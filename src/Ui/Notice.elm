module Ui.Notice exposing (Tone(..), view)

{-| Dismissible banner informing the user about something that happened.
-}

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)


type Tone
    = Info
    | Error


{-| Renders a banner holding `content` with a button emitting `onDismiss`.
-}
view : { tone : Tone, content : List (Html msg), onDismiss : msg } -> Html msg
view { tone, content, onDismiss } =
    div [ class ("rounded-xl px-4 py-3 ring-1 " ++ toneClasses tone) ]
        [ div [ class "flex items-start justify-between gap-4" ]
            [ p [ class "text-sm" ] content
            , button
                [ type_ "button"
                , attribute "aria-label" "Dismiss"
                , onClick onDismiss
                , class "cursor-pointer text-lg leading-none opacity-60 transition-opacity hover:opacity-100"
                ]
                [ text "×" ]
            ]
        ]


toneClasses : Tone -> String
toneClasses tone =
    case tone of
        Info ->
            "bg-pink-50 text-pink-700 ring-pink-200"

        Error ->
            "bg-red-50 text-red-700 ring-red-200"
