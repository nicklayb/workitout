module Ui.Field exposing (inputClasses, labelClasses, view)

{-| Labeled form field wrapper and the shared styles of form controls.
-}

import Html exposing (Html, label, span, text)
import Html.Attributes exposing (class)


{-| Style of the small uppercase labels.
-}
labelClasses : String
labelClasses =
    "text-xs font-medium tracking-wide text-gray-400 uppercase"


{-| Style of text-like inputs and selects. Out-of-range values of number
inputs get a red ring.
-}
inputClasses : String
inputClasses =
    "w-full rounded-lg bg-white px-3 py-2 text-gray-800 ring-1 ring-gray-200 outline-none transition-shadow placeholder:text-gray-300 focus:ring-2 focus:ring-pink-400 invalid:ring-red-300"


{-| Renders `control` under a label. Wrapping the control in the `label`
element makes clicking the label focus it.
-}
view : String -> Html msg -> Html msg
view labelText control =
    label [ class "flex flex-col gap-1" ]
        [ span [ class labelClasses ] [ text labelText ]
        , control
        ]
