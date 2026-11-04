module Ui.Button exposing (primaryClasses, secondaryClasses)

{-| Shared button styles, usable on buttons, links and labels alike.
-}


{-| Filled pink button, for the main action of a view.
-}
primaryClasses : String
primaryClasses =
    "inline-flex cursor-pointer items-center justify-center rounded-full bg-pink-500 font-semibold text-white shadow-sm transition-colors hover:bg-pink-600"


{-| Outlined button, for secondary actions.
-}
secondaryClasses : String
secondaryClasses =
    "inline-flex cursor-pointer items-center justify-center rounded-full bg-white font-semibold text-pink-600 ring-1 ring-pink-200 transition-colors hover:bg-pink-50"
