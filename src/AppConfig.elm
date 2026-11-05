module AppConfig exposing (AppConfig)

{-| Flags given by `src/index.js` when the application starts. `version` comes
from the `VERSION` file, injected at build time.
-}


type alias AppConfig =
    { lastPlan : Maybe String
    , weekday : Int
    , settings : Maybe String
    , customPlans : Maybe String
    , version : String
    }
