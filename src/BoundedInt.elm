module BoundedInt exposing (BoundedInt, Bounds, bounds, init, set, value)

{-| An integer that always stays within inclusive bounds: any value outside of
them is capped to the closest bound.
-}


{-| Inclusive range an integer is allowed to take.
-}
type alias Bounds =
    { minimum : Int
    , maximum : Int
    }


{-| Opaque so the value can only be built capped.
-}
type BoundedInt
    = BoundedInt Bounds Int


{-| Builds a bounded integer, capping the value within the bounds.
-}
init : Bounds -> Int -> BoundedInt
init boundsInput input =
    BoundedInt boundsInput (clamp boundsInput.minimum boundsInput.maximum input)


{-| Replaces the value, capping it within the existing bounds.
-}
set : Int -> BoundedInt -> BoundedInt
set input (BoundedInt boundsInput _) =
    init boundsInput input


{-| The capped value.
-}
value : BoundedInt -> Int
value (BoundedInt _ currentValue) =
    currentValue


{-| The bounds the value is capped within.
-}
bounds : BoundedInt -> Bounds
bounds (BoundedInt boundsInput _) =
    boundsInput
