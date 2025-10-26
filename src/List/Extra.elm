module List.Extra exposing (prependIf, prependIfElse)


prependIf : Bool -> a -> List a -> List a
prependIf condition value list =
    if condition then
        value :: list

    else
        list


prependIfElse : Bool -> a -> a -> List a -> List a
prependIfElse condition ifTrue ifFalse list =
    if condition then
        ifTrue :: list

    else
        ifFalse :: list
