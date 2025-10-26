module Ziplist exposing (Position(..), Ziplist(..), current, init, isReset, map, next, positionEquals)


type Ziplist a
    = Empty
    | Ziplist (List a) a (List a)


type Position
    = Before
    | Current
    | After


init : List a -> Ziplist a
init list =
    case list of
        [] ->
            Empty

        head :: tail ->
            Ziplist [] head tail


current : Ziplist a -> Maybe a
current ziplist =
    case ziplist of
        Ziplist _ currentItem _ ->
            Just currentItem

        _ ->
            Nothing


isReset : Ziplist a -> Bool
isReset ziplist =
    case ziplist of
        Ziplist [] _ _ ->
            True

        _ ->
            False


next : Ziplist a -> Ziplist a
next ziplist =
    case ziplist of
        Ziplist back currentItem [] ->
            init (List.reverse (currentItem :: back))

        Ziplist back currentItem (newCurrent :: tail) ->
            Ziplist (currentItem :: back) newCurrent tail

        Empty ->
            Empty


map : (Position -> a -> b) -> Ziplist a -> List b
map function ziplist =
    let
        folder position value accumulator =
            function position value :: accumulator
    in
    List.reverse (foldl folder [] ziplist)


foldl : (Position -> a -> b -> b) -> b -> Ziplist a -> b
foldl function accumulator ziplist =
    case ziplist of
        Ziplist back currentItem front ->
            let
                backAccumulated =
                    List.foldl (function Before) accumulator (List.reverse back)

                currentAccumulated =
                    function Current currentItem backAccumulated

                frontAccumulated =
                    List.foldl (function After) currentAccumulated front
            in
            frontAccumulated

        Empty ->
            accumulator


positionEquals : Position -> Position -> Bool
positionEquals left right =
    case ( left, right ) of
        ( Before, Before ) ->
            True

        ( Current, Current ) ->
            True

        ( After, After ) ->
            True

        _ ->
            False
