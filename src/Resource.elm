module Resource exposing (Resource(..), init, loaded, loading, value)


type Resource a
    = NotLoaded
    | Loading (Maybe a)
    | Loaded a


init : Resource a
init =
    NotLoaded


loading : Resource a -> Resource a
loading resource =
    case resource of
        Loaded loadedValue ->
            Loading (Just loadedValue)

        _ ->
            Loading Nothing


loaded : a -> Resource a
loaded valueInput =
    Loaded valueInput


value : Resource a -> Maybe a
value resource =
    case resource of
        Loaded loadedValue ->
            Just loadedValue

        Loading loadingValue ->
            loadingValue

        _ ->
            Nothing
