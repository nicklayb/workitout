module RemotePlan exposing (RemotePlan, decoder)

{-| Plan metadata as listed in the remote index.json.
-}

import Difficulty exposing (Difficulty)
import Json.Decode as JsonDecode exposing (Decoder)


{-| Metadata of a plan published on the `plans` branch.
-}
type alias RemotePlan =
    { description : String
    , author_name : String
    , download_url : String
    , sha : String
    , path : String
    , name : String
    , difficulty : Difficulty
    }


{-| Decodes the difficulty, defaulting to `Unset` when the field is missing (older indexes don't have it).
-}
difficultyDecoder : Decoder Difficulty
difficultyDecoder =
    JsonDecode.maybe (JsonDecode.field "difficulty" Difficulty.jsonDecoder)
        |> JsonDecode.map (Maybe.withDefault Difficulty.default)


{-| Decodes a plan entry of the remote index.
-}
decoder : Decoder RemotePlan
decoder =
    JsonDecode.map7 RemotePlan
        (JsonDecode.field "description" JsonDecode.string)
        (JsonDecode.field "author_name" JsonDecode.string)
        (JsonDecode.field "download_url" JsonDecode.string)
        (JsonDecode.field "sha" JsonDecode.string)
        (JsonDecode.field "path" JsonDecode.string)
        (JsonDecode.field "name" JsonDecode.string)
        difficultyDecoder
