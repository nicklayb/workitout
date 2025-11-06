module RemotePlan exposing (RemotePlan, decoder)

import Json.Decode as JsonDecode exposing (Decoder)


type alias RemotePlan =
    { description : String
    , author_name : String
    , download_url : String
    , sha : String
    , path : String
    , name : String
    }


decoder : Decoder RemotePlan
decoder =
    JsonDecode.map6 RemotePlan
        (JsonDecode.field "description" JsonDecode.string)
        (JsonDecode.field "author_name" JsonDecode.string)
        (JsonDecode.field "download_url" JsonDecode.string)
        (JsonDecode.field "sha" JsonDecode.string)
        (JsonDecode.field "path" JsonDecode.string)
        (JsonDecode.field "name" JsonDecode.string)
