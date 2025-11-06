module Tree exposing (Folder, decoder)

import Dict exposing (Dict)
import Json.Decode exposing (..)


type Folder a
    = Folder
        { files : Dict String a
        , folders : Dict String (Folder a)
        }


decoder : Decoder a -> Decoder (Folder a)
decoder innerDecoder =
    lazy <|
        \_ ->
            map2
                (\files folders -> Folder { files = files, folders = folders })
                (field "files" (dict innerDecoder))
                (field "folders" (dict (decoder innerDecoder)))
