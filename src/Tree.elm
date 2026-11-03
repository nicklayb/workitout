module Tree exposing (Folder(..), decoder, fileCount, isEmpty)

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


{-| Counts every file contained in a folder, including nested folders.
-}
fileCount : Folder a -> Int
fileCount (Folder { files, folders }) =
    Dict.foldl (\_ folder count -> count + fileCount folder) (Dict.size files) folders


{-| Tells whether a folder has neither files nor sub folders.
-}
isEmpty : Folder a -> Bool
isEmpty (Folder { files, folders }) =
    Dict.isEmpty files && Dict.isEmpty folders
