module GitHub exposing (HttpResponse, fetchContent)

import Base64
import Http
import Json.Decode as JsonDecode exposing (Decoder)


type alias HttpResponse a =
    Result Http.Error a


contentDecoder : Decoder String
contentDecoder =
    let
        decodeBase64 string =
            case Base64.decode string of
                Ok decoded ->
                    JsonDecode.succeed decoded

                _ ->
                    JsonDecode.fail "Invalid base64"
    in
    JsonDecode.field "content" JsonDecode.string
        |> JsonDecode.map (String.replace "\n" "")
        |> JsonDecode.andThen decodeBase64


fetchContent : (HttpResponse String -> msg) -> String -> Cmd msg
fetchContent msg filePath =
    Http.get
        { url = "https://api.github.com/repos/nicklayb/workitout/contents/plans/" ++ filePath ++ "?ref=plans"
        , expect = Http.expectJson msg contentDecoder
        }
