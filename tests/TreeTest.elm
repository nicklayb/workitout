module TreeTest exposing (suite)

{-| Unit tests for the `Tree` module: JSON decoding and folder helpers.
-}

import Dict
import Expect
import Fixtures
import Json.Decode as JsonDecode
import Test exposing (Test, describe, test)
import Tree exposing (Folder(..))


nestedFileCount : Int
nestedFileCount =
    4


decodeTree : String -> Result JsonDecode.Error (Folder String)
decodeTree =
    JsonDecode.decodeString (Tree.decoder JsonDecode.string)


emptyFolder : Folder String
emptyFolder =
    Folder { files = Dict.empty, folders = Dict.empty }


suite : Test
suite =
    describe "Tree"
        [ describe "decoder"
            [ test "decodes an empty folder" <|
                \_ -> decodeTree Fixtures.treeEmptyJson |> Expect.equal (Ok emptyFolder)
            , test "decodes nested folders and files" <|
                \_ ->
                    decodeTree Fixtures.treeNestedJson
                        |> Expect.equal
                            (Ok
                                (Folder
                                    { files = Dict.fromList [ ( "duff.yml", "Homer Simpson" ) ]
                                    , folders =
                                        Dict.fromList
                                            [ ( "springfield"
                                              , Folder
                                                    { files = Dict.fromList [ ( "sax.yml", "Lisa Simpson" ), ( "skate.yml", "Bart Simpson" ) ]
                                                    , folders =
                                                        Dict.fromList
                                                            [ ( "kwik-e-mart"
                                                              , Folder
                                                                    { files = Dict.fromList [ ( "squishee.yml", "Apu Nahasapeemapetilon" ) ]
                                                                    , folders = Dict.empty
                                                                    }
                                                              )
                                                            ]
                                                    }
                                              )
                                            ]
                                    }
                                )
                            )
            , test "fails when folders are missing" <|
                \_ -> decodeTree """{ "files": {} }""" |> Expect.err
            ]
        , describe "fileCount"
            [ test "counts files in nested folders" <|
                \_ -> decodeTree Fixtures.treeNestedJson |> Result.map Tree.fileCount |> Expect.equal (Ok nestedFileCount)
            , test "an empty folder has no file" <|
                \_ -> Tree.fileCount emptyFolder |> Expect.equal 0
            ]
        , describe "isEmpty"
            [ test "is true for a folder without files nor folders" <|
                \_ -> Tree.isEmpty emptyFolder |> Expect.equal True
            , test "is false for a folder with files" <|
                \_ -> decodeTree Fixtures.treeNestedJson |> Result.map Tree.isEmpty |> Expect.equal (Ok False)
            , test "is false for a folder holding only an empty sub folder" <|
                \_ ->
                    Folder { files = Dict.empty, folders = Dict.fromList [ ( "springfield", emptyFolder ) ] }
                        |> Tree.isEmpty
                        |> Expect.equal False
            ]
        ]
