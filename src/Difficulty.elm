module Difficulty exposing (Difficulty(..), badgeClasses, default, dotClasses, fromString, jsonDecoder, label, toString, yamlDecoder)

{-| Difficulty level of a plan, from `unset` (not specified) to `extreme`.
-}

import Json.Decode as JsonDecode
import Yaml.Decode as YamlDecode


{-| Difficulty of a plan. `Unset` is used when the plan doesn't specify one or specifies an unknown value.
-}
type Difficulty
    = Unset
    | Easy
    | Medium
    | Hard
    | Extreme


{-| Difficulty used when the field is absent or unknown.
-}
default : Difficulty
default =
    Unset


{-| Parses a difficulty from its serialized name, falling back to `Unset` for unknown values.
-}
fromString : String -> Difficulty
fromString string =
    case string of
        "easy" ->
            Easy

        "medium" ->
            Medium

        "hard" ->
            Hard

        "extreme" ->
            Extreme

        _ ->
            Unset


{-| Serializes a difficulty to its name as written in plan files and the index.
-}
toString : Difficulty -> String
toString difficulty =
    case difficulty of
        Unset ->
            "unset"

        Easy ->
            "easy"

        Medium ->
            "medium"

        Hard ->
            "hard"

        Extreme ->
            "extreme"


{-| Human readable label of a difficulty.
-}
label : Difficulty -> String
label difficulty =
    case difficulty of
        Unset ->
            "Unrated"

        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"

        Extreme ->
            "Extreme"


{-| Decodes a JSON string into a difficulty, unknown values becoming `Unset`.
-}
jsonDecoder : JsonDecode.Decoder Difficulty
jsonDecoder =
    JsonDecode.map fromString JsonDecode.string


{-| Decodes a YAML string into a difficulty, unknown values becoming `Unset`.
-}
yamlDecoder : YamlDecode.Decoder Difficulty
yamlDecoder =
    YamlDecode.map fromString YamlDecode.string


{-| Tailwind classes for a small colored dot with a soft ring representing the difficulty.
-}
dotClasses : Difficulty -> String
dotClasses difficulty =
    case difficulty of
        Unset ->
            "bg-gray-300 ring-gray-100"

        Easy ->
            "bg-green-500 ring-green-100"

        Medium ->
            "bg-yellow-400 ring-yellow-100"

        Hard ->
            "bg-orange-500 ring-orange-100"

        Extreme ->
            "bg-red-500 ring-red-100"


{-| Tailwind classes for a pill shaped badge displaying the difficulty label.
-}
badgeClasses : Difficulty -> String
badgeClasses difficulty =
    case difficulty of
        Unset ->
            "bg-gray-50 text-gray-500 ring-gray-200"

        Easy ->
            "bg-green-50 text-green-700 ring-green-200"

        Medium ->
            "bg-yellow-50 text-yellow-700 ring-yellow-200"

        Hard ->
            "bg-orange-50 text-orange-700 ring-orange-200"

        Extreme ->
            "bg-red-50 text-red-700 ring-red-200"
