module CustomPlans exposing (CustomPlan, CustomPlans, currentVersion, decodeString, empty, get, insert, remove, toList, toString, update)

{-| Plans created by the user, persisted in the local storage.

They are stored as versioned JSON, each plan being kept in the YAML format of
the hosted plans. To introduce a new version, bump `currentVersion` and make
`versionDecoder` migrate the older payloads, like `Settings.Persisted` does.

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import PlanDraft exposing (PlanDraft)
import PlanDraft.Yaml


{-| A plan along with the identifier it is stored under.
-}
type alias CustomPlan =
    { id : Int
    , draft : PlanDraft
    }


{-| Opaque so identifiers stay unique.
-}
type CustomPlans
    = CustomPlans (List CustomPlan)


{-| Version written by `toString`.
-}
currentVersion : Int
currentVersion =
    1


firstId : Int
firstId =
    1


empty : CustomPlans
empty =
    CustomPlans []


{-| Plans in their creation order.
-}
toList : CustomPlans -> List CustomPlan
toList (CustomPlans plans) =
    plans


get : Int -> CustomPlans -> Maybe PlanDraft
get id (CustomPlans plans) =
    plans
        |> List.filter (\plan -> plan.id == id)
        |> List.head
        |> Maybe.map .draft


{-| Appends a plan under a new identifier, which is returned along.
-}
insert : PlanDraft -> CustomPlans -> ( Int, CustomPlans )
insert draft (CustomPlans plans) =
    let
        id =
            plans
                |> List.map .id
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault firstId
    in
    ( id, CustomPlans (plans ++ [ { id = id, draft = draft } ]) )


{-| Changes the plan stored under `id`, if any.
-}
update : Int -> (PlanDraft -> PlanDraft) -> CustomPlans -> CustomPlans
update id change (CustomPlans plans) =
    CustomPlans
        (List.map
            (\plan ->
                if plan.id == id then
                    { plan | draft = change plan.draft }

                else
                    plan
            )
            plans
        )


remove : Int -> CustomPlans -> CustomPlans
remove id (CustomPlans plans) =
    CustomPlans (List.filter (\plan -> plan.id /= id) plans)


{-| Encodes the plans at the current version.
-}
toString : CustomPlans -> String
toString (CustomPlans plans) =
    Encode.encode 0
        (Encode.object
            [ ( "version", Encode.int currentVersion )
            , ( "plans", Encode.list encodePlan plans )
            ]
        )


encodePlan : CustomPlan -> Encode.Value
encodePlan plan =
    Encode.object
        [ ( "id", Encode.int plan.id )
        , ( "yaml", Encode.string (PlanDraft.Yaml.encode plan.draft) )
        ]


{-| Decodes plans written by `toString`. Fails as a whole when any plan is
invalid, the version is unknown or identifiers are duplicated.
-}
decodeString : String -> Result String CustomPlans
decodeString json =
    Decode.decodeString decoder json
        |> Result.mapError Decode.errorToString


decoder : Decoder CustomPlans
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen versionDecoder
        |> Decode.andThen ensureUniqueIds


versionDecoder : Int -> Decoder (List CustomPlan)
versionDecoder version =
    case version of
        1 ->
            Decode.field "plans" (Decode.list version1PlanDecoder)

        _ ->
            Decode.fail ("Unsupported plans version " ++ String.fromInt version)


version1PlanDecoder : Decoder CustomPlan
version1PlanDecoder =
    Decode.map2 CustomPlan
        (Decode.field "id" Decode.int)
        (Decode.field "yaml" Decode.string |> Decode.andThen yamlDecoder)


yamlDecoder : String -> Decoder PlanDraft
yamlDecoder yaml =
    case PlanDraft.Yaml.decode yaml of
        Ok draft ->
            Decode.succeed draft

        Err error ->
            Decode.fail error


ensureUniqueIds : List CustomPlan -> Decoder CustomPlans
ensureUniqueIds plans =
    let
        ids =
            List.map .id plans
    in
    if List.length ids == List.length (uniqueValues ids) then
        Decode.succeed (CustomPlans plans)

    else
        Decode.fail "Duplicated plan identifiers"


uniqueValues : List Int -> List Int
uniqueValues values =
    List.foldl
        (\value kept ->
            if List.member value kept then
                kept

            else
                value :: kept
        )
        []
        values
