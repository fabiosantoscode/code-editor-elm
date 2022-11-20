module Utils exposing (..)

import Set exposing (Set)


flatMap : (Int -> a -> List b) -> List a -> List b
flatMap f xs =
    xs |> List.indexedMap f |> List.foldr (++) []


setFlatMap : (a -> Set comparable) -> List a -> Set comparable
setFlatMap fn inp =
    List.map fn inp |> List.foldr Set.union Set.empty


list2ToTuple : List a -> Maybe ( a, a )
list2ToTuple list =
    case list of
        [ first, second ] ->
            Just ( first, second )

        _ ->
            Nothing


maybe2List : Maybe a -> List a
maybe2List val =
    case val of
        Just x ->
            [ x ]

        Nothing ->
            []


bool2Int : Bool -> Int
bool2Int b =
    if b then
        1

    else
        0


listAt : Int -> List a -> Maybe a
listAt index list =
    case list of
        [] ->
            Nothing

        item :: rest ->
            if index == 0 then
                Just item

            else
                listAt (index - 1) rest


listAllJust : List (Maybe a) -> Maybe (List a)
listAllJust xs =
    case xs of
        [] ->
            Just []

        (Just item) :: rest ->
            listAllJust rest |> Maybe.map (\goodRest -> item :: goodRest)

        Nothing :: _ ->
            Nothing


listAllOk : List (Result listError listItem) -> Result listError (List listItem)
listAllOk xs =
    case xs of
        [] ->
            Result.Ok []

        (Result.Ok item) :: rest ->
            listAllOk rest |> Result.map (\goodRest -> item :: goodRest)

        (Result.Err x) :: _ ->
            Result.Err x
