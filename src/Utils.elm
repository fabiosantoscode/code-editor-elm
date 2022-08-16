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
