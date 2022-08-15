module Utils exposing (..)


flatMap : (Int -> a -> List b) -> List a -> List b
flatMap f xs =
    xs |> List.indexedMap f |> List.foldr (++) []

