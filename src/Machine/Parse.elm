module Machine.Parse exposing (..)

import AST exposing (AST(..))
import Set  exposing (Set)


type ParseResult
    = ParsedNumber Int
    | ParsedVariable String
    | Empty


tryParseAtom : String -> ParseResult
tryParseAtom searchString =
    case searchString of
        "" ->
            Empty

        s ->
            case String.toInt s of
                Just n ->
                    ParsedNumber n

                Nothing ->
                    ParsedVariable s


tryParseAtomAst : String -> Set String -> Maybe AST
tryParseAtomAst string vars =
    case tryParseAtom string of
        ParsedNumber n ->
            Just (Number { value = n })

        ParsedVariable v ->
            if Set.member v vars then
                Just (Reference { name = v })

            else
                Nothing

        _ ->
            Nothing


tryParseAtomToString : String -> Maybe String
tryParseAtomToString string =
    case tryParseAtom string of
        ParsedNumber n ->
            Just (String.fromInt n)

        ParsedVariable n ->
            Just n

        _ ->
            Nothing
