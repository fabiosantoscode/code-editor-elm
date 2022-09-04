module Machine.Parse exposing (..)

import AST exposing (AST(..))


type ParseResult
    = ParseError String
    | ParsedNumber Int
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
                    ParseError "Invalid number"


tryParseAtomAst : String -> Maybe AST
tryParseAtomAst string =
    case tryParseAtom string of
        ParsedNumber n ->
            Just (Number { value = n })

        _ ->
            Nothing


isParseError : ParseResult -> Bool
isParseError r =
    case r of
        ParseError _ ->
            True

        _ ->
            False
