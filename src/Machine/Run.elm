module Machine.Run exposing (..)

import AST exposing (AST, Path)
import Browser exposing (UrlRequest(..))
import Dict exposing (Dict)
import Machine.StandardLibrary exposing (getStandardLibFunction)
import Set exposing (Set)
import Utils exposing (..)
import Machine.Errors exposing (..)



findAllVars : AST -> Set String
findAllVars ast =
    case ast of
        AST.Block { assignments } ->
            assignments |> List.map (\a -> a.name) |> Set.fromList

        _ ->
            Set.empty


runPath : AST -> Path -> Result RunError Int
runPath ast path =
    case path of
        [] ->
            Err (InternalPanic "could not find node (empty path)")

        rootIndex :: _ ->
            AST.getAstNodeByPath path ast
                |> Result.fromMaybe (InternalPanic "could not find node")
                |> Result.andThen
                    (runExpression
                        (gatherVariables ast rootIndex)
                        (findAllVars ast)
                    )


gatherVariables : AST -> Int -> Dict String Int
gatherVariables ast until =
    case ast of
        AST.Block { assignments } ->
            List.take until assignments
                |> List.foldl
                    (\{ name, expression } vars ->
                        case runExpression vars (findAllVars ast) expression of
                            Ok ret ->
                                Dict.insert name ret vars

                            Err _ ->
                                vars
                    )
                    (Dict.fromList [])

        _ ->
            Debug.todo "What"


runExpression : Dict String Int -> Set String -> AST -> Result RunError Int
runExpression vars allVars expression =
    case expression of
        AST.Block _ ->
            Debug.todo "cannot run bloccs"

        AST.Form f ->
            getStandardLibFunction f.head
                |> Result.fromMaybe (MissingFunc f.head)
                |> Result.andThen
                    (\stdLibFunc ->
                        List.map (runExpression vars allVars) f.tail
                            |> Utils.listAllOk
                            |> Result.andThen stdLibFunc.impl
                    )

        AST.Incomplete ->
            Err Incomplete

        AST.Number { value } ->
            Ok value

        AST.Reference r ->
            case Dict.get r.name vars of
                Just referenced ->
                    Result.Ok referenced

                Nothing ->
                    if Set.member r.name allVars then
                        Err ReferencedError

                    else
                        Err (MissingVar r.name)
