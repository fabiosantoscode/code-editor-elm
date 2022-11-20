module Machine.Run exposing (..)

import AST exposing (AST)
import Browser exposing (UrlRequest(..))
import Dict exposing (Dict)
import Machine.Errors exposing (..)
import Machine.StandardLibrary exposing (getStandardLibFunction)
import Set exposing (Set)
import Utils exposing (..)


findAllVarNames : AST -> Set String
findAllVarNames ast =
    case ast of
        AST.Block { assignments } ->
            assignments |> List.map (\a -> a.name) |> Set.fromList

        _ ->
            Set.empty


runPath : AST -> Path -> MachineResult
runPath ast path =
    case path of
        [] ->
            Err (NoResult path)

        rootIndex :: _ ->
            AST.getAstNodeByPath path ast
                |> Result.fromMaybe (InternalPanic path "could not find node")
                |> Result.andThen
                    (runExpression
                        (gatherVariables ast rootIndex)
                        (findAllVarNames ast)
                        path
                    )


gatherVariables : AST -> Int -> Dict String Int
gatherVariables ast until =
    let
        evaluateIth i assignments vars =
            case assignments of
                [] ->
                    vars

                { name, expression } :: rest ->
                    runExpression vars (findAllVarNames ast) [ i ] expression
                        |> Result.map (\ret -> Dict.insert name ret vars)
                        |> Result.withDefault vars
                        |> evaluateIth (i + 1) rest
    in
    case ast of
        AST.Block { assignments } ->
            evaluateIth 0 (List.take until assignments) (Dict.fromList [])

        _ ->
            Debug.todo "What"


runExpression : Dict String Int -> Set String -> List Int -> AST -> MachineResult
runExpression vars allVars path expression =
    case expression of
        AST.Block _ ->
            Debug.todo "cannot run bloccs"

        AST.Form f ->
            getStandardLibFunction f.head
                |> Result.fromMaybe (MissingFunc path f.head)
                |> Result.andThen
                    (\stdLibFunc ->
                        List.indexedMap (\i exp -> runExpression vars allVars (path ++ [ i ]) exp) f.tail
                            |> Utils.listAllOk
                            |> Result.andThen (stdLibFunc.impl path)
                    )

        AST.Incomplete ->
            Err (NoResult path)

        AST.Number { value } ->
            Ok value

        AST.Reference r ->
            case Dict.get r.name vars of
                Just referenced ->
                    Result.Ok referenced

                Nothing ->
                    if Set.member r.name allVars then
                        Err (NoResult path)

                    else
                        Err (MissingVar path r.name)
