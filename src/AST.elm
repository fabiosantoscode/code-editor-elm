module AST exposing (..)

import Html exposing (a)
import Set exposing (Set)
import Utils exposing (flatMap)


type alias Assign =
    { name : String, expression : AST }


type AST
    = Block { assignments : List Assign }
    | Form { head : String, tail : List AST }
    | Number { value : Int }
    | Reference { name : String }
    | Incomplete


type ASTTransformation
    = Insert AST
    | ReplaceWith AST


type alias IterationContext =
    { path : Path
    , replacing : Replacement
    }


type alias Path =
    List Int


type alias Replacement =
    Maybe { path : Path, search : String, addition : Bool }


getTransAddedNode : ASTTransformation -> AST
getTransAddedNode trans =
    case trans of
        Insert x ->
            x

        ReplaceWith x ->
            x


getAstChildren : AST -> List AST
getAstChildren ast =
    case ast of
        Block p ->
            p.assignments |> List.map (\a -> a.expression)

        Form f ->
            f.tail

        _ ->
            []


mutAstChildren : (Int -> AST -> ASTTransformation) -> AST -> AST
mutAstChildren mutator ast =
    case ast of
        Block p ->
            let
                assignmentReplacer =
                    \index a ->
                        case mutator index a.expression of
                            ReplaceWith newChild ->
                                [ { a | expression = newChild } ]

                            Insert newChild ->
                                [ { name = "", expression = newChild }, a ]
            in
            Block
                { p | assignments = flatMap assignmentReplacer p.assignments }

        Form f ->
            let
                formReplacer =
                    \index a ->
                        case mutator index a of
                            ReplaceWith newChild ->
                                [ newChild ]

                            Insert newChild ->
                                [ newChild, a ]
            in
            Form
                { f | tail = flatMap formReplacer f.tail }

        _ ->
            ast


addAstChild : AST -> AST -> AST
addAstChild ast newChild =
    case ast of
        Block p ->
            Block { p | assignments = p.assignments ++ [ { name = "", expression = newChild } ] }

        Form f ->
            Form { f | tail = f.tail ++ [ newChild ] }

        _ ->
            ast


mutateNthChild : Int -> ASTTransformation -> AST -> ASTTransformation
mutateNthChild index trans ast =
    let
        currentChildren =
            getAstChildren ast

        indexedMutator =
            \hereIndex child ->
                if hereIndex == index then
                    trans

                else
                    ReplaceWith child
    in
    if index == List.length currentChildren then
        ReplaceWith (addAstChild ast (getTransAddedNode trans))

    else
        ReplaceWith (mutAstChildren indexedMutator ast)


mutateTargetChild1 : List Int -> ASTTransformation -> AST -> ASTTransformation
mutateTargetChild1 path transformer ast =
    case path of
        [] ->
            ReplaceWith (getTransAddedNode transformer)

        [ hereIndex ] ->
            mutateNthChild hereIndex transformer ast

        hereIndex :: rest ->
            ReplaceWith
                (mutAstChildren
                    (\index child ->
                        if index == hereIndex then
                            mutateTargetChild1 rest transformer child

                        else
                            ReplaceWith child
                    )
                    ast
                )


mutateTargetChild : List Int -> ASTTransformation -> AST -> AST
mutateTargetChild path transformer ast =
    case mutateTargetChild1 path transformer ast of
        ReplaceWith c ->
            c

        Insert _ ->
            Number { value = -500 }



-- mutateTargetChild never returns this one.


generateVarName : Set String -> Int -> String
generateVarName existingNames index =
    let
        newName =
            "number_" ++ String.fromInt index
    in
    if Set.member newName existingNames then
        generateVarName existingNames (index + 1)

    else
        newName



isVariableVisibleFrom : List String -> Path -> String -> Bool
isVariableVisibleFrom varNames path varName =
    let
        varNamesUpToHere =
            List.take (List.head path |> Maybe.withDefault -1) varNames
    in
    List.any ((==) varName) varNamesUpToHere


setFlatMap : (a -> Set comparable) -> List a -> Set comparable
setFlatMap fn inp =
    List.map fn inp |> List.foldr Set.union Set.empty


declaredVariableNames : AST -> Set String
declaredVariableNames ast =
    case ast of
        Block p ->
            Set.union
                (setFlatMap (\a -> Set.fromList [ a.name ] |> Set.remove "") p.assignments)
                (setFlatMap (\a -> a.expression |> declaredVariableNames) p.assignments)

        Form f ->
            f.tail |> setFlatMap declaredVariableNames

        _ ->
            Set.empty



-- TODO does not work recursively


ensureOneVariableName : Set String -> AST -> Maybe ( Path, ASTTransformation )
ensureOneVariableName availableNames ast =
    --- generateVarName availableNames 0
    case ast of
        Block p ->
            p.assignments
                |> flatMap
                    (\index { name } ->
                        if name == "" then
                            [ index ]

                        else
                            []
                    )
                |> List.head
                |> Maybe.map
                    (\index ->
                        let
                            newStats =
                                p.assignments
                                    |> List.indexedMap
                                        (\innerIndex a ->
                                            if index == innerIndex then
                                                { a | name = generateVarName availableNames 0 }

                                            else
                                                a
                                        )
                        in
                        ( [], ReplaceWith (Block { p | assignments = newStats }) )
                    )

        _ ->
            Nothing


ensureVariableNames : AST -> AST
ensureVariableNames ast =
    case ensureOneVariableName (declaredVariableNames ast) ast of
        Nothing ->
            ast

        Just ( path, transformer ) ->
            ensureVariableNames (mutateTargetChild path transformer ast)


commitASTTransformation : Path -> ASTTransformation -> AST -> AST
commitASTTransformation path transformation ast =
    mutateTargetChild path
        transformation
        ast
        |> ensureVariableNames
