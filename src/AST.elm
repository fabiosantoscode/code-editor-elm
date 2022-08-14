module AST exposing (..)

import Html exposing (a)


type alias Assign =
    { name : String, expression : AST }


type AST
    = Block { assignments : List Assign }
    | Form { head : String, tail : List AST }
    | Number { value : Int }
    | Reference { name : String }


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


setAstChildren : AST -> List AST -> AST
setAstChildren ast newChildren =
    case ast of
        Block p ->
            Block { p | assignments = newChildren |> List.map (\c -> { name = "", expression = c }) }

        Form f ->
            Form { f | tail = newChildren }

        _ ->
            ast


type MutASTChildrenRet
    = ReplaceWithChild AST
    | PrependOneChild AST


flatMap : (Int -> a -> List b) -> List a -> List b
flatMap f xs =
    xs |> List.indexedMap f |> List.foldr (++) []


mutAstChildren : (Int -> AST -> MutASTChildrenRet) -> AST -> AST
mutAstChildren mutator ast =
    case ast of
        Block p ->
            Block
                { p
                    | assignments =
                        p.assignments
                            |> flatMap
                                (\index a ->
                                    case mutator index a.expression of
                                        ReplaceWithChild newChild ->
                                            [ { a | expression = newChild } ]

                                        PrependOneChild newChild ->
                                            [ { name = "", expression = newChild }, a ]
                                )
                }

        Form f ->
            Form
                { f
                    | tail =
                        f.tail
                            |> flatMap
                                (\index a ->
                                    case mutator index a of
                                        ReplaceWithChild newChild ->
                                            [ newChild ]

                                        PrependOneChild newChild ->
                                            [ newChild, a ]
                                )
                }

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


makeMutator : ASTTransformation -> (Maybe AST -> List (Maybe AST))
makeMutator transformation maybeExisting =
    case transformation of
        Insert newNode ->
            [ Just newNode, maybeExisting ]

        ReplaceWith newNode ->
            [ Just newNode ]


mutateNthChild : Int -> ASTTransformation -> AST -> MutASTChildrenRet
mutateNthChild index trans ast =
    let
        currentChildren =
            getAstChildren ast

        indexedMutator =
            \hereIndex child ->
                if hereIndex == index then
                    case trans of
                        Insert newNode ->
                            PrependOneChild newNode

                        ReplaceWith newNode ->
                            ReplaceWithChild newNode

                else
                    ReplaceWithChild child
    in
    if index == List.length currentChildren then
        ReplaceWithChild (addAstChild ast (getTransAddedNode trans))

    else
        ReplaceWithChild (mutAstChildren indexedMutator ast)


mutateTargetChild1 : List Int -> ASTTransformation -> AST -> MutASTChildrenRet
mutateTargetChild1 path transformer ast =
    case path of
        [] ->
            ReplaceWithChild (getTransAddedNode transformer)

        [ hereIndex ] ->
            mutateNthChild hereIndex transformer ast

        hereIndex :: rest ->
            ReplaceWithChild
                (mutAstChildren
                    (\index child ->
                        if index == hereIndex then
                            mutateTargetChild1 rest transformer child

                        else
                            ReplaceWithChild child
                    )
                    ast
                )


mutateTargetChild : List Int -> ASTTransformation -> AST -> AST
mutateTargetChild path transformer ast =
    case mutateTargetChild1 path transformer ast of
        ReplaceWithChild c ->
            c

        PrependOneChild _ ->
            Number { value = -500 }


generateVarName : List String -> Int -> String
generateVarName existingNames index =
    let
        newName =
            "number_" ++ String.fromInt index
    in
    if List.any ((==) newName) existingNames then
        generateVarName existingNames (index + 1)

    else
        newName


atIndex : List String -> Int -> String
atIndex list index =
    case list of
        [] ->
            "NAME MISSING"

        x :: xs ->
            if index == 0 then
                x

            else
                atIndex xs (index - 1)


getNthVarName : List String -> List Int -> String
getNthVarName varNames path =
    atIndex varNames (List.head path |> Maybe.withDefault -1)


isVariableAvailable : List String -> List Int -> String -> Bool
isVariableAvailable varNames path varName =
    let
        varNamesUpToHere =
            List.take (List.head path |> Maybe.withDefault -1) varNames
    in
    List.any ((==) varName) varNamesUpToHere


getNewVarNames : Path -> List String -> ASTTransformation -> List String
getNewVarNames mutPath existingNames trans =
    case mutPath of
        [ statementIndex ] ->
            case trans of
                Insert newNode ->
                    List.take statementIndex existingNames
                        ++ generateVarName existingNames 0
                        :: List.drop statementIndex existingNames

                ReplaceWith newNode ->
                    List.take statementIndex existingNames
                        ++ generateVarName existingNames 0
                        :: List.drop (statementIndex + 1) existingNames

        _ ->
            existingNames
