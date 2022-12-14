module AST exposing (..)

import Set exposing (Set)
import Utils exposing (..)


type alias Assign =
    { name : String, expression : AST }


type alias RForm =
    { head : String, tail : List AST }


type alias RBlock =
    { assignments : List Assign }


type AST
    = Block RBlock
    | Form RForm
    | Number { value : Int }
    | Reference { name : String }
    | Incomplete


type ASTTransformation
    = Insert AST
    | ReplaceWith AST


type alias IterationContext =
    { path : Path
    , replacing : Maybe Replacement
    }


type alias Path =
    List Int


type alias Replacement =
    { path : Path, search : String }


getDepth : AST -> Int
getDepth ast =
    getAstChildren ast
        |> List.map (\child -> 1 + getDepth child)
        |> List.maximum
        |> Maybe.withDefault 1


getAstNodeByPath : Path -> AST -> Maybe AST
getAstNodeByPath path ast =
    case path of
        [] ->
            Just ast

        index :: rest ->
            getAstChildren ast
                |> listAt index
                |> Maybe.andThen (getAstNodeByPath rest)


containsIncompleteNode : AST -> Bool
containsIncompleteNode ast =
    case ast of
        Incomplete ->
            True

        _ ->
            List.any containsIncompleteNode (getAstChildren ast)


getTransAddedNode : ASTTransformation -> AST
getTransAddedNode trans =
    case trans of
        Insert x ->
            x

        ReplaceWith x ->
            x


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
        childCount =
            case ast of
                Block p ->
                    p.assignments |> List.length

                Form f ->
                    f.tail |> List.length

                _ ->
                    -1

        indexedMutator =
            \hereIndex child ->
                if hereIndex == index then
                    trans

                else
                    ReplaceWith child
    in
    if index == childCount then
        ReplaceWith (addAstChild ast (getTransAddedNode trans))

    else
        ReplaceWith (mutAstChildren indexedMutator ast)


insertVarName : Path -> String -> AST -> Maybe ( Path, AST )
insertVarName path name ast =
    case ( path, ast ) of
        ( statIndex :: _, Block b ) ->
            Just
                ( [ statIndex - 1 ]
                , Block
                    { b
                        | assignments =
                            flatMap
                                (\i stat ->
                                    if i == statIndex then
                                        [ { name = name, expression = Incomplete }, stat ]

                                    else
                                        [ stat ]
                                )
                                b.assignments
                    }
                )

        _ ->
            Nothing


refactorToNewVariable : Path -> String -> AST -> Maybe ( Path, AST )
refactorToNewVariable path name ast =
    ast
        |> commitASTTransformation path (ReplaceWith (Reference { name = name }))
        |> insertVarName path name


mutateTargetChildInner : List Int -> ASTTransformation -> AST -> ASTTransformation
mutateTargetChildInner path transformer ast =
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
                            mutateTargetChildInner rest transformer child

                        else
                            ReplaceWith child
                    )
                    ast
                )



-- Make sure {path} addresses an available path


truncatePathToAstInner : Bool -> Path -> AST -> Path
truncatePathToAstInner isAdd path ast =
    case path of
        [] ->
            []

        [ leafIndex ] ->
            let
                maxIndex =
                    List.length (getAstChildren ast)
                        -- leafIndex can be == list length to add at last
                        + bool2Int isAdd
            in
            if leafIndex > maxIndex then
                []

            else
                [ leafIndex ]

        index :: rest ->
            listAt index (getAstChildren ast)
                |> Maybe.map (\e -> index :: truncatePathToAstInner isAdd rest e)
                |> Maybe.withDefault []


truncatePathToAst : Bool -> Path -> AST -> Maybe Path
truncatePathToAst isAdd path ast =
    case truncatePathToAstInner isAdd path ast of
        [] ->
            Nothing

        x ->
            Just x


getAstChildren : AST -> List AST
getAstChildren ast =
    case ast of
        Block { assignments } ->
            assignments |> List.map (\a -> a.expression)

        Form { tail } ->
            tail

        _ ->
            []


mutateTargetChild : List Int -> ASTTransformation -> AST -> AST
mutateTargetChild path transformer ast =
    case mutateTargetChildInner path transformer ast of
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


generateIdForPath : String -> Path -> String
generateIdForPath prefix path =
    prefix ++ (path |> List.map String.fromInt |> String.join ".")


generateIdForAdd : Path -> String
generateIdForAdd =
    generateIdForPath "add-statement-"


isVariableVisibleFrom : List String -> Path -> String -> Bool
isVariableVisibleFrom varNames path varName =
    let
        varNamesUpToHere =
            List.take (List.head path |> Maybe.withDefault -1) varNames
    in
    List.any ((==) varName) varNamesUpToHere


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


ctxEnterPath : IterationContext -> Int -> IterationContext
ctxEnterPath context index =
    { context | path = context.path ++ [ index ] }
