module AST exposing (..)


type AST
    = Program { expressions : List AST }
    | Form { head : String, tail : List AST }
    | Number { value : Int }


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
        Program p ->
            p.expressions

        Form f ->
            f.tail

        _ ->
            []


setAstChildren : AST -> List AST -> AST
setAstChildren ast newChildren =
    case ast of
        Program p ->
            Program { p | expressions = newChildren }

        Form f ->
            Form { f | tail = newChildren }

        _ ->
            ast


mutAstChildren : (List AST -> List AST) -> AST -> AST
mutAstChildren mutator ast =
    getAstChildren ast
        |> mutator
        |> setAstChildren ast


compactListOfMaybe : List (Maybe a) -> List a
compactListOfMaybe list =
    List.foldr
        (\x acc ->
            case x of
                Just y ->
                    y :: acc

                Nothing ->
                    acc
        )
        []
        list


makeMutator : ASTTransformation -> (Maybe AST -> List (Maybe AST))
makeMutator transformation maybeExisting =
    case transformation of
        Insert newNode ->
            [ Just newNode, maybeExisting ]

        ReplaceWith newNode ->
            [ Just newNode ]


mutateNthChild : Int -> ASTTransformation -> AST -> AST
mutateNthChild index trans ast =
    let
        currentChildren =
            getAstChildren ast

        indexedMutator =
            \hereIndex child ->
                if hereIndex == index then
                    case trans of
                        Insert newNode ->
                            [ newNode, child ]

                        ReplaceWith newNode ->
                            [ newNode ]

                else
                    [ child ]

        mutList =
            if index == List.length currentChildren then
                currentChildren ++ [ getTransAddedNode trans ]

            else
                List.indexedMap indexedMutator currentChildren
                    |> List.foldr (++) []
    in
    setAstChildren ast mutList


mutateTargetChild : (List Int) -> ASTTransformation -> AST -> AST
mutateTargetChild path transformer ast =
    case path of
        [] ->
            getTransAddedNode transformer

        [ hereIndex ] ->
            mutateNthChild hereIndex transformer ast

        hereIndex :: rest ->
            mutAstChildren
                (List.indexedMap
                    (\index child ->
                        if index == hereIndex then
                            mutateTargetChild rest transformer child

                        else
                            child
                    )
                )
                ast
