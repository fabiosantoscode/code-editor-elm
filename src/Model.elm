module Model exposing (..)

import AST exposing (..)
import Browser.Dom
import Machine.Parse exposing (tryParseAtomAst)
import Task


type alias Model =
    { program : AST
    , replacing : Maybe Replacement
    }


initialModel : Model
initialModel =
    { program =
        Block
            { assignments =
                [ { name = "addedNumbers"
                  , expression =
                        Form
                            { head = "+"
                            , tail =
                                [ Number { value = 40 }
                                , Number { value = 2 }
                                ]
                            }
                  }
                , { name = "variable2"
                  , expression =
                        Form
                            { head = "And"
                            , tail =
                                [ Form
                                    { head = "=="
                                    , tail =
                                        [ Reference { name = "addedNumbers" }
                                        , Number { value = 3 }
                                        ]
                                    }
                                , Number { value = 3 }
                                ]
                            }
                  }
                , { name = "reference"
                  , expression = Reference { name = "addedNumbers" }
                  }
                ]
            }
    , replacing =
        Just
            { path = [ 2 ]
            , search = ""
            }
    }


type Msg
    = AddExpression Path
    | InitiateReplace Path String
    | UpdateSearch String
    | CommitChange AST
    | Focus (Result Browser.Dom.Error ())
    | Noop


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    let
        noCmd m =
            ( m, Cmd.none )
    in
    case msg of
        Noop ->
            noCmd model

        Focus _ ->
            -- Is it important to handle the focus error?
            noCmd model

        AddExpression path ->
            noCmd { model | program = commitASTTransformation path (Insert AST.Incomplete) model.program }

        InitiateReplace path search ->
            setReplacement model path False search

        UpdateSearch newSearch ->
            noCmd (setReplacementSearch model newSearch)

        CommitChange newAst ->
            noCmd { model | program = applyAstMutation model newAst, replacing = Nothing }


setReplacementSearch : Model -> String -> Model
setReplacementSearch model newSearch =
    { model
        | replacing = Maybe.map (\r -> { r | search = newSearch }) model.replacing
    }



-- Update model.replacing while auto-applying good AST changes and focusing the window


setReplacement : Model -> Path -> Bool -> String -> ( Model, Cmd Msg )
setReplacement model newPath isAdd newSearch =
    let
        newProgram =
            case model.replacing of
                Just { search, path } ->
                    case ( path /= newPath, tryParseAtomAst search ) of
                        ( True, Just ast ) ->
                            applyAstMutation model ast

                        _ ->
                            model.program

                _ ->
                    model.program

        -- User might be replacing the parent of their new path
        ( newReplacing, command ) =
            case truncatePathToAst isAdd newPath newProgram of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just validPath ->
                    ( makeReplacement validPath newSearch
                    , Task.attempt Focus (Browser.Dom.focus (generateIdForAdd validPath))
                    )
    in
    ( { model | program = newProgram, replacing = newReplacing }, command )


applyAstMutation : Model -> AST -> AST
applyAstMutation model ast =
    case model.replacing of
        Just { path } ->
            commitASTTransformation path (ReplaceWith ast) model.program

        Nothing ->
            model.program


makeReplacement : Path -> String -> Maybe Replacement
makeReplacement path search =
    Just { path = path, search = search }


ctxIsReplacingPath : IterationContext -> Path -> Bool
ctxIsReplacingPath context path =
    context.replacing
        |> Maybe.map (\r -> r.path == path)
        |> Maybe.withDefault False


pathStartsWith : Path -> Path -> Bool
pathStartsWith haystack needle =
    case ( haystack, needle ) of
        ( _, [] ) ->
            True

        ( itemh :: resth, itemn :: restn ) ->
            itemh == itemn && pathStartsWith resth restn

        _ ->
            False
