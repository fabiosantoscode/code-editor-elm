module Model exposing (..)

import AST exposing (..)
import Browser.Dom
import Machine.Parse exposing (tryParseAtomAst)
import Machine.Run
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
                            { head = "÷"
                            , tail =
                                [ Number { value = 40 }, Number { value = 0 } ]
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
    | InitiateReplace Path
    | UpdateSearch String
    | ReplaceWithNewVariable Path String
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
            ( { model
                | program = commitASTTransformation path (Insert AST.Incomplete) model.program
                , replacing = makeReplacement path ""
              }
            , focusPathCmd path
            )

        ReplaceWithNewVariable path name ->
            case refactorToNewVariable path name model.program of
                Just ( newStatPath, newProgram ) ->
                    ( { model
                        | program = newProgram
                        , replacing = makeReplacement newStatPath ""
                      }
                    , focusPathCmd newStatPath
                    )

                Nothing ->
                    Debug.todo "invalid path or toplevel block"

        InitiateReplace path ->
            setReplacement model path

        UpdateSearch newSearch ->
            noCmd (setReplacementSearch newSearch model)

        CommitChange newAst ->
            case model.replacing of
                Just r ->
                    ( { model | program = commitASTTransformation r.path (ReplaceWith newAst) model.program }
                        |> setReplacementSearch ""
                    , focusPathCmd r.path
                    )

                Nothing ->
                    Debug.todo "replacing nothing"


focusPathCmd : Path -> Cmd Msg
focusPathCmd path =
    Task.attempt Focus (Browser.Dom.focus (generateIdForAdd path))


setReplacementSearch : String -> Model -> Model
setReplacementSearch newSearch m =
    { m | replacing = Maybe.map (\r -> { r | search = newSearch }) m.replacing }



-- Update model.replacing while auto-applying good AST changes and focusing the window


setReplacement : Model -> Path -> ( Model, Cmd Msg )
setReplacement model newPath =
    let
        newProgram =
            case model.replacing of
                Just { search, path } ->
                    case
                        ( path /= newPath
                        , tryParseAtomAst search (Machine.Run.findAllVarNames model.program)
                        )
                    of
                        ( True, Just ast ) ->
                            applyAstMutation model ast

                        _ ->
                            model.program

                _ ->
                    model.program

        -- User might be replacing the parent of their new path
        ( newReplacing, command ) =
            case truncatePathToAst False newPath newProgram of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just validPath ->
                    ( makeReplacement validPath ""
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
