module Model exposing (..)

import AST exposing (..)


type alias Model =
    { program : AST
    , replacing : Replacement
    }


initialModel : Model
initialModel =
    { program =
        Block
            { assignments =
                [ { name = "form1"
                  , expression =
                        Form
                            { head = "func"
                            , tail =
                                [ Number { value = 1 }
                                , Number { value = 2 }
                                , Number { value = 3 }
                                ]
                            }
                  }
                , { name = "form2"
                  , expression =
                        Form
                            { head = "func"
                            , tail =
                                [ Number { value = 2 }
                                , Form
                                    { head = "func"
                                    , tail =
                                        [ Number { value = 1 }
                                        , Number { value = 2 }
                                        , Form
                                            { head = "func"
                                            , tail =
                                                [ Number { value = 1 }
                                                , Number { value = 2 }
                                                , Number { value = 3 }
                                                , Incomplete
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                  }
                ]
            }
    , replacing =
        Just
            { path = [ 2 ]
            , search = ""
            , addition = True
            }
    }


type Msg
    = InitiateAdd Path String
    | InitiateReplace Path String
    | CommitChange AST
    | Noop


applyAstMutation : Model -> AST -> Model
applyAstMutation model ast =
    case model.replacing of
        Just replacement ->
            let
                transformation =
                    if replacement.addition then
                        Insert ast

                    else
                        ReplaceWith ast
            in
            { program = commitASTTransformation replacement.path transformation model.program
            , replacing = Nothing
            }

        Nothing ->
            model


makeReplacement : Path -> Bool -> String -> Replacement
makeReplacement path addition search =
    Just { path = path, search = search, addition = addition }


applyMsg : Msg -> Model -> Model
applyMsg msg model =
    case msg of
        Noop ->
            model

        InitiateAdd path search ->
            { model | replacing = makeReplacement path True search }

        InitiateReplace path search ->
            { model | replacing = makeReplacement path False search }

        CommitChange newAst ->
            applyAstMutation model newAst


ctxEnterPath : IterationContext -> Int -> IterationContext
ctxEnterPath context index =
    { context | path = context.path ++ [ index ] }


ctxIsReplacingPath : IterationContext -> Path -> Bool
ctxIsReplacingPath context path =
    context.replacing
        |> Maybe.map
            (\r ->
                if r.addition == False then
                    r.path == path

                else
                    False
            )
        |> Maybe.withDefault False


ctxIsAddingPath : IterationContext -> Path -> Bool
ctxIsAddingPath context path =
    context.replacing
        |> Maybe.map
            (\r ->
                if r.addition then
                    r.path == path

                else
                    False
            )
        |> Maybe.withDefault False
