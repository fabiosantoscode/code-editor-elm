module Model exposing (..)

import AST exposing (..)


type alias Model =
    { program : AST
    , replacing : Replacement
    }


initialModel : Model
initialModel =
    { program =
        Program
            { expressions =
                [ Form
                    { head = "func"
                    , tail =
                        [ Number { value = 1 }
                        , Number { value = 2 }
                        , Number { value = 3 }
                        ]
                    }
                , Form
                    { head = "func"
                    , tail =
                        [ Number { value = 2 }
                        , Form
                            { head = "func"
                            , tail =
                                [ Number { value = 1 }
                                , Number { value = 2 }
                                ]
                            }
                        , Number { value = 3 }
                        ]
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
    | Noop


applyAstMutation : Model -> Model
applyAstMutation model =
    let
        applier =
            \replacement ->
                mutateTargetChild replacement.path
                    (if replacement.addition then
                        Insert (Number { value = 9999999999 })

                     else
                        ReplaceWith (Number { value = 9999999999 })
                    )
                    model.program

        newProgram =
            model.replacing
                |> Maybe.map applier
                |> Maybe.withDefault model.program
    in
    { model | program = newProgram, replacing = Nothing }


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


ctxEnterPath : IterationContext -> Int -> IterationContext
ctxEnterPath context index =
    { context | path = context.path ++ [ index ] }


ctxCurrentReplacePath : IterationContext -> Maybe Path
ctxCurrentReplacePath context =
    context.replacing
        |> Maybe.andThen
            (\r ->
                if r.addition == False then
                    Just r.path

                else
                    Nothing
            )


ctxCurrentAddPath : IterationContext -> Maybe Path
ctxCurrentAddPath context =
    context.replacing
        |> Maybe.andThen
            (\r ->
                if r.addition then
                    Just r.path

                else
                    Nothing
            )
