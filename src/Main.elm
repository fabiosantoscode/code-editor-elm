module Main exposing (main)

import AST exposing (..)
import Browser
import Html exposing (Html, button, div, input, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import IterationContext exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { program : AST
    , replacing : Replacement
    }


init : Model
init =
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



-- UPDATE


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


type Msg
    = InitiateAdd Path String
    | InitiateReplace Path String


makeReplacement : Path -> Bool -> String -> Replacement
makeReplacement path addition search =
    Just { path = path, search = search, addition = addition }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InitiateAdd path search ->
            { model | replacing = makeReplacement path True search }

        InitiateReplace path search ->
            { model | replacing = makeReplacement path False search }



-- VIEW


addingPlaceholder : Html msg
addingPlaceholder =
    div [ class "ast-replaceable ast-replaceable--being-replaced" ] [ text "..." ]


mapWithAddButtons : IterationContext -> (Int -> AST -> Html Msg) -> List AST -> List (Html Msg)
mapWithAddButtons ctx renderer astList =
    let
        conditonalAddButton =
            \index ->
                if ctxAddingPath ctx == Just (ctx.path ++ [ index ]) then
                    addingPlaceholder

                else
                    addButton ctx.path index

        renderWithButton =
            \index astChild ->
                [ conditonalAddButton index, renderer index astChild ]

        mapped =
            List.indexedMap renderWithButton astList
                |> List.foldr (++) []
    in
    mapped ++ [ conditonalAddButton (List.length astList) ]


replaceButton : Path -> String -> Html Msg -> Html Msg
replaceButton path className astHtml =
    button
        [ class ("ast-replace-button " ++ className)
        , onClick (InitiateReplace path "")
        ]
        [ astHtml ]


addButton : Path -> Int -> Html Msg
addButton path index =
    button
        [ class ""
        , onClick (InitiateAdd (path ++ [ index ]) "")
        ]
        [ text "+" ]



--- Rendering AST nodes


renderPunctuation : String -> Html Msg
renderPunctuation symbol =
    span [ class "ast-punctuation" ] [ text symbol ]


renderForm : List (Html Msg) -> List (Html Msg)
renderForm inner =
    renderPunctuation "(" :: inner ++ [ renderPunctuation ")" ]


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxReplacingPath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]


layoutClasses : AST -> List String
layoutClasses ast =
    case ast of
        Program _ ->
            [ "layout-vertical" ]

        Form _ ->
            [ "layout-horizontal" ]

        _ ->
            []


renderAst : IterationContext -> AST -> Html Msg
renderAst ctx ast =
    let
        classListFor =
            case ast of
                Program _ ->
                    [ "ast-program" ]

                Form _ ->
                    [ "ast-form" ]

                Number _ ->
                    [ "ast-number" ]

        className =
            "ast"
                :: classListFor
                ++ beingReplacedClasses ctx
                ++ layoutClasses ast
                |> String.join " "

        recurse =
            \i -> renderAst (ctxAppendPath ctx i)

        recurseChildren =
            mapWithAddButtons ctx recurse
    in
    case ast of
        Program { expressions } ->
            div [ class className ] (recurseChildren expressions)

        Form { head, tail } ->
            div [ class className ]
                (renderForm
                    (replaceButton ctx.path "" (renderPunctuation head)
                        :: recurseChildren tail
                    )
                )

        Number { value } ->
            replaceButton ctx.path className (text (String.fromInt value))


renderReplaceUi : Model -> List (Html Msg)
renderReplaceUi { replacing } =
    case replacing of
        Nothing ->
            []

        Just { path, search, addition } ->
            let
                replaceText =
                    if addition then
                        "Add"

                    else
                        "Replace"
            in
            [ div [ class "ast-replace-ui" ]
                [ text replaceText
                , div [ class "ast-replace-path" ]
                    [ text (String.join "." (List.map String.fromInt path)) ]
                , div [ class "ast-replace-search" ]
                    [ input
                        [ class "ast-replace-search-input"
                        , value search
                        , onInput (InitiateReplace path)
                        ]
                        []
                    ]
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        ([ node "link" [ rel "stylesheet", href "/assets/styles.css" ] []
         , renderAst
            { path = [], replacing = model.replacing }
            model.program
         ]
            ++ renderReplaceUi model
        )
