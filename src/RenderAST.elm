module RenderAST exposing (..)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Utils exposing (..)



--- Rendering replacer machinery


replaceButton : Path -> String -> Html Msg -> Html Msg
replaceButton path className astHtml =
    button
        [ class ("ast-button " ++ className)
        , onClick (InitiateReplace path "")
        ]
        [ astHtml ]


addButton : IterationContext -> Html Msg
addButton ctx =
    let
        attributes =
            if ctxCurrentAddPath ctx == Just ctx.path then
                [ class "ast-button ast-add-button ast-add-button--adding" ]

            else
                [ class "ast-button ast-add-button ast-add-button--clickable"
                , onClick (InitiateAdd ctx.path "")
                ]
    in
    button attributes [ span [] [] ]


varNameIsClickable : Model -> List String -> String -> Bool
varNameIsClickable model varNames varName =
    model.replacing
        |> Maybe.map (\r -> isVariableVisibleFrom varNames r.path varName)
        |> Maybe.withDefault False


renderClickableVarName : Model -> List String -> String -> Html Msg
renderClickableVarName model varNames varName =
    let
        attributes =
            if varNameIsClickable model varNames varName then
                [ class "ast-button color-var-name ast-var-name ast-var-name--clickable"
                , onClick (CommitChange (Reference { name = varName }))
                ]

            else
                [ class "ast-button color-var-name ast-var-name" ]
    in
    button attributes [ text (varName ++ ":") ]



--- Rendering AST nodes


renderForm : Path -> RForm -> List (Html Msg) -> List (Html Msg)
renderForm path form tail =
    let
        btn =
            replaceButton path "ast-form__head"

        endBtn =
            replaceButton path "ast-form__endparen"
    in
    btn (text (form.head ++ "(")) :: tail ++ [ endBtn (text ")") ]


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxCurrentReplacePath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]


classListFor : AST -> List String
classListFor ast =
    case ast of
        Block _ ->
            [ "ast-program", "layout-vertical" ]

        Form _ ->
            [ "ast-form", "layout-horizontal" ]

        Number _ ->
            [ "ast-number" ]

        Reference _ ->
            [ "ast-reference", "color-var-name" ]

        Incomplete ->
            [ "ast-incomplete" ]


--- Bringing it all together


renderEditor : Model -> IterationContext -> AST -> List (Html Msg)
renderEditor model ctx ast =
    let
        className =
            classListFor ast
                ++ beingReplacedClasses ctx
                |> String.join " "
    in
    case ast of
        Block p ->
            let
                varNames =
                    p.assignments |> List.map (\a -> a.name)

                renderWithVarName =
                    \index { expression, name } ->
                        renderClickableVarName model varNames name
                            :: renderEditor model (ctxEnterPath ctx index) expression
                            ++ [ addButton (ctxEnterPath ctx index) ]
            in
            [ div
                [ class className ]
                (flatMap renderWithVarName p.assignments
                    ++ [ addButton (ctxEnterPath ctx (List.length p.assignments)) ])
            ]

        Form f ->
            let
                childRender =
                    \index child ->
                        renderEditor model (ctxEnterPath ctx index) child
            in
            [ div
                [ class className
                , class ("ast-form--depth-" ++ String.fromInt (List.length ctx.path))
                ]
                (renderForm ctx.path
                    f
                    (flatMap childRender f.tail)
                )
            ]

        Number { value } ->
            [ replaceButton ctx.path className (text (String.fromInt value)) ]

        Reference { name } ->
            [ replaceButton ctx.path className (text name) ]

        Incomplete ->
            [ replaceButton ctx.path className (text "") ]
