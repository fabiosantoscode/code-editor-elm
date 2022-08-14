module RenderAST exposing (..)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)


mapWithAddButtons : IterationContext -> List AST -> List (Html Msg)
mapWithAddButtons ctx astList =
    let
        childCtx =
            ctxEnterPath ctx

        renderWithButton =
            \index child ->
                [ addButton (childCtx index)
                , renderAst (childCtx index) child
                ]

        addButtonAtEnd =
            addButton (childCtx (List.length astList))

        mapped =
            List.indexedMap renderWithButton astList
                |> List.foldr (++) []
    in
    mapped ++ [ addButtonAtEnd ]


replaceButton : Path -> String -> Html Msg -> Html Msg
replaceButton path className astHtml =
    button
        [ class ("ast-button ast-replace-button " ++ className)
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



--- Rendering AST nodes


renderPunctuation : String -> Html Msg
renderPunctuation symbol =
    span [ class "ast-punctuation" ] [ text symbol ]


renderForm : Path -> List (Html Msg) -> List (Html Msg)
renderForm path inner =
    let
        punc =
            \character ->
                button [ class "ast-button ast-punctuation", onClick (InitiateReplace path "") ] [ text character ]
    in
    punc "(" :: inner ++ [ punc ")" ]


layoutClasses : AST -> List String
layoutClasses ast =
    case ast of
        Program _ ->
            [ "layout-vertical" ]

        Form _ ->
            [ "layout-horizontal" ]

        _ ->
            []


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxCurrentReplacePath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]


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
    in
    case ast of
        Program { expressions } ->
            div [ class className ] (mapWithAddButtons ctx expressions)

        Form { head, tail } ->
            div [ class className ]
                (renderForm ctx.path
                    (replaceButton ctx.path "" (renderPunctuation head)
                        :: mapWithAddButtons ctx tail
                    )
                )

        Number { value } ->
            replaceButton ctx.path className (text (String.fromInt value))
