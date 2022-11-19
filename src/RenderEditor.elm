module RenderEditor exposing (renderEditor)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Machine.Errors
import Machine.Parse exposing (tryParseAtomToString)
import Machine.Run
import Machine.StandardLibrary exposing (FunctionDisplayType(..), getFunctionDisplayType)
import Model exposing (..)
import RenderSuggestions exposing (renderSuggestions)
import Utils exposing (..)



--- Rendering replacer machinery


renderEditor : Model -> IterationContext -> AST -> List (Html Msg)
renderEditor model ctx ast =
    let
        className =
            classListFor ast
                ++ beingReplacedClasses ctx
                |> String.join " "

        renderedHtml =
            case ast of
                Number { value } ->
                    [ replaceableLeafNode ctx className (String.fromInt value) ]

                Reference { name } ->
                    [ replaceableLeafNode ctx className name ]

                Incomplete ->
                    [ replaceableLeafNode ctx className "" ]

                Block p ->
                    [ div [ class className ] (renderBlock model ctx p) ]

                Form f ->
                    let
                        childRender index child =
                            renderEditor model (ctxEnterPath ctx index) child
                    in
                    [ replaceableNode ctx
                        (div
                            [ class className
                            , class ("ast-form--depth-" ++ String.fromInt (List.length ctx.path))
                            , class
                                (if renderFormHorizontally ast then
                                    "layout-horizontal"

                                 else
                                    "layout-vertical padding-y padding-x-2"
                                )
                            ]
                            (renderForm ctx.path
                                f
                                childRender
                            )
                        )
                    ]

        resultHere =
            Machine.Run.runPath model.program ctx.path

        astResult =
            case resultHere of
                Err e ->
                    span [ style "color" "red" ] [ text (Machine.Errors.formatError e) ]

                Ok r ->
                    text (String.fromInt r)
    in
    case ast of
        Block _ ->
            renderedHtml

        Number _ ->
            renderedHtml

        Incomplete ->
            renderedHtml

        _ ->
            case ( resultHere, ctx.path ) of
                ( Err Machine.Errors.NoResult, _ ) ->
                    -- Only show errors in the statement level (don't worry they bubble up)
                    if renderResultInstead ctx ast then
                        [ replaceButton ctx.path "color-inline-result cursor-pointer" (text "n/a") ]

                    else
                        renderedHtml

                ( Err _, [ _ ] ) ->
                    -- Only show errors in the statement level (don't worry they bubble up)
                    renderedHtml ++ [ astResult ]

                _ ->
                    if renderResultInstead ctx ast then
                        [ replaceButton ctx.path "color-inline-result cursor-pointer" astResult ]

                    else
                        renderedHtml


classListFor : AST -> List String
classListFor ast =
    case ast of
        Block _ ->
            [ "ast-program", "layout-vertical" ]

        Form _ ->
            [ "ast-form", "reveal-bg-behind" ]

        Number _ ->
            [ "ast-number" ]

        Reference _ ->
            [ "ast-reference" ]

        Incomplete ->
            [ "ast-incomplete", "reveal-bg-behind", "min-width-1ch", "min-height-1em" ]


renderBlock : Model -> IterationContext -> RBlock -> List (Html Msg)
renderBlock model ctx { assignments } =
    let
        enter =
            ctxEnterPath ctx

        suggestions i contents =
            renderSuggestions model (enter i) contents

        addWidget i =
            [ addStatementButton (enter i) (i == List.length assignments) ]

        renderWithVarName i { expression, name } =
            div []
                (button
                    [ class "ast-button color-var-name ast-var-name" ]
                    [ text (name ++ ":") ]
                    :: suggestions i (renderEditor model (enter i) expression)
                    ++ addWidget (i + 1)
                )
    in
    addWidget 0
        ++ List.indexedMap renderWithVarName assignments


renderFormHorizontally : AST -> Bool
renderFormHorizontally form =
    getDepth form < 3


renderResultInstead : IterationContext -> AST -> Bool
renderResultInstead ctx node =
    if containsIncompleteNode node then
        False

    else
        case ctx.replacing of
            Just { path } ->
                not (pathStartsWith path ctx.path)

            _ ->
                True


renderForm : Path -> RForm -> (Int -> AST -> List (Html Msg)) -> List (Html Msg)
renderForm path form childRender =
    let
        btn =
            replaceButton path "ast-form__head" (text form.head)

        isBinOp =
            getFunctionDisplayType form.head == Just DispOperator
    in
    case ( form.tail, isBinOp ) of
        ( [ left, right ], True ) ->
            childRender 0 left
                ++ btn
                :: childRender 1 right

        _ ->
            btn :: flatMap childRender form.tail


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxIsReplacingPath ctx ctx.path then
        [ "ast-replaceable", "outline-replace" ]

    else
        [ "ast-replaceable" ]


replaceButton : Path -> String -> Html Msg -> Html Msg
replaceButton path className astHtml =
    button
        [ class ("ast-button " ++ className)
        , onClick (InitiateReplace path "")
        ]
        [ astHtml ]


userIsReplacing : IterationContext -> Maybe Replacement
userIsReplacing ctx =
    ctx.replacing
        |> Maybe.andThen
            (\r ->
                if r.path == ctx.path && r.search /= "" then
                    Just r

                else
                    Nothing
            )


replaceableNode : IterationContext -> Html Msg -> Html Msg
replaceableNode ctx contents =
    userIsReplacing ctx
        |> Maybe.map (nodeReplacementPreview "")
        |> Maybe.withDefault contents


replaceableLeafNode : IterationContext -> String -> String -> Html Msg
replaceableLeafNode ctx className contents =
    case userIsReplacing ctx of
        Just r ->
            nodeReplacementPreview className r

        Nothing ->
            autoWidthButton
                [ class "ast-input margin-y-form-child"
                , class className
                , onClick (InitiateReplace ctx.path "")
                ]
                contents


nodeReplacementPreview : String -> Replacement -> Html Msg
nodeReplacementPreview className replacement =
    let
        ( validityClassName, textualPreview ) =
            tryParseAtomToString replacement.search
                |> Maybe.map (\ss -> ( "ast-color-valid-replacement", ss ))
                |> Maybe.withDefault ( "ast-color-invalid-replacement", "" )
    in
    autoWidthButton
        [ class "ast-input margin-y-form-child"
        , class className
        , class validityClassName
        ]
        textualPreview


autoWidthButton : List (Attribute Msg) -> String -> Html Msg
autoWidthButton attributes string =
    let
        w =
            string |> String.length |> String.fromInt
    in
    button (style "width" (w ++ "ch") :: attributes) [ text string ]


addStatementButton : IterationContext -> Bool -> Html Msg
addStatementButton ctx isLast =
    if isLast then
        button
            [ class "ast-button ast-add-statement-input"
            , class "ast-add-statement-input--clickable-last"
            , onClick (AddExpression ctx.path)
            ]
            [ text "+" ]

    else
        button
            [ class "ast-button ast-add-statement-input"
            , class "ast-add-statement-input--clickable"
            , onClick (AddExpression ctx.path)
            ]
            [ span [] [] ]
