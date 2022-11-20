module RenderEditor exposing (renderEditor)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Machine.Errors
import Machine.Parse
import Machine.Run
import Machine.StandardLibrary exposing (FunctionDisplayType(..))
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

        resultHere =
            Machine.Run.runPath model.program ctx.path

        astResult =
            case resultHere of
                Err (Machine.Errors.NoResult _) ->
                    span [ class "color-ref-nothing" ] [ text "no data" ]

                Err e ->
                    span [ style "color" "red" ] [ text (Machine.Errors.formatError e) ]

                Ok r ->
                    text (String.fromInt r)
    in
    case ast of
        Number { value } ->
            [ replaceableLeafNode ctx className (String.fromInt value) ]

        Reference { name } ->
            if renderNodeExpanded ctx ast then
                case resultHere of
                    Err (Machine.Errors.NoResult _) ->
                        [ replaceableLeafNode ctx (className ++ " color-ref-nothing font-italic") name ]

                    Err _ ->
                        [ replaceableLeafNode ctx (className ++ " color-indirect-error") name ]

                    Ok _ ->
                        [ replaceableLeafNode ctx className name ]

            else
                case resultHere of
                    Err _ ->
                        [ replaceButton ctx.path "color-ref-nothing font-italic" (text "no data") ]

                    Ok val ->
                        [ replaceButton ctx.path "color-inline-result cursor-pointer" (text (String.fromInt val)) ]

        Incomplete ->
            [ replaceableLeafNode ctx className "" ]

        Block p ->
            [ div [ class className ] (renderBlock model ctx p) ]

        Form f ->
            if not (renderNodeExpanded ctx ast) then
                case resultHere of
                    Err _ ->
                        [ replaceButton ctx.path "color-indirect-error" astResult ]

                    _ ->
                        [ replaceButton ctx.path "color-inline-result cursor-pointer" astResult ]

            else
                let
                    errorOriginatedHere =
                        case resultHere of
                            Err (Machine.Errors.NoResult _) ->
                                False

                            Err e ->
                                ctx.path == Machine.Errors.getErrorPath e

                            _ ->
                                False

                    childRender index child =
                        renderEditor model (ctxEnterPath ctx index) child
                in
                (if errorOriginatedHere && ctxIsReplacingPath ctx ctx.path then
                    [ div [ class "font-size-sm margin-top-0 padding-l" ] [ astResult ] ]

                 else
                    []
                )
                    ++ [ replaceableNode ctx
                            (div
                                [ class className
                                , class ("ast-form--depth-" ++ String.fromInt (List.length ctx.path))
                                , class "layout-vertical padding-y padding-x"
                                , classList [ ( "outline-error", errorOriginatedHere ) ]
                                ]
                                (replaceButton ctx.path "ast-form__head" (text f.head) :: flatMap childRender f.tail)
                            )
                       ]


renderNodeExpanded : IterationContext -> AST -> Bool
renderNodeExpanded ctx node =
    if containsIncompleteNode node then
        True

    else
        case ctx.replacing of
            Just { path } ->
                pathStartsWith path ctx.path

            _ ->
                False


classListFor : AST -> List String
classListFor ast =
    case ast of
        Block _ ->
            [ "ast-program", "layout-vertical" ]

        Form _ ->
            [ "ast-form" ]

        Number _ ->
            [ "ast-number" ]

        Reference _ ->
            [ "ast-reference" ]

        Incomplete ->
            [ "ast-incomplete", "min-width-1ch", "min-height-1em" ]


renderBlock : Model -> IterationContext -> RBlock -> List (Html Msg)
renderBlock model ctx { assignments } =
    let
        enter =
            ctxEnterPath ctx

        suggestions i contents =
            renderSuggestions model i contents

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
        , onClick (InitiateReplace path)
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
            button
                [ class "ast-input margin-y-form-child"
                , class className
                , onClick (InitiateReplace ctx.path)
                ]
                [ text contents ]


nodeReplacementPreview : String -> Replacement -> Html Msg
nodeReplacementPreview className replacement =
    let
        ( validityClassName, textualPreview ) =
            Machine.Parse.tryParseAtomToString replacement.search
                |> Maybe.map (\ss -> ( "ast-color-valid-replacement", ss ))
                |> Maybe.withDefault ( "ast-color-invalid-replacement", "no" )
    in
    button
        [ class "ast-input margin-y-form-child"
        , class className
        , class validityClassName
        ]
        [ text textualPreview ]


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
