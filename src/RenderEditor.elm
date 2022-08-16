module RenderEditor exposing (renderEditor)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Machine.StandardLibrary exposing (FunctionDisplayType(..), getStandardLibFunction)
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


addStatementButton : IterationContext -> Html Msg
addStatementButton ctx =
    let
        baseClass =
            [ class "ast-button ast-add-statement-button" ]

        attributes =
            if ctxIsAddingPath ctx ctx.path then
                button
                    (baseClass
                        ++ [ class "ast-add-statement-button--adding"
                           , class "outline-replace"
                           ]
                    )
                    [ span [] [text "Add statement"] ]

            else
                button
                    (baseClass
                        ++ [ class "ast-add-statement-button--clickable"
                           , onClick (InitiateAdd ctx.path "")
                           ]
                    )
                    [ span [] [] ]
    in
    attributes


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


renderPrefixForm : Path -> RForm -> List (Html Msg) -> List (Html Msg)
renderPrefixForm path form tailHtml =
    let
        btn =
            replaceButton path "ast-form__head"

        endBtn =
            replaceButton path "ast-form__endparen"
    in
    btn (text (form.head ++ "(")) :: tailHtml ++ [ endBtn (text ")") ]


renderBinOpForm : Path -> String -> ( Html Msg, Html Msg ) -> List (Html Msg)
renderBinOpForm path opName ( left, right ) =
    let
        btn =
            replaceButton path "ast-form__op"
    in
    [ btn (text opName), left, right ]


renderForm : Path -> RForm -> List (Html Msg) -> List (Html Msg)
renderForm path form tailHtml =
    getStandardLibFunction form.head
        |> Maybe.andThen
            (\f ->
                if f.display == DispOperator then
                    Maybe.map (renderBinOpForm path form.head) (list2ToTuple tailHtml)

                else
                    Nothing
            )
        |> Maybe.withDefault (renderPrefixForm path form tailHtml)


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxIsReplacingPath ctx ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced", "outline-replace" ]

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
                            ++ [ addStatementButton (ctxEnterPath ctx (index + 1)) ]
            in
            [ div
                [ class className ]
                (addStatementButton (ctxEnterPath ctx 0)
                    :: flatMap renderWithVarName p.assignments
                )
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
