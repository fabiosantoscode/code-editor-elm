module RenderEditor exposing (renderEditor)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2)
import Machine.StandardLibrary exposing (FunctionDisplayType(..), getStandardLibFunction)
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
    in
    case ast of
        Block p ->
            [ div [ class className ] (renderBlock model ctx p) ]

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
            [ replaceInput ctx className (String.fromInt value) ]

        Reference { name } ->
            [ replaceInput ctx className name ]

        Incomplete ->
            [ replaceInput ctx className "" ]


classListFor : AST -> List String
classListFor ast =
    case ast of
        Block _ ->
            [ "ast-program", "layout-vertical" ]

        Form _ ->
            [ "ast-form", "layout-horizontal", "reveal-bg-behind" ]

        Number _ ->
            [ "ast-number" ]

        Reference _ ->
            [ "ast-reference", "color-var-name" ]

        Incomplete ->
            [ "ast-incomplete", "reveal-bg-behind" ]


renderBlock : Model -> IterationContext -> RBlock -> List (Html Msg)
renderBlock model ctx { assignments } =
    let
        varNames =
            List.map .name assignments

        enter =
            ctxEnterPath ctx

        suggestions i contents =
            renderSuggestions model (enter i) False contents

        addWidget i =
            renderSuggestions model
                (enter i)
                True
                [ addStatementButton (enter i) (i == List.length assignments) ]

        renderWithVarName i { expression, name } =
            div []
                (renderClickableVarName model varNames name
                    :: suggestions i (renderEditor model (enter i) expression)
                    ++ addWidget (i + 1)
                )
    in
    addWidget 0
        ++ List.indexedMap (lazy2 renderWithVarName) assignments


renderPrefixForm : Path -> String -> List (Html Msg) -> List (Html Msg)
renderPrefixForm path headString tailHtml =
    let
        btn =
            replaceButton path "ast-form__head"

        endBtn =
            replaceButton path "ast-form__endparen"
    in
    btn (text (headString ++ "(")) :: tailHtml ++ [ endBtn (text ")") ]


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
                    list2ToTuple tailHtml
                        |> Maybe.map (renderBinOpForm path form.head)

                else
                    Nothing
            )
        |> Maybe.withDefault (renderPrefixForm path form.head tailHtml)


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxIsReplacingPath ctx ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced", "outline-replace" ]

    else if ctx.replacing == Nothing then
        [ "ast-replaceable", "hover-outline-replace" ]

    else
        [ "ast-replaceable" ]


replaceButton : Path -> String -> Html Msg -> Html Msg
replaceButton path className astHtml =
    button
        [ class ("ast-button " ++ className)
        , onClick (InitiateReplace path "")
        ]
        [ astHtml ]


replaceInput : IterationContext -> String -> String -> Html Msg
replaceInput { path, replacing } className contents =
    let
        replacingMeWith =
            replacing
                |> Maybe.andThen
                    (\r ->
                        if r.path == path then
                            Just r

                        else
                            Nothing
                    )

        inputLength =
            [ replacingMeWith
                |> Maybe.map .search
                |> Maybe.withDefault ""
            , contents
            , "_"
            ]
                |> List.foldr (\s acc -> Basics.max acc (String.length s)) 0

        styles =
            [ class ("ast-input margin-y-form-child " ++ className)
            , style "width" ((inputLength |> String.fromInt) ++ "ch")
            ]
    in
    case replacingMeWith of
        Just { search } ->
            input
                (styles
                    ++ [ onInput (InitiateReplace path)
                       , if search == "" then
                            placeholder contents

                         else
                            value search
                       ]
                )
                []

        Nothing ->
            input
                (styles
                    ++ [ onClick (InitiateReplace path "")
                       , onInput (InitiateReplace path)
                       , value contents
                       ]
                )
                []


addStatementButton : IterationContext -> Bool -> Html Msg
addStatementButton ctx isLast =
    if ctxIsAddingPath ctx ctx.path then
        input
            [ class "ast-input ast-add-statement-input"
            , class "ast-add-statement-input--adding"
            , class "outline-replace"
            , placeholder "Add statement"
            ]
            []

    else if isLast then
        button
            [ class "ast-button ast-add-statement-input"
            , class "ast-add-statement-input--clickable-last"
            , onClick (InitiateAdd ctx.path "")
            ]
            [ text "+" ]

    else
        button
            [ class "ast-button ast-add-statement-input"
            , class "ast-add-statement-input--clickable"
            , onClick (InitiateAdd ctx.path "")
            ]
            [ span [] [] ]


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
