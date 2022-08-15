module RenderAST exposing (..)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Utils exposing (flatMap)



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


renderForm : Path -> String -> List (Html Msg) -> List (Html Msg)
renderForm path head tail =
    let
        punc =
            \character ->
                button [ class "ast-button ast-punctuation", onClick (InitiateReplace path "") ] [ text character ]
    in
    replaceButton path "ast-form__head" (punc (head ++ "(")) :: tail ++ [ punc ")" ]


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxCurrentReplacePath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]


type alias Renderer =
    IterationContext -> AST -> List (Html Msg)


mapWithAddButtons : IterationContext -> List a -> (Int -> a -> List (Html Msg)) -> List (Html Msg)
mapWithAddButtons ctx astList renderChild =
    let
        renderWithButton =
            \index child ->
                addButton (ctxEnterPath ctx index)
                    :: renderChild index child

        mapped =
            List.indexedMap renderWithButton astList
                |> List.foldr (++) []

        addButtonAtEnd =
            addButton (ctxEnterPath ctx (List.length astList))
    in
    mapped ++ [ addButtonAtEnd ]



--- Bringing it all together


renderEditor : Model -> Renderer
renderEditor model ctx ast =
    let
        classListFor =
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

        className =
            classListFor
                ++ beingReplacedClasses ctx
                |> String.join " "

        childRender =
            \index child ->
                renderEditor model (ctxEnterPath ctx index) child
    in
    case ast of
        Block p ->
            let
                varNames =
                    p.assignments |> List.map (\a -> a.name)

                renderWithVarName =
                    \index { expression, name } ->
                        renderClickableVarName model varNames name
                            :: childRender index expression
            in
            [ div
                [ class className ]
                (mapWithAddButtons ctx p.assignments renderWithVarName)
            ]

        Form { head, tail } ->
            [ div [ class className, class ("ast-form--depth-" ++ String.fromInt (List.length ctx.path)) ]
                (renderForm ctx.path
                    head
                    (flatMap childRender tail)
                )
            ]

        Number { value } ->
            [ replaceButton ctx.path className (text (String.fromInt value)) ]

        Reference { name } ->
            [ replaceButton ctx.path className (text name) ]

        Incomplete ->
            [ replaceButton ctx.path className (text "") ]
