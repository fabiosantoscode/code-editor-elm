module RenderAST exposing (..)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)



--- Rendering replacer machinery


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


varNameIsClickable : Model -> IterationContext -> String -> Bool
varNameIsClickable model ctx varName =
    case model.replacing of
        Nothing ->
            False

        Just r ->
            isVariableAvailable model.varNames r.path varName



-- TODO: Only clickable if ctx.path is after its definition


renderClickableVarName : Model -> IterationContext -> String -> Html Msg
renderClickableVarName model ctx varName =
    let
        attributes =
            if varNameIsClickable model ctx varName then
                [ class "ast-button ast-var-name ast-var-name--clickable"
                , onClick (CommitChange (Reference {name = varName}))
                ]

            else
                [ class "ast-button ast-var-name" ]
    in
    button attributes [ text (varName ++ ":") ]



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


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxCurrentReplacePath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]


type alias Renderer =
    IterationContext -> AST -> List (Html Msg)


mapWithAddButtons : Renderer -> IterationContext -> List AST -> List (Html Msg)
mapWithAddButtons renderChild ctx astList =
    let
        childCtx =
            ctxEnterPath ctx

        renderWithButton =
            \index child ->
                addButton (childCtx index)
                    :: renderChild (childCtx index) child

        mapped =
            List.indexedMap renderWithButton astList
                |> List.foldr (++) []

        addButtonAtEnd =
            addButton (childCtx (List.length astList))
    in
    mapped ++ [ addButtonAtEnd ]



--- Bringing it all together


renderEditor : Model -> Renderer
renderEditor model ctx ast =
    let
        classListFor =
            case ast of
                Program _ ->
                    [ "ast-program", "layout-vertical" ]

                Form _ ->
                    [ "ast-form", "layout-horizontal" ]

                Number _ ->
                    [ "ast-number" ]

                Reference _ ->
                    [ "ast-reference" ]

        className =
            classListFor
                ++ beingReplacedClasses ctx
                |> String.join " "

        childRender =
            renderEditor model
    in
    case ast of
        Program { expressions } ->
            let
                renderWithVarName =
                    \childCtx childAst ->
                        renderClickableVarName model childCtx (getNthVarName model.varNames childCtx.path)
                            :: childRender childCtx childAst
            in
            [ div [ class className ] (mapWithAddButtons renderWithVarName ctx expressions) ]

        Form { head, tail } ->
            [ div [ class className ]
                (renderForm ctx.path
                    (replaceButton ctx.path "" (renderPunctuation head)
                        :: mapWithAddButtons childRender ctx tail
                    )
                )
            ]

        Number { value } ->
            [ replaceButton ctx.path className (text (String.fromInt value)) ]

        Reference { name } ->
            [ replaceButton ctx.path className (text name) ]
