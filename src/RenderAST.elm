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


renderClickableVarName : IterationContext -> String -> Html Msg
renderClickableVarName ctx varName =
    let
        attributes =
            [ class "ast-button ast-var-name"
            , onClick (CommitChange (Number { value = 9999 }))
            ]
    in
    button attributes [ text varName ]



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


type alias PrependedRenderer =
    IterationContext -> List (Html Msg)


prependNothing : PrependedRenderer
prependNothing _ =
    []


mapWithAddButtons : IterationContext -> List AST -> PrependedRenderer -> List (Html Msg)
mapWithAddButtons ctx astList prepend =
    let
        childCtx =
            ctxEnterPath ctx

        renderWithButton =
            \index child ->
                addButton (childCtx index)
                    :: prepend (childCtx index)
                    ++ [ renderEditor (childCtx index) child ]

        mapped =
            List.indexedMap renderWithButton astList
                |> List.foldr (++) []

        addButtonAtEnd =
            addButton (childCtx (List.length astList))
    in
    mapped ++ [ addButtonAtEnd ]



--- Bringing it all together


renderEditor : IterationContext -> AST -> Html Msg
renderEditor ctx ast =
    let
        classListFor =
            case ast of
                Program _ ->
                    [ "ast-program", "layout-vertical" ]

                Form _ ->
                    [ "ast-form", "layout-horizontal" ]

                Number _ ->
                    [ "ast-number" ]

        className =
            classListFor
                ++ beingReplacedClasses ctx
                |> String.join " "
    in
    case ast of
        Program { expressions } ->
            let
                prependVarName =
                    \childCtx -> [ renderClickableVarName childCtx "number1" ]
            in
            div [ class className ] (mapWithAddButtons ctx expressions prependVarName)

        Form { head, tail } ->
            div [ class className ]
                (renderForm ctx.path
                    (replaceButton ctx.path "" (renderPunctuation head)
                        :: mapWithAddButtons ctx tail prependNothing
                    )
                )

        Number { value } ->
            replaceButton ctx.path className (text (String.fromInt value))
