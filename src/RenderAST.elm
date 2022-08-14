module RenderAST exposing (..)
import AST exposing (..)
import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


beingReplacedClasses : IterationContext -> List String
beingReplacedClasses ctx =
    if ctxReplacingPath ctx == Just ctx.path then
        [ "ast-replaceable", "ast-replaceable--being-replaced" ]

    else
        [ "ast-replaceable" ]



mapWithAddButtons : IterationContext -> (Int -> AST -> Html Msg) -> List AST -> List (Html Msg)
mapWithAddButtons ctx renderer astList =
    let
        conditonalAddButton =
            \index ->
                let
                    innerCtx =
                        ctxAppendPath ctx index
                in
                addButton innerCtx.path (ctxAddingPath innerCtx == Just innerCtx.path)

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


addButton : Path -> Bool -> Html Msg
addButton path adding =
    let
        className =
            if adding then
                "ast-add-target"

            else
                "ast-add-button"
    in
    button
        [ class className
        , onClick (InitiateAdd path "")
        ]
        [ span [] [] ]



--- Rendering AST nodes


renderPunctuation : String -> Html Msg
renderPunctuation symbol =
    span [ class "ast-punctuation" ] [ text symbol ]


renderForm : Path -> List (Html Msg) -> List (Html Msg)
renderForm path inner =
    let
        punc =
            \character ->
                button [ class "ast-punctuation", onClick (InitiateReplace path "") ] [ text character ]
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
            \i child -> renderAst (ctxAppendPath ctx i) child

        recurseChildren =
            mapWithAddButtons ctx recurse
    in
    case ast of
        Program { expressions } ->
            div [ class className ] (recurseChildren expressions)

        Form { head, tail } ->
            div [ class className ]
                (renderForm ctx.path
                    (replaceButton ctx.path "" (renderPunctuation head)
                        :: recurseChildren tail
                    )
                )

        Number { value } ->
            replaceButton ctx.path className (text (String.fromInt value))
