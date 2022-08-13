module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, button, div, input, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import IterationContext exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type AST
    = Program { expressions : List AST }
    | Form { head : String, tail : List AST }
    | Number { value : Int }


type alias Model =
    { program : AST
    , replacing : Replacement
    }


init : Model
init =
    { program =
        Program
            { expressions =
                [ Form
                    { head = "+"
                    , tail =
                        [ Number { value = 1 }
                        , Number { value = 2 }
                        , Number { value = 3 }
                        ]
                    }
                , Form
                    { head = "+"
                    , tail =
                        [ Number { value = 1 }
                        , Number { value = 2 }
                        , Number { value = 3 }
                        ]
                    }
                ]
            }
    , replacing =
        Just
            { path = [ 0, 2 ]
            , search = ""
            , addition = True
            }
    }



-- UPDATE


type Msg
    = InitiateAdd Path String
    | InitiateReplace Path String


getAstChildren : AST -> List AST
getAstChildren ast =
    case ast of
        Program p ->
            p.expressions

        Form f ->
            f.tail

        _ ->
            []


setAstChildren : AST -> List AST -> AST
setAstChildren ast newChildren =
    case ast of
        Program p ->
            Program { p | expressions = newChildren }

        Form f ->
            Form { f | tail = newChildren }

        _ ->
            ast


mutateNthChild : Int -> (AST -> List AST) -> AST -> AST
mutateNthChild index mutator ast =
    let
        currentChildren =
            getAstChildren ast

        indexedMutator =
            \hereIndex child ->
                if hereIndex == index then
                    mutator child

                else
                    [ child ]

        mutList =
            if index == List.length currentChildren then
                currentChildren
                    ++ indexedMutator index
                        -- TODO: this is a hack
                        (Number { value = 66666 })

            else
                List.indexedMap indexedMutator currentChildren
                    |> List.foldr (++) []
    in
    setAstChildren ast mutList


mutateTargetChild : Path -> (AST -> List AST) -> AST -> AST
mutateTargetChild path transformer ast =
    case path of
        [] ->
            ast

        [ hereIndex ] ->
            mutateNthChild hereIndex transformer ast

        midIndex :: rest ->
            mutateNthChild midIndex
                (\a -> [ mutateTargetChild rest transformer a ])
                ast


update : Msg -> Model -> Model
update msg model =
    case msg of
        InitiateAdd path search ->
            { model | replacing = Just { path = path, search = search, addition = True } }

        InitiateReplace path search ->
            { model
                | replacing = Just { path = path, search = search, addition = False }
            }



-- VIEW


renderPunctuation : String -> Html Msg
renderPunctuation symbol =
    span [ class "ast-punctuation" ] [ text symbol ]


renderFormParens : List (Html Msg) -> Html Msg
renderFormParens inner =
    div [ class "ast-form-parens" ]
        (renderPunctuation "(" :: inner ++ [ renderPunctuation ")" ])


mapWithAddButtons : IterationContext -> (Int -> AST -> Html Msg) -> List AST -> List (Html Msg)
mapWithAddButtons ctx renderer astList =
    let
        renderWithButton =
            \index astChild ->
                if ctxReplacingPath ctx == Just (ctx.path ++ [ index ]) then
                    [ addButton ctx.path index ]
                        ++ [ text "replace me UwU" ]

                else if ctxAddingPath ctx == Just (ctx.path ++ [ index ]) then
                    [ text "adding UwU" ]
                        ++ [ renderer index astChild ]

                else
                    [ addButton ctx.path index ]
                        ++ [ renderer index astChild ]

        listLength = List.length astList

        renderWithLastAddButton =
            \index astChild ->
                let
                    isLast =
                        index == listLength - 1
                in
                if isLast then
                    (renderWithButton index astChild)
                        ++ (
                            if ctxAddingPath ctx == Just (ctx.path ++ [ listLength ]) then
                                [ text "adding UwU" ]
                            else
                                [ addButton ctx.path listLength ]
                        )

                else
                    renderWithButton index astChild
    in
    List.indexedMap renderWithLastAddButton astList
        |> List.foldr (++) [  ]


wrapBeingReplaced : IterationContext -> Html Msg -> Html Msg
wrapBeingReplaced ctx ast =
    if ctxReplacingPath ctx == Just ctx.path then
        div [ class "ast-replaceable ast-replaceable--being-replaced" ] [ ast ]

    else
        div [ class "ast-replaceable" ] [ ast ]


replaceButton : Path -> Html Msg -> Html Msg
replaceButton path astHtml =
    button
        [ class "ast-replaceable-button"
        , onClick (InitiateReplace path "")
        ]
        [ astHtml ]


addButton : Path -> Int -> Html Msg
addButton path index =
    button
        [ class ""
        , onClick (InitiateAdd (path ++ [ index ]) "")
        ]
        [ text "+" ]


renderAst : IterationContext -> AST -> Html Msg
renderAst ctx ast =
    let
        recurse =
            \i -> renderAst (ctxAppendPath ctx i)

        recurseChildren =
            mapWithAddButtons ctx recurse

        replaceWrap =
            wrapBeingReplaced ctx

        asReplaceButton =
            replaceButton ctx.path

        plainHtml =
            case ast of
                Program { expressions } ->
                    div [ class "ast-program" ]
                        (recurseChildren expressions)

                Form { head, tail } ->
                    replaceWrap
                        (div [ class "ast-form" ]
                            [ renderFormParens
                                (asReplaceButton
                                    (renderPunctuation head)
                                    :: recurseChildren tail
                                )
                            ]
                        )

                Number { value } ->
                    replaceWrap (asReplaceButton (div [] [ text (String.fromInt value) ]))
    in
    plainHtml


renderReplaceUi : Model -> List (Html Msg)
renderReplaceUi { replacing } =
    case replacing of
        Nothing ->
            []

        Just { path, search } ->
            [ div [ class "ast-replace-ui" ]
                [ text "Replace"
                , div [ class "ast-replace-path" ]
                    [ text (String.join "." (List.map String.fromInt path)) ]
                , div [ class "ast-replace-search" ]
                    [ input
                        [ class "ast-replace-search-input"
                        , value search
                        , onInput (InitiateReplace path)
                        ]
                        []
                    ]
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        ([ node "link" [ rel "stylesheet", href "/assets/styles.css" ] []
         , renderAst
            { path = [], replacing = model.replacing }
            model.program
         ]
            ++ renderReplaceUi model
        )
