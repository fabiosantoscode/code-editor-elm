module RenderSuggestions exposing (..)

import AST exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Machine.Parse exposing (ParseResult(..), tryParseAtom)
import Machine.StandardLibrary exposing (StandardLibraryFunction, standardLibraryFunctions)
import Model exposing (..)


renderSuggestions : Model -> IterationContext -> Bool -> List (Html Msg) -> List (Html Msg)
renderSuggestions model ctx isAddBtn statHtml =
    let
        headPath =
            \{ path } -> List.head path

        maybeUi =
            model.replacing
                |> Maybe.map
                    (\r ->
                        if r.addition == isAddBtn && headPath ctx == headPath r then
                            [ replaceUi r ]

                        else
                            []
                    )
                |> Maybe.withDefault []
    in
    [ div [ class "ast-suggestions__container" ]
        (div [ class "ast-suggestions__statement" ] statHtml
            :: maybeUi
        )
    ]


replaceUi : Replacement -> Html Msg
replaceUi r =
    div
        [ class "ast-suggestions__wrapper" ]
        [ div [ class "ast-suggestions__root" ]
            (renderReplaceBox r
                ++ [ renderFunctionButtons standardLibraryFunctions ]
            )
        ]


renderReplaceBox : Replacement -> List (Html Msg)
renderReplaceBox r =
    [ input
        [ class "replace-box"
        , value r.search
        , onInput UpdateSearch
        , id (generateIdForAdd r.path)
        ]
        []
    , div []
        [ case tryParseAtom r.search of
            ParsedNumber _ ->
                div [ class "height-explain text-explain" ] [ text "Number" ]

            ParseError s ->
                div [ class "height-explain text-error" ] [ text s ]

            Empty ->
                div [ class "height-explain"] []
        ]
    ]


renderFunctionButtons : List StandardLibraryFunction -> Html Msg
renderFunctionButtons functions =
    Html.div
        [ class "replace-function-buttons"
        ]
        (List.map renderFunctionButton functions)


renderFunctionButton : StandardLibraryFunction -> Html Msg
renderFunctionButton f =
    let
        asForm =
            Form
                { head = f.name
                , tail = List.repeat (List.length f.parameters) Incomplete
                }
    in
    button
        [ onClick (CommitChange asForm)
        , class "replace-function-button"
        ]
        [ text f.name ]
