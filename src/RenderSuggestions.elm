module RenderSuggestions exposing (..)

import AST exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Machine.Parse exposing (ParseResult(..), tryParseAtom)
import Machine.Run exposing (gatherVariables)
import Machine.StandardLibrary exposing (StandardLibraryFunction, standardLibraryFunctions)
import Model exposing (..)
import Utils exposing (..)


renderSuggestions : Model -> Int -> List (Html Msg) -> List (Html Msg)
renderSuggestions model index statHtml =
    let
        maybeUi =
            case model.replacing of
                Just r ->
                    if List.head r.path == Just index then
                        [ replaceUi
                            r
                            (gatherVariables model.program index)
                        ]

                    else
                        []

                _ ->
                    []
    in
    [ div [ class "ast-suggestions__container" ]
        (div [ class "ast-suggestions__statement" ] statHtml
            :: maybeUi
        )
    ]


replaceUi : Replacement -> Dict String Int -> Html Msg
replaceUi r goodVars =
    div
        [ class "ast-suggestions__wrapper" ]
        [ div [ class "ast-suggestions__root" ]
            [ renderReplaceBox r
            , renderSuggestionButtonList
                (renderParsedNumber r
                    ++ getSearchSuggestions r goodVars
                )
            , renderFunctionButtons standardLibraryFunctions
            ]
        ]


renderReplaceBox : Replacement -> Html Msg
renderReplaceBox r =
    div []
        [ input
            [ class "replace-box"
            , value r.search
            , onInput UpdateSearch
            , id (generateIdForAdd r.path)
            ]
            []
        , text " "
        , button [] [ text "Place" ]
        ]


renderParsedNumber : Replacement -> List (Html Msg)
renderParsedNumber r =
    case tryParseAtom r.search of
        ParsedNumber value ->
            [ button
                [ class "height-explain text-explain"
                , onClick (CommitChange (AST.Number { value = value }))
                ]
                [ text ("Number " ++ String.fromInt value) ]
            ]

        _ ->
            []


getSearchSuggestions : Replacement -> Dict String Int -> List (Html Msg)
getSearchSuggestions r goodVars =
    let
        matches search var =
            search /= "" && String.contains (String.toLower search) (String.toLower var)

        renderSuggestion var value =
            button
                [ onClick (CommitChange (AST.Reference { name = var })) ]
                [ text (var ++ " (" ++ String.fromInt value ++ ")") ]
    in
    goodVars
        |> Dict.toList
        |> List.reverse
        |> flatMap
            (\_ ( var, value ) ->
                if matches r.search var then
                    [ renderSuggestion var value ]

                else
                    []
            )
        |> List.take 3


renderSuggestionButtonList : List (Html Msg) -> Html Msg
renderSuggestionButtonList xs =
    ul [] (List.map (\item -> li [] [ item ]) xs)


renderFunctionButtons : List StandardLibraryFunction -> Html Msg
renderFunctionButtons functions =
    div
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
