module RenderSuggestions exposing (..)

import AST exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Machine.Parse exposing (ParseResult(..), tryParseAtom)
import Machine.Run
import Machine.StandardLibrary exposing (StandardLibraryFunction, standardLibraryFunctions)
import Model exposing (..)
import Set exposing (Set)
import Utils exposing (..)


renderSuggestions : Model -> Int -> List (Html Msg) -> List (Html Msg)
renderSuggestions model index statHtml =
    let
        maybeUi =
            case model.replacing of
                Just r ->
                    if List.head r.path == Just index then
                        [ replaceUi r
                            (Machine.Run.findAllVarNames model.program)
                            (Machine.Run.findGoodVariables model.program r.path)
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


replaceUi : Replacement -> Set String -> Dict String Int -> Html Msg
replaceUi r vars goodVars =
    div
        [ class "ast-suggestions__wrapper" ]
        [ div [ class "ast-suggestions__root flex-column" ]
            [ renderReplaceBox r
            , renderSuggestionButtonList
                (getSearchSuggestions r vars goodVars
                    ++ renderParsableSuggestion r
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


getSearchSuggestions : Replacement -> Set String -> Dict String Int -> List (Html Msg)
getSearchSuggestions r vars goodVars =
    let
        matches search var =
            search /= "" && String.contains (String.toLower search) (String.toLower var)

        renderSuggestion var value =
            let
                valuePreview =
                    (value |> Maybe.map (\v -> " (" ++ String.fromInt v ++ ")")) |> Maybe.withDefault ""
            in
            button
                [ onClick (CommitChange (AST.Reference { name = var })) ]
                [ text (var ++ valuePreview) ]
    in
    vars
        |> Set.toList
        |> List.reverse
        |> flatMap
            (\_ var ->
                if matches r.search var then
                    [ renderSuggestion var (Dict.get var goodVars) ]

                else
                    []
            )
        |> List.take 3


renderParsedNumber : Replacement -> List (Html Msg)
renderParsedNumber r =
    case tryParseAtom r.search of
        ParsedNumber value ->
            [ button
                [ class "height-explain text-explain"
                , onClick (CommitChange (AST.Number { value = value }))
                ]
                [ text ("Create the number " ++ String.fromInt value) ]
            ]

        _ ->
            []


renderParsableSuggestion : Replacement -> List (Html Msg)
renderParsableSuggestion r =
    case tryParseAtom r.search of
        ParsedNumber value ->
            [ button
                [ class "height-explain text-explain"
                , onClick (CommitChange (AST.Number { value = value }))
                ]
                [ text ("Create the number " ++ String.fromInt value) ]
            ]

        ParsedVariable name ->
            [ button
                [ onClick (ReplaceWithNewVariable r.path name) ]
                [ text ("Create the variable " ++ name) ]
            ]

        Empty ->
            []


renderSuggestionButtonList : List (Html Msg) -> Html Msg
renderSuggestionButtonList xs =
    ul [ class "margin-y-0 overflow-auto" ] (List.map (\item -> li [] [ item ]) xs)


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
