module Main exposing (main)

import AST exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Machine.StandardLibrary exposing (StandardLibraryFunction, standardLibraryFunctions)
import Model exposing (..)
import RenderEditor exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = applyMsg
        , view = renderModel
        }


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


renderFunctionButtons : List StandardLibraryFunction -> Html Msg
renderFunctionButtons functions =
    Html.div
        [ class "replace-function-buttons"
        ]
        (List.map renderFunctionButton functions)


renderReplaceUi : Model -> List (Html Msg)
renderReplaceUi { replacing } =
    case replacing of
        Nothing ->
            []

        Just _ ->
            [ div [ class "ast-replace-ui" ]
                [ renderFunctionButtons standardLibraryFunctions ]
            ]


renderModel : Model -> Html Msg
renderModel model =
    div []
        (node "link" [ rel "stylesheet", href "/assets/styles.css" ] []
            :: renderEditor
                model
                { path = [], replacing = model.replacing }
                model.program
            ++ renderReplaceUi model
        )
