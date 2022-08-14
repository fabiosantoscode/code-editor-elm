module Main exposing (main)

import AST exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import RenderAST exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = applyMsg
        , view = renderModel
        }


renderReplaceUi : Model -> List (Html Msg)
renderReplaceUi { replacing } =
    case replacing of
        Nothing ->
            []

        Just { path, search, addition } ->
            let
                replaceText =
                    if addition then
                        "Add"

                    else
                        "Replace"
            in
            [ div [ class "ast-replace-ui" ]
                [ text replaceText
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


renderModel : Model -> Html Msg
renderModel model =
    div []
        ([ node "link" [ rel "stylesheet", href "/assets/styles.css" ] []
         , renderEditor
            { path = [], replacing = model.replacing }
            model.program
         ]
            ++ renderReplaceUi model
        )
