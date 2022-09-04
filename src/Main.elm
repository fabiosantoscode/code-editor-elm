module Main exposing (main)

import AST exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import RenderEditor exposing (renderEditor)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = updateModel
        , subscriptions = \_ -> Sub.none
        , view = renderModel
        }


renderModel : Model -> Html Msg
renderModel model =
    div []
        (node "link" [ rel "stylesheet", href "/assets/styles.css" ] []
            :: renderEditor
                model
                { path = [], replacing = model.replacing }
                model.program
        )
