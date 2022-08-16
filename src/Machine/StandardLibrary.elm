module Machine.StandardLibrary exposing (..)


type FunctionDisplayType
    = DispOperator
    | DispIf


type alias StandardLibraryFunction =
    { name : String
    , parameters : List String
    , display : FunctionDisplayType
    }


standardLibraryFunctions : List StandardLibraryFunction
standardLibraryFunctions =
    [ { display = DispOperator, name = "+", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "-", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "×", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "÷", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "==", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "≠", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "<", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = ">", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "≤", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "≥", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "And", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "Or", parameters = [ "Left", "Right" ] }
    , { display = DispOperator, name = "Not", parameters = [ "Left" ] }
    , { display = DispIf, name = "If", parameters = [ "Condition", "True", "False" ] }
    ]


getStandardLibFunction : String -> Maybe StandardLibraryFunction
getStandardLibFunction name =
    List.filter (\x -> x.name == name) standardLibraryFunctions
        |> List.head
