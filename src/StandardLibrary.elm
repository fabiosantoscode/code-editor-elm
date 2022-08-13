module StandardLibrary exposing (..)


type alias StandardLibraryFunction =
    { name : String
    , parameters : List String
    }


standardLibraryFunctions : List StandardLibraryFunction
standardLibraryFunctions =
    [ { name = "+", parameters = [ "Left", "Right" ] }
    , { name = "-", parameters = [ "Left", "Right" ] }
    , { name = "*", parameters = [ "Left", "Right" ] }
    , { name = "/", parameters = [ "Left", "Right" ] }
    , { name = "Equals", parameters = [ "Left", "Right" ] }
    , { name = "Differs", parameters = [ "Left", "Right" ] }
    , { name = "Less Than", parameters = [ "Left", "Right" ] }
    , { name = "Greater Than", parameters = [ "Left", "Right" ] }
    , { name = "Less Than Or Equal To", parameters = [ "Left", "Right" ] }
    , { name = "Greater Than Or Equal To", parameters = [ "Left", "Right" ] }
    , { name = "And", parameters = [ "Left", "Right" ] }
    , { name = "Or", parameters = [ "Left", "Right" ] }
    , { name = "Not", parameters = [ "Left" ] }
    , { name = "If", parameters = [ "Condition", "True", "False" ] }
    ]
