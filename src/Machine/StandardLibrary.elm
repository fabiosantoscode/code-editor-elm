module Machine.StandardLibrary exposing (..)


type FunctionDisplayType
    = BinOp
    | Prefix


type alias StandardLibraryFunction =
    { name : String
    , parameters : List String
    , display : FunctionDisplayType
    }


standardLibraryFunctions : List StandardLibraryFunction
standardLibraryFunctions =
    [ { display = BinOp, name = "+", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "-", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "*", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "/", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Equals", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Differs", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Less Than", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Greater Than", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Less Than Or Equal To", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Greater Than Or Equal To", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "And", parameters = [ "Left", "Right" ] }
    , { display = BinOp, name = "Or", parameters = [ "Left", "Right" ] }
    , { display = Prefix, name = "Not", parameters = [ "Left" ] }
    , { display = Prefix, name = "If", parameters = [ "Condition", "True", "False" ] }
    ]
