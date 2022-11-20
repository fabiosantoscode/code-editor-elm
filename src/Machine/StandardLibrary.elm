module Machine.StandardLibrary exposing (..)

import Machine.Errors exposing (..)


type FunctionDisplayType
    = DispOperator
    | DispIf


type alias StandardLibraryFunction =
    { name : String
    , parameters : List String
    , display : FunctionDisplayType
    , impl : Path -> List Int -> Result RunError Int
    }


impl2 : (Int -> Int -> Int) -> Path -> List Int -> Result RunError Int
impl2 fn path items =
    case items of
        [ a, b ] ->
            Ok (fn a b)

        _ ->
            Err (IncorrectArgCount path (List.length items) 2)


impl2r : (Path -> Int -> Int -> Result RunError Int) -> Path -> List Int -> Result RunError Int
impl2r fn path items =
    case items of
        [ a, b ] ->
            fn path a b

        _ ->
            Err (IncorrectArgCount path (List.length items) 2)


impl3 : (Int -> Int -> Int -> Int) -> Path -> List Int -> Result RunError Int
impl3 fn path items =
    case items of
        [ a, b, c ] ->
            Ok (fn a b c)

        _ ->
            Err (IncorrectArgCount path (List.length items) 3)


standardLibraryFunctions : List StandardLibraryFunction
standardLibraryFunctions =
    [ { display = DispOperator
      , name = "+"
      , parameters = [ "Left", "Right" ]
      , impl = impl2 (\a b -> a + b)
      }
    , { display = DispOperator
      , name = "÷"
      , parameters = [ "Left", "Right" ]
      , impl =
            impl2r
                (\path a b ->
                    if b == 0 then
                        Err (DivisionByZero path)

                    else
                        Ok (a // b)
                )
      }

    --, { display = DispOperator, name = "-", parameters = [ "Left", "Right" ] }
    -- , { display = DispOperator, name = "×", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = "÷", parameters = [ "Left", "Right" ] }
    , { display = DispOperator
      , name = "=="
      , parameters = [ "Left", "Right" ]
      , impl =
            impl2
                (\a b ->
                    if a == b then
                        1

                    else
                        0
                )
      }

    --, { display = DispOperator, name = "≠", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = "<", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = ">", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = "≤", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = "≥", parameters = [ "Left", "Right" ] }
    , { display = DispOperator
      , name = "And"
      , parameters = [ "Left", "Right" ]
      , impl =
            impl2
                (\a b ->
                    case ( a /= 0, b /= 0 ) of
                        ( True, True ) ->
                            b

                        _ ->
                            a
                )
      }

    --, { display = DispOperator, name = "Or", parameters = [ "Left", "Right" ] }
    --, { display = DispOperator, name = "Not", parameters = [ "Left" ] }
    , { display = DispIf
      , name = "If"
      , parameters = [ "Condition", "True", "False" ]
      , impl =
            impl3
                (\ifPart thenPart elsePart ->
                    if ifPart /= 0 then
                        thenPart

                    else
                        elsePart
                )
      }
    ]


getStandardLibFunction : String -> Maybe StandardLibraryFunction
getStandardLibFunction name =
    List.filter (\x -> x.name == name) standardLibraryFunctions
        |> List.head


getFunctionDisplayType : String -> Maybe FunctionDisplayType
getFunctionDisplayType name =
    getStandardLibFunction name |> Maybe.map (\fn -> fn.display)
