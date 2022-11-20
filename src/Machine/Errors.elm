module Machine.Errors exposing (..)


type alias Path =
    List Int


type RunError
    = MissingFunc Path String
    | MissingVar Path String
    | IncorrectArgCount Path Int Int
    | InternalPanic Path String
    | DivisionByZero Path
    | NoResult Path


type alias MachineResult =
    Result RunError Int


formatError : RunError -> String
formatError err =
    case err of
        MissingFunc _ fname ->
            "Missing function " ++ fname

        MissingVar _ varname ->
            "Missing variable " ++ varname

        IncorrectArgCount _ actual expected ->
            "Expected " ++ String.fromInt expected ++ " parameters, but received " ++ String.fromInt actual

        DivisionByZero _ -> "Division by zero"

        InternalPanic _ panic ->
            "FATAL ERROR " ++ panic

        NoResult _ ->
            "no data"


isNoResult : RunError -> Bool
isNoResult err =
    case err of
        NoResult _ ->
            True

        _ ->
            False


getErrorPath : RunError -> Path
getErrorPath err =
    case err of
        MissingFunc path _ ->
            path

        MissingVar path _ ->
            path

        IncorrectArgCount path _ _ ->
            path

        DivisionByZero path ->
            path

        InternalPanic path _ ->
            path

        NoResult path ->
            path
