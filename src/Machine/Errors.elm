module Machine.Errors exposing (..)


type RunError
    = MissingFunc String
    | MissingVar String
    | IncorrectArgCount Int Int
    | InternalPanic String
    | ReferencedError
    | Incomplete


formatError : RunError -> String
formatError err =
    case err of
        MissingFunc fname ->
            "Missing function " ++ fname

        MissingVar varname ->
            "Missing variable " ++ varname

        IncorrectArgCount actual expected ->
            "Expected " ++ (String.fromInt expected) ++ " parameters, but received " ++ (String.fromInt actual)

        InternalPanic panic ->
            "FATAL ERROR " ++ panic

        Incomplete ->
            ""

        ReferencedError ->
            ""
