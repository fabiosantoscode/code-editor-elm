module IterationContext exposing (..)

type alias Path =
    List Int


type alias Replacement =
    Maybe { path : Path, search : String, addition : Bool }


type alias IterationContext =
    { path : Path
    , replacing : Replacement
    }



ctxAppendPath : IterationContext -> Int -> IterationContext
ctxAppendPath context index =
    { context | path = context.path ++ [ index ] }

ctxReplacingPath : IterationContext -> Maybe Path
ctxReplacingPath context =
    context.replacing |> Maybe.andThen 
      (\r -> if r.addition == False then Just r.path else Nothing)

ctxAddingPath : IterationContext -> Maybe Path
ctxAddingPath context =
    context.replacing |> Maybe.andThen
      (\r -> if r.addition then Just r.path else Nothing)
