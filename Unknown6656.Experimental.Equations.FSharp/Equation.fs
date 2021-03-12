module Equation


type Solution =
    | None
    | Some of float list
    | All

type Expression =
    | Variable
    | Const of float
    | Sum of  Expression * Expression
    with
        member x.IsConstant =
            match x with
            | Variable -> false
            | Const _ -> true
            | Sum (a, b) -> a.IsConstant && b.IsConstant

        member this.EvaluateAt x =
            match this with
            | Variable -> x
            | Const c -> c
            | Sum (a, b) -> (a.EvaluateAt x) + (b.EvaluateAt x)

        member x.SolveFor result =
            match x with
            | Variable -> Some [result]
            | Const c when c = result -> All
            | Const _ -> None
            | Sum (a, b) -> failwith "<todo>"