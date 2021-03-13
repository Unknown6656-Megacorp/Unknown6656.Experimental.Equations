open System

open Equation


[<EntryPoint>]
let main argv =
    let e1 = Sum[Const 3.0; Variable; Sum[Const 5.0; Negation Variable; Reciprocal Variable]]
    let e2 = e1.Fold
    let res = e2.EvaluateAt 5.0
    let x = e2.SolveFor res



    0


    