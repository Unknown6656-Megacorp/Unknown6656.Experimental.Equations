open System

open Equation


[<EntryPoint>]
let main argv =
    let e1 = Sum[
        Const 3.0
        Negation Variable
        Product[Variable; Variable; Const 2.0]
    ]
    let e2 = e1.Fold
    let x = 5.0
    let res = e2.EvaluateAt x

    printfn "\nf(x) = %O\n     = %O\nf(%f) = %f\n" e1 e2 x res

    let x' = e2.SolveFor res
    
    printfn "L(f|%f) = %O" res x'
    0


    