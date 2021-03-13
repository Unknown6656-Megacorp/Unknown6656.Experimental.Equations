open System

open Equation


[<EntryPoint>]
let main argv =
    let e1 = Sum[
        Const 3.0
        Variable
        Product[
            Variable
            Const 2.0
        ]
        Sum[
            Const 5.0
            Negation Variable
            Reciprocal Variable
            Const 0.0
            Product[
                Variable
                Const 1.0
            ]
            Product[
                Variable
                Const 0.0
            ]
        ]
    ]
    let e2 = e1.Fold
    let x = 5.0
    let res = e2.EvaluateAt x

    printfn "\n\nf(x) = %O\n     = %O\nf(%f) = %f" e1 e2 x res

    let x' = e2.SolveFor res
    
    printfn "L(f|%f) = %O" res x'
    0


    