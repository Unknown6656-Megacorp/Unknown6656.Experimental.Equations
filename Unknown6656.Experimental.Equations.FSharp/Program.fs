open System

open Equation


[<EntryPoint>]
let main argv =
    [
        [Const 1.0; Const -1.0]
        [Const 1.0; Const 2.0; Const -1.0]
        [Const 1.0; Const 2.0; Const 3.0; Const -1.0]
        [Const 1.0; Const 2.0; Const 3.0; Const -2.0]
        [Const 1.0; Const 2.0; Const 3.0; Const -2.0; Const -1.0]
    ]
    |> List.map (fun l ->
        let l' = process_pairwise (fun a b -> match a,b with | Const x, Const y when x + y = 0.0 -> Some (Const 0.0) | _ -> None) l
        printfn "%A --> %A" l l'
        l'
    )
    |> List.map (fun l ->
        let l' = process_pairwise (fun a b -> match a,b with | Const x, Const y when x + y = 0.0 -> Some (Const 0.0) | _ -> None) l
        printfn "%A --> %A" l l'
        l'
    )
    |> ignore


    let e1 = Sum[Const 3.0; Variable; Sum[Const 5.0; Negation Variable; Reciprocal Variable]]
    let e2 = e1.Fold
    let res = e2.EvaluateAt 5.0
    let x = e2.SolveFor res



    0


    