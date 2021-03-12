module Equation


let inline (||>>) (f, g) (x, y) = (f x, g y)

let inline (?) c a b = if c then a else b

let separate (selector : 'a -> Choice<'b, 'c>) =
    let rec intern acc = function
                         | [] -> acc
                         | x::xs ->
                             let acc = match selector x with
                                       | Choice1Of2 a -> a::(fst acc), snd acc
                                       | Choice2Of2 b -> fst acc, b::(snd acc)
                             intern acc xs
    intern ([], [])


type Solution =
    | None
    | Some of float list
    | All

type Expression =
    | Variable
    | Const of float
    | Sum of Expression list
    | Product of Expression list
    // | Difference of Expression * Expression
    // | Quotient of Expression * Expression
    // | Power of Expression * Expression
    with
        override this.ToString() =
            match this with
            | Variable -> "x"
            | Const x -> x.ToString()
            | Sum xs -> xs
                        |> List.map (fun x -> x.ToString())
                        |> String.concat " + "
                        |> sprintf "(%s)"
            | Product xs -> xs
                            |> List.map (fun x -> x.ToString())
                            |> String.concat " * "
                            |> sprintf "(%s)"

        member x.IsConstant =
            match x with
            | Variable -> false
            | Const _
            | Sum [] | Product [] -> true
            | Sum xs | Product xs -> List.forall (fun (x : Expression) -> x.IsConstant) xs

        member x.Fold =
            let try_fold op expr initial xs =
                let c, v = xs
                            |> separate (fun (x : Expression) ->
                                match x.Fold with
                                | Const c -> Choice1Of2 c
                                | _ -> Choice2Of2 x
                            )
                            ||> (List.fold op initial >> Const, fun vars -> vars)


                // match a.Fold, b.Fold with
                // | Const a', Const b' -> Const (op a' b')
                // | t -> expr t
                
            let original = x
            match x with
            | Variable
            | Const _ -> x
            | Sum xs -> try_fold (+) Sum xs
            | Product xs -> try_fold (*) Product xs
            |> function


            | Sum (a, b) when a = b -> Product (Const 2.0, a)

            | Sum (Const a, Sum (Const b, c))
            | Sum (Const a, Sum (c, Const b))
            | Sum (Sum (Const b, c), Const a)
            | Sum (Sum (c, Const b), Const a) -> Sum(Const(a + b), c)
            

            | Sum (a, Sum (b, c)) when a = b -> Product(Const 2.0 )
            | Sum (a, Sum (c, b)) when a = b -> 



            | Product (Const a, Product (Const b, c))
            | Product (Const a, Product (c, Const b))
            | Product (Product (Const b, c), Const a)
            | Product (Product (c, Const b), Const a) -> Product(Const(a * b), c)

            | a -> a
            |> fun a -> if a = original then a else a.Fold

        member this.EvaluateAt x =
            match this with
            | Variable -> x
            | Const c -> c
            | Sum (a, b) -> (a.EvaluateAt x) + (b.EvaluateAt x)
            | Product (a, b) -> (a.EvaluateAt x) * (b.EvaluateAt x)

        member x.SolveFor result =
            match x.Fold with
            | Variable -> Some [result]
            | Const c when c = result -> All
            | Const _ -> None
            | Sum (Const a, b) -> b.SolveFor (result - a)
            | Sum (a, Const b) -> a.SolveFor (result - b)
            | Product (Const a, b) -> b.SolveFor (result / a)
            | Product (a, Const b) -> a.SolveFor (result / b)
            | a -> failwithf "TODO: %O" a
