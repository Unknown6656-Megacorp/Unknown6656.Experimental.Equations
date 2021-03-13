module Equation


let inline (||>>) (f, g) (x, y) = (f x, g y)

let inline (?) c a b = if c then a else b

let inline (|Contains|_|) list item = if List.contains item list then Some item else None

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
    | Negation of Expression
    | Reciprocal of Expression
    | Sum of Set<Expression>
    | Product of Set<Expression>
    | Exponentiation of Expression * Expression
    with
        static member Difference a b = seq { a; Negation b }
                                       |> set
                                       |> Sum

        static member Division a b = seq { a; Reciprocal b }
                                     |> set
                                     |> Product

        override this.ToString() =
            match this with
            | Variable -> "x"
            | Const x -> x.ToString()
            | Negation x -> sprintf "-(%O)" x
            | Reciprocal x -> sprintf "1/(%O)" x
            | Sum xs -> xs
                        |> Seq.map (fun x -> x.ToString())
                        |> String.concat " + "
                        |> sprintf "(%s)"
            | Product xs -> xs
                            |> Seq.map (fun x -> x.ToString())
                            |> String.concat " * "
                            |> sprintf "(%s)"
            | Exponentiation (a, b) -> sprintf "(%O)^(%O)" a b

        member x.IsConstant =
            match x with
            | Variable -> false
            | Const _ -> true
            | Sum xs when xs.Count = 0 -> true
            | Product xs when xs.Count = 0 -> true
            | Sum xs | Product xs -> Seq.forall (fun (x : Expression) -> x.IsConstant) xs
            | Reciprocal x
            | Negation x -> x.IsConstant
            | Exponentiation(a, b) -> a.IsConstant && b.IsConstant
            
        member this.EvaluateAt x =
            match this with
            | Variable -> x
            | Const c -> c
            | Negation n -> -(n.EvaluateAt x)
            | Reciprocal r -> 1.0 / (r.EvaluateAt x)
            | Exponentiation (a, b) -> (a.EvaluateAt x) ** (b.EvaluateAt x)
            | Sum xs -> xs |> Seq.fold (fun sum elem -> sum + elem.EvaluateAt x) 0.0
            | Product xs -> xs |> Seq.fold (fun product elem -> product * elem.EvaluateAt x) 1.0

        member x.Fold =
            let separate = 
                Seq.toList >> separate (fun (x : Expression) ->
                                            match x.Fold with
                                            | Const c -> Choice1Of2 c
                                            | _ -> Choice2Of2 x
                                        )

            let rec fold_sum items =
                let consts, vars = separate items
                let vars = vars
                           |> List.collect (function Sum xs -> fold_sum xs | x -> [x])
                           |> List.groupBy id
                           |> List.map (fun (e, l) ->
                                            if l.Length > 1 then
                                                [Const (float l.Length); e]
                                                |> set
                                                |> Product
                                            else
                                                e)
                (consts
                |> List.fold (+) 0.0
                |> Const)
                :: vars
                
            let rec fold_product items =
                let consts, vars = separate items
                let vars = vars
                            |> List.collect (function Product xs -> fold_product xs | x -> [x])
                            |> List.groupBy id
                            |> List.map (fun (e, l) -> if l.Length > 1 then Exponentiation (e, Const (float l.Length)) else e)
                (consts
                |> List.fold (*) 1.0
                |> Const)
                :: vars

            let original = x
            let folded = match x with
                         | Variable
                         | Const _ -> x
                         | Reciprocal x -> Reciprocal x.Fold
                         | Negation x -> Negation x.Fold
                         | Sum xs -> (fold_sum >> set >> Sum) xs
                         | Product xs -> (fold_product >> set >> Product) xs
                         | Exponentiation (a, b) -> Exponentiation (a.Fold, b.Fold)
                         |> function
                         | Negation (Const x) -> Const -x
                         | Reciprocal (Const x) -> Const (1.0 / x)
                         | Negation (Negation x)
                         | Reciprocal (Reciprocal x)
                         | Exponentiation (x, Const 1.0) -> x
                         | Exponentiation (_, Const 0.0)
                         | Exponentiation (Const 1.0, _) -> Const 1.0
                         | Exponentiation (Const 0.0, _) -> Const 0.0
                         | Exponentiation (x, Negation y) -> Reciprocal (Exponentiation (x, y))
                         | Exponentiation (x, Const y) when y < 0.0 -> Reciprocal (Exponentiation (x, Const -y))

                         | Sum xs ->
                            if xs.Count = 0 then Const 0.0
                            elif xs.Count = 1 then Seq.head xs
                            else
                                ()
                        
                         | Product xs ->
                            if xs.Count = 0 then Const 1.0
                            elif xs.Count = 1 then Seq.head xs
                            else
                                ()

                         
                         //| Sum xs ->
                         //   match xs with
                         //   | Contains (Sum s2) -> Const 0.0


                         | a -> a

            if folded = original then
                folded
            else
                folded.Fold

        member x.SolveFor result =
            match x.Fold with
            | Variable -> Some [result]
            | Const c when c = result -> All
            | Const _ -> None
            //| Sum (Const a, b) -> b.SolveFor (result - a)
            //| Sum (a, Const b) -> a.SolveFor (result - b)
            //| Product (Const a, b) -> b.SolveFor (result / a)
            //| Product (a, Const b) -> a.SolveFor (result / b)
            | a -> failwithf "TODO: %O" a
