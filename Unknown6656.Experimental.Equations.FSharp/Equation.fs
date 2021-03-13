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

let rec pairs = function
                | []
                | [_] -> []
                | h::t -> [
                    for x in t -> h, x
                    yield! pairs t
                ]

// this could be optimized !
let rec process_pairwise func =
    let rec loop func head acc tail =
        match tail with
        | [] -> head::acc@tail
        | y::ys ->
            match func head y with
            | Some r -> r::acc@(loop_list ys)
            | None ->
            match func y head with
            | Some r -> r::acc@(loop_list ys)
            | None -> loop func head (acc@[y]) ys
    and loop_list list = match list with
                         | []
                         | [_] -> list
                         | y::ys -> 
                            match loop func y [] ys with
                            | x::xs -> x::loop_list xs
                            | l -> l
    loop_list

type Solution =
    | Empty
    | Values of float list
    | All

type Expression =
    | Variable
    | Const of float
    | Negation of Expression
    | Reciprocal of Expression
    | Sum of Expression list
    | Product of Expression list
    | Exponentiation of Expression * Expression
    with
        static member private sort a b =
            let order = function
                        | Const -> 0
                        | Variable _ -> 1
                        | Negation _ -> 2
                        | Reciprocal _ -> 3
                        | Sum _ -> 4
                        | Product _ -> 5
                        | Exponentiation _ -> 6
            order a

        static member Difference a b = Sum [a; Negation b]

        static member Division a b = Product [a; Reciprocal b]

        override this.ToString() =
            match this with
            | Variable -> "x"
            | Const x -> x.ToString()
            | Negation x -> sprintf "-(%A)" x
            | Reciprocal x -> sprintf "1/(%A)" x
            | Sum xs -> xs
                        |> Seq.map (fun x -> x.ToString())
                        |> String.concat " + "
                        |> sprintf "(%s)"
            | Product xs -> xs
                            |> Seq.map (fun x -> x.ToString())
                            |> String.concat " * "
                            |> sprintf "(%s)"
            | Exponentiation (a, b) -> sprintf "(%A)^(%A)" a b

        member x.IsConstant =
            match x with
            | Variable -> false
            | Const _
            | Sum [] | Product [] -> true
            | Sum xs | Product xs -> List.forall (fun (x : Expression) -> x.IsConstant) xs
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
            | Sum xs -> xs |> List.fold (fun sum elem -> sum + elem.EvaluateAt x) 0.0
            | Product xs -> xs |> List.fold (fun product elem -> product * elem.EvaluateAt x) 1.0

        member x.Fold =
            let separate = separate (fun (x : Expression) ->
                                        match x.Fold with
                                        | Const c -> Choice1Of2 c
                                        | _ -> Choice2Of2 x
                                    )

            let rec fold_sum items =
                let consts, vars = separate items
                let vars = vars
                           |> List.collect (function Sum xs -> fold_sum xs | x -> [x])
                           |> List.groupBy id
                           |> List.map (fun (e, l) -> if l.Length > 1 then Product [Const (float l.Length); e] else e)
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
                         | Sum xs -> (fold_sum >> Sum) xs
                         | Product xs -> (fold_product >> Product) xs
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
                         | Exponentiation (x, Exponentiation (y, z)) -> Exponentiation (x, Product [y; z])
                         | Sum [] -> Const 0.0
                         | Product [] -> Const 1.0
                         | Sum [x] | Product [x] -> x
                         | Sum xs ->
                             xs
                             |> process_pairwise (fun a b ->
                                 //printf "fold %O and %O" a b
                                 None
                             )
                             |> Sum
                         | Product xs ->
                             xs
                             |> process_pairwise (fun a b ->
                                 //printf "fold %O and %O" a b
                                 None
                             )
                             |> Product
                         | a -> a

            printfn "%O    =====>    %O" original folded
            if folded = original then
                folded
            else
                folded.Fold

        member x.SolveFor result =
            match x.Fold with
            | Variable -> Values [result]
            | Const c when c = result -> All
            | Const _ -> Empty
            //| Sum (Const a, b) -> b.SolveFor (result - a)
            //| Sum (a, Const b) -> a.SolveFor (result - b)
            //| Product (Const a, b) -> b.SolveFor (result / a)
            //| Product (a, Const b) -> a.SolveFor (result / b)
            | a -> failwithf "TODO: %O" a
