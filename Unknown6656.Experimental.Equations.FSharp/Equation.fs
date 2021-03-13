module Equation

open System.Collections.Generic


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


let internal _fold_cache = Dictionary<Expression, Expression>()


let inline (||>>) (f, g) (x, y) = (f x, g y)

let inline (?) c a b = if c then a else b

let inline (|Contains|_|) list item = if List.contains item list then Some item else None

let (|CacheContains|_|) item = match _fold_cache.TryGetValue item with true, folded -> Some folded | _ -> None

let inline intersect3 (a : 'a seq) b =
    let only_a = List<'a>()
    let intersection = List<'a>()
    for item in a do ((?) (Seq.contains item b) intersection only_a).Add item
    {|
        OnlyA = List.ofSeq only_a
        OnlyB = List.filter (fun item -> not <| Seq.contains item a) b
        Intersection = List.ofSeq intersection
    |}

let separate (selector : 'a -> Choice<'b, 'c>) =
    let rec intern acc = function
                         | [] -> acc
                         | x::xs ->
                             let acc = match selector x with
                                       | Choice1Of2 a -> (fst acc)@[a], snd acc
                                       | Choice2Of2 b -> fst acc, (snd acc)@[b]
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
            match func (head, y) with
            | Some r -> r::acc@(loop_list ys)
            | None ->
            match func (y, head) with
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



type Expression
    with
        static member private sort a b =
            let order = function
                        | Const _ -> 0
                        | Variable _ -> 1
                        | Negation _ -> 2
                        | Reciprocal _ -> 3
                        | Sum _ -> 4
                        | Product _ -> 5
                        | Exponentiation _ -> 6
            (order a).CompareTo(order b)

        static member Difference a b = Sum [a; Negation b]

        static member Division a b = Product [a; Reciprocal b]

        static member ClearFoldCache = _fold_cache.Clear

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
            let separate_consts_from_vars = separate (fun (x : Expression) -> match x.Fold with Const c -> Choice1Of2 c | other -> Choice2Of2 other)

            let rec fold_sum items =
                let consts, vars = separate_consts_from_vars items
                let vars = vars
                           |> List.map (function Sum xs -> fold_sum xs | x -> x)
                           |> List.collect (function Sum xs -> xs | x -> [x])
                           |> List.groupBy id
                           |> List.map (fun (e, l) -> if l.Length > 1 then Product [Const (float l.Length); e] else e)
                match List.fold (+) 0.0 consts with
                | 0.0 -> Sum vars
                | sum -> Sum ((Const sum)::vars)
                |> function
                | Sum [] -> Const 0.0
                | Sum [x] -> x
                | other -> other

            let rec fold_product items =
                let consts, vars = separate_consts_from_vars items
                let vars = vars
                           |> List.map (function Product xs -> fold_product xs | x -> x)
                           |> List.collect (function Product xs -> xs | x -> [x])
                           |> List.groupBy id
                           |> List.map (fun (e, l) -> if l.Length > 1 then Exponentiation (e, Const (float l.Length)) else e)
                           |> List.sortWith Expression.sort
                match List.fold (*) 1.0 consts with
                | -1.0 -> Negation (Product vars)
                | 1.0 -> Product vars
                | 0.0 -> Const 0.0
                | factor -> Product ((Const factor)::vars)
                |> function
                | Product [] -> Const 1.0
                | Product [x] -> x
                | other -> other

            match x with
            | Variable
            | Const _ -> x
            | CacheContains cached -> cached
            | original ->
                match original with
                | Reciprocal x -> Exponentiation(x.Fold, Const -1.0)
                | Negation x -> Negation x.Fold
                | Sum xs -> fold_sum xs
                | Product xs -> fold_product xs
                | Exponentiation (a, b) -> Exponentiation (a.Fold, b.Fold)
                | other -> failwithf "INVALID PROGRAM STATE:  %O" other
                |> function
                | Negation (Const x) -> Const -x
                | Reciprocal (Const x) -> Const (1.0 / x)
                | Negation (Negation x)
                | Reciprocal (Reciprocal x)
                | Exponentiation (x, Const 1.0) -> x
                | Exponentiation (_, Const 0.0)
                | Exponentiation (Const 1.0, _) -> Const 1.0
                | Exponentiation (Const 0.0, _) -> Const 0.0
                | Exponentiation (x, Exponentiation (y, z)) -> Exponentiation (x, Product [y; z])
                | Sum xs ->
                    xs
                    |> process_pairwise
                        (function
                        | Const 0.0, a -> Some a
                        | Negation a, b when a = b -> Some (Const 0.0)
                        | Product a, Product b ->
                            let res = intersect3 a b
                            match res.Intersection with
                            | [] -> None
                            | factors -> (Sum [Product res.OnlyA; Product res.OnlyB])::factors
                                         |> Product
                                         |> Some
                        | _ -> None)
                    |> Sum
                | Product xs ->
                    xs
                    |> process_pairwise
                        (function
                        | Const 1.0, a -> Some a
                        | Const 0.0, _ -> Some (Const 0.0)
                        | Reciprocal a, b when a = b -> Some (Const 1.0)
                        | Exponentiation (a, b), Exponentiation (c, d) when a = c -> Some(Exponentiation(a, Sum[b; d]))
                        | Exponentiation (a, b), Exponentiation (c, d) when b = d -> Some(Exponentiation(Product[a; c], b))
                        | _ -> None)
                    |> Product
                | a -> a
                |> fun folded ->
                    _fold_cache.[x] <- folded
                    // printfn "%70O   -->   %70O" original folded
                    if folded = original then
                        folded
                    else
                        folded.Fold

        member x.SolveForZero() =
            let x = x.Fold
            printfn "SOLVING  %O  ==  0" x
            match x with
            | Variable ->
                printfn "FINAL    x -> 0"
                Values [0.0]
            | Const 0.0 ->
                printfn "FINAL    0 -> ∀"
                All
            | Const c ->
                printfn "FINAL    %f -> {}" c
                Empty
            | Negation x -> x.SolveForZero()
            | Sum ((Const c)::xs) -> (Sum xs).SolveFor -c
            | Product ((Const c)::xs) -> (Product xs).SolveFor (1.0 / c)


            //| Sum sum ->


            //| Sum (Const a, b) -> b.SolveFor (result - a)
            //| Sum (a, Const b) -> a.SolveFor (result - b)
            //| Product (Const a, b) -> b.SolveFor (result / a)
            //| Product (a, Const b) -> a.SolveFor (result / b)
            | a -> failwithf "TODO: %O" a

        member x.SolveFor result =
            let x = x.Fold
            printfn "SOLVING  %O  ==  %f" x result
            match x with
            | Variable -> Values [result]
            | Const c when c = result -> All
            | Const _ -> Empty
            | Negation x -> x.SolveFor -result
            | x ->
                printfn "SOLVING  %O  ==  %f" x result
                (Sum [x; Const -result]).SolveForZero()
