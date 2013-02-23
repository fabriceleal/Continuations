open System

// http://www.markhneedham.com/blog/2009/06/22/f-continuation-passing-style/
// http://nathansuniversity.com/cont_.html

// This would work if everything was curried
let k_combinator (f : 'a -> 'b) (k : 'b -> 'c) =
    f >> k;;

// Identity, regular-style
let id x = 
    x;;

// Identity, CPS-style
let id_k x k =
    k x;;

// Max, regular-style
let max x y =
    if x > y then x else y;;

// Max, CPS-style
let max_k x y k =
    if x > y then k x else k y;;

// Print "%d\n", Regular style
let print_d (x : int) = 
    printfn "%d" x;;

// factorial 
let rec factorial n =
    if n = 0 then
        1
    else
        n * factorial (n-1);;

let rec factorial_k n k =
    if n = 0 then
        k 1
    else
        factorial_k (n-1) (fun x -> k(x * n));;

// add_one
let add_one x = 
    x + 1;;

let add_one_k x k =
    k(x + 1);;

// sum
let rec sum x =
    if x = 1 then
        1
    else
        sum(x - 1) + x;;

let rec sum_k x k =
    if x = 1 then
        k 1
    else
        sum_k(x - 1) (fun y -> k(x + y));;

// fibo
let rec fibo n =
    if n = 0 then
        1
    else if n = 1 then
        1
        else
            fibo (n - 1) + fibo (n - 2);;

let rec fibo_k n k =
    if n = 0 then
        k 1
    else if n = 1 then 
        k 1
        else
            let k_new1 = (fun x1 -> 
                let k_new2 = (fun x2 -> k(x1 + x2))
                fibo_k (n - 2) k_new2
            )
            fibo_k (n - 1) k_new1;;

// nth
let rec nth n (ls : 'a list) =
    if ls.IsEmpty then
        None
    else if n = 0 then
        Some(ls.Head)
    else
        nth (n - 1) ls.Tail;;

let rec nth_k n (ls : 'a list) k =
    if ls.IsEmpty then
        k(None)
    else if n = 0 then
        k(Some(ls.Head))
    else
        nth_k (n - 1) ls.Tail k

type Tree =
    | Node of Tree * Tree
    | Leaf

// node_count
let rec node_count = function
                    | Node(lt, rt) -> 1 + node_count(lt)  + node_count(rt)
                    | Leaf -> 0;;

let rec node_count_k tree k = match tree with
                                | Node(ltree, rtree) ->
                                    let new_k1 = (fun ltree_count -> 
                                        let new_k2 = (fun rtree_count -> 
                                            k(1 + ltree_count + rtree_count)
                                        )
                                        node_count_k rtree new_k2
                                    )
                                    node_count_k ltree new_k1
                                | Leaf -> k 0

[<EntryPoint>]
let main argv = 
    // this:
    let g n = n + 1
    let f n = g(n + 1) + 1
    printfn "Normal: %d" (f(1) + 1)
    // becomes:
    let g_k n k = k(n + 1)
    let f_k n k = g_k(n + 1) (fun x -> k(x + 1))
    f_k 1 (fun x -> printfn "CPS: %d" (x + 1))


    // EXAMPLE max

    // Close enough?    
    max 1 2 |> print_d

    // More CPS Styl-ish
    max_k 1 2 print_d
    // -------------------

    // EXAMPLE factorial
    let fact_n = 5

    // this:
    factorial fact_n |> printfn "Normal Factorial of %d: %d" fact_n
    
    // becomes:
    factorial_k fact_n (fun x -> printfn "CPS Factorial of %d: %d" fact_n x) |> ignore
    // -------------------

    // EXAMPLE sum
    let sum_n = 5

    // this:
    sum sum_n |> printfn "Normal sum of %d: %d" sum_n

    // becomes:
    sum_k sum_n (fun t -> printfn "CPS sum of %d: %d" sum_n t)
    // -------------------

    // EXAMPLE fibo
    let fibo_n = 9
    
    // this:
    fibo fibo_n |> printfn "Normal fibonacci %d: %d" fibo_n
    
    // becomes:
    fibo_k fibo_n (fun x -> printfn "CPS fibonacci %d: %d" fibo_n x)
    // -------------------

    // EXAMPLE nth
    let ls, i1, i2 = [1;2;3;4;5;6], 3, 15
    
    // this:
    match (nth i1 ls) with
    | Some(n) -> printfn "Normal %dth: %d" i1 n
    | None -> printfn "Normal %dth: List not big enough!" i1

    match (nth i2 ls) with
    | Some(n) -> printfn "Normal %dth: %d" i2 n
    | None -> printfn "Normal %dth: List not big enough!" i2

    // becomes:    
    nth_k i1 ls (function
                | Some(n) -> printfn "CPS %dth: %d" i1 n
                | None -> printfn "CPS %dth: List not big enough!" i1)

    nth_k i2 ls (function
                | Some(n) -> printfn "CPS %dth: %d" i2 n
                | None -> printfn "CPS %dth: List not big enough!" i2)
    // -------------------

    // EXAMPLE count_nodes
    let t = Node(Node(Leaf, Leaf), Node(Leaf, Node(Leaf, Node(Leaf, Leaf))))

    // this:
    node_count t |> printfn "Normal nodes: %d"

    // becomes:
    node_count_k t (fun count -> printfn "CPS nodes: %d" count)

    // -------------------

    // wait
    Console.ReadKey() |> ignore

    0
