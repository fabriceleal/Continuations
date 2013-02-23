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
        

[<EntryPoint>]
let main argv = 

    // Close enough?    
    max 1 2 |> print_d

    // More CPS Styl-ish
    max_k 1 2 print_d

    // this:
    let g n = n + 1
    let f n = g(n + 1) + 1
    printfn "Normal: %d" (f(1) + 1)
    // becomes:
    let g_k n k = k(n + 1)
    let f_k n k = g_k(n + 1) (fun x -> k(x + 1))
    f_k 1 (fun x -> printfn "CPS: %d" (x + 1))

    // this:
    factorial 5 |> printfn "Normal Factorial of %d: %d" 5
    // becomes:
    factorial_k 5 (fun x -> printfn "CPS Factorial of %d: %d" 5 x) |> ignore
    
    // this:
    sum 5 |> printfn "Normal sum of %d: %d" 5
    // becomes:
    sum_k 5 (fun t -> printfn "CPS sum of %d: %d" 5 t)

    // this:
    fibo 9 |> printfn "Normal fibonacci %d: %d" 9
    // becomes:
    fibo_k 9 (fun x -> printfn "CPS fibonacci %d: %d" 9 x)

    // this:
    match (nth 2 [1; 2; 3; 4; 5; 6]) with
    | Some(n) -> printfn "Normal %dth: %d" 2 n
    | None -> printfn "Normal %dth: List not big enough!" 2
    match (nth 15 [1; 2; 3; 4; 5; 6]) with
    | Some(n) -> printfn "Normal %dth: %d" 15 n
    | None -> printfn "Normal %dth: List not big enough!" 15
    // becomes:
    nth_k 3 [1; 2; 3; 4; 5; 6] (function
                                | Some(n) -> printfn "CPS %dth: %d" 2 n
                                | None -> printfn "CPS %dth: List not big enough!" 2)
    nth_k 15 [1; 2; 3; 4; 5; 6] (function
                                | Some(n) -> printfn "CPS %dth: %d" 15 n
                                | None -> printfn "CPS %dth: List not big enough!" 15)

    // wait
    Console.ReadKey() |> ignore

    0
