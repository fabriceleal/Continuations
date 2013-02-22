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

let rec factorial n =
    if n = 0 then
        1
    else
        n * factorial (n-1);;

let rec factorial_k n k =
    if n = 0 then
        k(1)
    else
        factorial_k (n-1) (fun x -> k(x * n));;

let add_one x = 
    x + 1

let add_one_k x k =
    k(x + 1)

let rec sum x =
    if x = 1 then
        1
    else
        sum(x - 1) + x;;

let rec sum_k x k =
    if x = 1 then
        k(1)
    else
        sum_k(x - 1) (fun y -> k(x + y));

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
    factorial 5 |> printfn "Normal Factorial of 5: %d"
    // becomes:
    factorial_k 5 (fun x -> printfn "CPS Factorial of 5: %d" x) |> ignore
    
    // this:
    sum 5 |> printfn "Normal sum of 5: %d"
    // becomes:
    sum_k 5 (fun t -> printfn "CPS sum of 5: %d" t)



    // wait
    Console.ReadKey() |> ignore

    0
