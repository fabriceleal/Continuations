open System

// http://www.markhneedham.com/blog/2009/06/22/f-continuation-passing-style/


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




[<EntryPoint>]
let main argv = 

    // Close enough?    
    max 1 2 |> print_d

    max_k 1 2 print_d


    
    // wait
    Console.ReadKey() |> ignore

    0
