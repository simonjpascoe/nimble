open System
open System.IO
open System.Text.RegularExpressions

let RANDOM = System.Random()

let rands a b m = Array.init m (fun j -> (RANDOM.Next (b-a+1) + a))

let debugn a = printfn "%A" a; a

// LOADERS
let readInput1 () =
    let count = Console.ReadLine() |> Int32.Parse
    Array.init count (fun i -> let ln = Console.ReadLine() |> int
                               let ds = Console.ReadLine() |> fun s -> s.Split ' ' |> Array.map int |> Array.sort
                               ln, ds)

// PROBLEM
// don't think it is even optimal to start not at 1, becuase you can always
// shift down [2,3,4] to [1,2,3] without changing the left by rotating
// every dice the same way.

// DICE ARE SORTED

let straight ln ds =
  if Array.head ds >= ln
    then ln
    else ds |> Array.fold (fun st d -> if d >= st+1 then st+1 else st) 0

// let tcs = [[6;10;12;8]; [5;4;5;4;4;4]; [10;10;7;6;7;4;4;5;7;4]; [10]] |> List.map List.sort

// let gentc c n s = Array.init c (fun i -> let ln = 1 + RANDOM.Next n
//                                          rands 4 s ln)

// let timeit f =
//   let timer = System.Diagnostics.Stopwatch()
//   timer.Start()
//   let result = f()
//   printfn "Elapsed = %i" timer.ElapsedMilliseconds
//   result

// //readInput1 () |> timeit (fun () -> Array.mapi (fun i (ln,ds) -> printfn "Case #%d: %d" (i+1) (straight ln ds)))

// let trial n s =
//   let input = gentc 100 n s
//   let fn tc =
//     let a = Array.length tc
//     let b = Array.sort tc
//     let r = straight a b
//     r

//   timeit (fun () -> input |> Array.mapi (fun i tc -> fn tc))

readInput1 () |> Array.mapi (fun i (ln,ds) -> printfn "Case #%d: %d" (i+1) (straight ln ds))