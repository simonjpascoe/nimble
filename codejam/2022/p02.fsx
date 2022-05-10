open System
open System.IO
open System.Text.RegularExpressions

// HELPERS
let ints str =
  Regex.Matches(str, "(\d*)")
    |> Seq.choose (fun m -> if m.Value <> "" then Some (int m.Value) else None)
    |> List.ofSeq

let debugn a = printfn "%A" a; a

// LOADERS
let readInput1 () =
    let count = Console.ReadLine() |> Int32.Parse
    Seq.init count (fun i -> let p () = Console.ReadLine() |> ints |> List.take 4
                             [p(); p(); p()])
      |> Seq.toList

// PROBLEM
// For each input i (implied)
//   1) c+m+y+k = 10e6
//   2) for each printer j: c <= Cj, m <= Mj, y <= Yj amd k <= Kj
//  which means the ranges for each variable X are [0..min(X1, X2, X3, X4)]
//  because cannot even use more from any printer
//  impossible if the summation is smaller than 10e6

let J = [0..3]

let ddd (printers : int list list) =
  let f = List.sum
  let bounds = J |> List.map (fun j -> printers |> List.map (fun p -> p.[j]) |> List.min)
  let initial = f bounds
  if not (initial >= 1000000)
    then None
    else // now find the largest set of numbers less than bounds that summate okay
         let factor = float (f bounds) / 1000000.0
         let approx_soln = bounds |> List.map (fun b -> int (floor (float b / factor)))
         let gap = 1000000 - f approx_soln
         let nudge = match gap with | 0 -> [0;0;0;0] | 1 -> [1;0;0;0] | 2 -> [1;1;0;0] | 3 -> [1;1;1;0] | 4 -> [1;1;1;1]
         List.map2 (+) approx_soln nudge |> Some

let solve = List.mapi (fun j ps -> match ddd ps with | None    -> printfn "Case #%d: IMPOSSIBLE" (j+1)
                                                     | Some xs -> printfn "Case #%d: %d %d %d %d" (j+1) xs.[0] xs.[1] xs.[2] xs.[3])

let tcs =[[[300000; 200000; 300000; 500000]
           [300000; 200000; 500000; 300000]
           [300000; 500000; 300000; 200000]];
          [[1000000; 1000000; 0; 0]
           [0; 1000000; 1000000; 1000000]
           [999999; 999999; 999999; 999999]];
          [[768763; 148041; 178147; 984173]
           [699508; 515362; 534729; 714381]
           [949704; 625054; 946212; 951187]]]

readInput1 () |> solve