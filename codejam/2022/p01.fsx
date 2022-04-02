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
    Seq.init count (fun i -> Console.ReadLine() |> ints |> function xs -> xs.[0], xs.[1])
      |> Seq.toList

// PROBLEM
let layout R C =
  let odd x = x % 2 = 1
  let row = function | 0            -> ".." + String.replicate (C-1) "+-" + "+"
                     | 1            -> ".." + String.replicate (C-1) "|." + "|"
                     | n when odd n -> String.replicate C "|." + "|"
                     | _            -> String.replicate C "+-" + "+"
  let template = List.init (2 * R + 1) row

  template |> String.concat Environment.NewLine

readInput1 ()
  |> List.mapi (fun i (R, C) -> printfn "Case #%d:" (i+1)
                                let t = layout R C
                                printfn "%s" t
                                )