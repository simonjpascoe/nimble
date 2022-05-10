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
let readInput1 n filename =
  match filename with 
    | Some fn -> File.ReadAllLines fn |> List.ofArray |> List.tail
    | None    -> let count = Console.ReadLine() |> Int32.Parse
                 Seq.init (n * count) (fun i -> Console.ReadLine()) |> Seq.toList
    |> List.chunkBySize n

// PROBLEM
let parseProblems (input : string list list) =
    input |> List.map (fun (xs: string list) -> ints xs.[1])

let solve (problem: int list) =
    [0..(List.length problem)-2] 
        |> List.fold (fun (c, p:int list) (i:int) -> 
                        let m = p.[i..] |> List.min
                        let j = List.findIndex ((=) m) p
                        let c2 = c + (j - i + 1)
                        let p2 = p.[0..i-1] @ (List.rev p.[i..j]) @ p.[j+1..]
                        (c2, p2)
            ) (0, problem)


readInput1 2 None 
    |> parseProblems 
    |> List.map solve 
    |> List.mapi (fun i (c,_) -> sprintf "Case #%A: %A" (i+1) c)
    |> String.concat Environment.NewLine
    |> printf "%s"