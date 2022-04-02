open System
open System.IO
open System.Text.RegularExpressions

let firstMatch pattern input =
  let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
  if (m.Success) then Some m.Groups.[1].Value else None

// HELPERS


let debugn a = printfn "%A" a; a

// LOADERS
let readInput () =
    //printfn "Ready"
    let count = Console.ReadLine() |> Int32.Parse
    List.init count (fun i -> Console.ReadLine() |> fun x -> x.Split(' ') |> fun xs -> (int xs.[0], int xs.[1], xs.[2]))

// PROBLEM
// CJ -> +X
// JC -> +Y
let cost x y (cj: string) =
    cj.ToCharArray() |> Array.pairwise |> Array.sumBy (fun e -> match e with | ('C', 'J') -> x | ('J', 'C') -> y | _ -> 0)

let replace1 x y vk = 
    let m = Regex.Match(vk, "([C|J]?(\\?+)[C|J]?)")
    match m.Success with 
        | false -> None
        | true  -> let left, right = m.Value.[0], m.Value.[m.Length-1]
                   let fill = match left, right with
                                | 'C', 'J' -> if x < y then 'C' else 'J'
                                | 'J', 'C' -> if y < x then 'J' else 'C'
                                | '?', '?' -> 'C'
                                | '?', x   -> x
                                | x, '?'   -> x
                                | x, _     -> x
                   let v2 = m.Value.Replace('?', fill)
                   Some (vk.Replace(m.Value, v2))

let fillCheaply x y (cjq : string) = 
    // each question mark _set_ will be the same character 
    // depending upon the left and right terminators
    // so split it up, then glue it back
    // which sounds like a good task for Regex
    let mx = List.unfold (fun s -> replace1 x y s |> Option.map (fun x -> (x,x))) cjq
    match mx with 
      | [] -> cjq
      | xs -> List.last xs
      |> cost x y 

let solve_1_1 i (x, y, cjq) = 
    fillCheaply x y cjq |> fun cost -> printfn "Case #%d: %d" (i+1) cost

readInput () |> List.mapi  solve_1_1
    
