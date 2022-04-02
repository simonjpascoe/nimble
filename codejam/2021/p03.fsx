// Reversesort engineering

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
  List.init count (fun _ -> Console.ReadLine() |> ints |> fun xs -> xs.[0], xs.[1])

// PROBLEM
// create perms!
let permutations n =
  let rec perms list taken = 
    seq { if Set.count taken = List.length list then yield [] else
          for l in list do
            if not (Set.contains l taken) then 
              for perm in perms list (Set.add l taken)  do
                yield l::perm }
  let ls = List.init n (fun i -> i+1)
  perms ls Set.empty
  
let cost_range n = (n-1, n*(n+1 /2 - 1))

let cost xs = 
    [0..(List.length xs)-2] 
            |> List.fold (fun (c, p:int list) (i:int) -> 
                            let m = p.[i..] |> List.min
                            let j = List.findIndex ((=) m) p
                            let c2 = c + (j - i + 1)
                            let p2 = p.[0..i-1] @ (List.rev p.[i..j]) @ p.[j+1..]
                            (c2, p2)
                ) (0, xs)
    |> fst

let solve i (n, c) =
  let a,b = cost_range n
  let x = b
  let rec step m d = 
    


// let solve i (n, c) =
//     [0..(List.length problem)-2] 
//         |> List.fold (fun (c, p:int list) (i:int) -> 
//                         let m = p.[i..] |> List.min
//                         let j = List.findIndex ((=) m) p
//                         let c2 = c + (j - i + 1)
//                         let p2 = p.[0..i-1] @ (List.rev p.[i..j]) @ p.[j+1..]
//                         (c2, p2)
//             ) (0, problem)


// readInput1 () |> List.mapi solve