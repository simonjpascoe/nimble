open System

let ints (s: string) = s.Split ' ' |> Array.map int
let ints64 (s: string) = s.Split ' ' |> Array.map int64
let debugn a = printfn "%A" a; a

// LOADERS

let readInput1 () =
  let count = Console.ReadLine() |> int
  Array.init count (fun i -> Console.ReadLine () |> ignore
                             let ff = Console.ReadLine() |> ints64
                             let pp = Console.ReadLine() |> ints
                             ff, pp)

let prepare ff pp = Array.mapi2 (fun i f p -> (i+1, f, p)) ff pp

let rec step vs =
  match vs with
    | [|(_, v, _)|] -> v
    | ps    -> let deep = ps |> Array.map (fun (_, _, p) -> p) |> Array.max
               if deep = 0
                 then ps |> Array.map (fun (_,a,_) -> a) |> Array.sum
                 else let children, nodes1 = ps |> Array.partition (fun (_, _, p) -> p = deep)
                      let node, remaining = nodes1 |> Array.partition (fun (i, _, _) -> i = deep)
                      let v = Array.concat [node; children] |> Array.map (fun (_,a,_) -> a) |> fun xs -> Array.sum xs - Array.min xs
                      let node2 = Array.head node |> fun (a,_,c) -> (a,v,c)
                      step (Array.concat [[|node2|]; remaining])


// type Tree = | Node of int64 * Tree array
//             | Leaf of int64

// let build ff pp =
//   let nodes = Array.mapi2 (fun i f p -> (i+1, f, p)) ff pp

//   let rec q f i = let children = nodes |> Array.filter (fun (_, _, p) -> p = i)
//                   match children with
//                     | [||] -> Leaf f
//                     | xs   -> Node (f, xs |> Array.map (fun (i1, f1, _) -> q f1 i1))
//   q 0L 0

// let rec algebra tree =
//   match tree with | Leaf y -> y
//                   | Node (y, xs) -> let a = Array.concat [[|y|]; (Array.map algebra xs)]
//                                     Array.sum a - Array.min a

//

// let ff1,pp1 = [|60; 20; 40; 50|], [|0;1;1;2|]
// let ff2, pp2 = [|100; 100; 100; 90; 80; 100; 90; 100|], [|0; 1; 2; 1; 2; 3; 1; 3|]

readInput1 () |> Array.mapi (fun i (a, b) -> printfn "Case #%d: %d" (i+1) (prepare a b |> step))