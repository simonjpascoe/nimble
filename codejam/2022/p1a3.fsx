open System

let debugn a = printfn "%A" a; a

// LOADERS
let readInput1 () =
    let count = Console.ReadLine() |> Int32.Parse
    Array.init count (fun i -> let e, w = Console.ReadLine() |> fun s -> s.Split ' ' |> Array.map int |> fun xs -> xs.[0], xs.[1]
                               let xs = List.init e (fun j -> Console.ReadLine() |> fun s -> s.Split ' '
                                                                                 |> Array.map int
                                                                                 |> List.ofArray)
                               xs)

// PROBLEM

let splitPrefix xs ys =
  if xs = ys then xs, [], []
    else let n1 = List.length xs
         let n2 = List.length ys
         let n = min n1 n2
         let xxs = List.take n xs
         let yys = List.take n ys
         let _, zs = List.fold2 (fun (s, zs) a b -> if s then (s, zs) else if a = b then (false, a::zs) else (true, zs)) (false, []) xxs yys
         let n3 = List.length zs
         List.rev zs, List.skip n3 xs, List.skip n3 ys

// SOLUTION
// on each exercise mutate to the next common remaining case, do exercise, repeat with state
// and sum the costs

let deltaCost xs ys =
  let _, delta1, delta2 = splitPrefix xs ys
  List.length delta1 + List.length delta2

let delta1 prev next = List.map2 (-) prev next

let solve (exercises: int list list) =
  let rec step (c, prev) queue =
    match queue with
      | [] -> c + List.sum prev
      | exs -> let common = exs |> List.transpose |> List.map List.min
               let cost0 = delta1 prev common |> List.sumBy abs
               let (next::rest) = exs
               let cost1 = delta1 common next |> List.sumBy abs
               step (c + cost0 + cost1, next) rest

  let state0 = exercises.[0] |> List.map (fun _ -> 0)
  step (0, state0) exercises

//let trial2 () = solve [[1;2;1]; [2;1;2]]
//let trial3 () = solve [[3;1;1]; [3;3;3]; [2;3;3]]
readInput1 () |> Array.mapi (fun i xs -> printfn "Case #%d: %A" (i+1) (solve xs))