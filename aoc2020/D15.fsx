#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let input15 = [1;0;18;10;19;6]

let day15a (input : int list) n =
  // index at *1*
  let turns = [input.Length + 1.. n]

  let step state turn =
    let prev = List.last state
    let indexed = state |> List.mapi (fun i x -> (i,x))
                        |> List.filter (fun (_,x) -> x = prev)
    if List.length indexed = 1
      then state @ [0]
      else let xs = indexed |> List.rev |> List.take 2 |> List.map fst
           state @ [xs.[0] - xs.[1]]

  turns |> List.fold step input

let day15b (input : int list) =
  // can let day14a run again (brute force!), or ...
  // memoize the last positions for each number spotted
  let append map x i =
    let updated = match Map.tryFind x map with
                    | None        -> (i, -1)
                    | Some (a, _) -> (i, a)
    Map.add x updated map

  let state0 = input |> List.mapi (fun i x -> (i, x))
                     |> List.fold (fun s (i, x) -> append s x i) Map.empty

  let step (prev, index, memory) =
    let i1 = index + 1
    // find prev occurance
    let history = memory |> Map.tryFind prev
    match history with
      | None        -> Some (0, (0, i1, append memory 0 i1))
      | Some (_,-1) -> Some (0, (0, i1, append memory 0 i1))
      | Some (x,y)  -> let v = x-y
                       Some (v, (v, i1, append memory v i1))

  Seq.append input (Seq.unfold step (input |> List.last, input.Length - 1 , state0))



