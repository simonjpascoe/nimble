#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC


let parse (input : string list) =
  input |> List.map (fun s -> s.ToCharArray() |> Array.map char2int |> List.ofArray)
        |> pad_matrix -9999

let step (count0: int, consortium: int list list) =
  // increase the energy of all by +1
  let c2 = consortium |> List.map (List.map (fun x -> x+1))
  // run flashes
  let nplus (state : int list list)=
    state |> List.map2di (fun r c v -> if v > 9 then Some (r,c) else None)
          |> List.collect id
          |> List.choose id

  let flash fs incs (state : int list list) =
    state |> List.map2di (fun r c v -> if List.contains (r,c) fs
                                        then -9999
                                        else let ic = Map.tryFind (r,c) incs |> Option.defaultValue 0
                                             v + ic)

  let substep (count, state) =
    let tflashers = nplus state
    let increments = tflashers |> List.collect (adjacents true)
                               |> List.countBy id
                               |> Map.ofList
    count + List.length tflashers, flash tflashers increments state

  let count2, c3 = match applyUntil substep (fun _ -> false) (0, c2)
                     with | Choice1Of2 _ -> failwith "illogical"
                          | Choice2Of2 x -> x

  // then reset inner flashers to 0
  let c4 = c3 |> List.map2di (fun r c v ->
                    if r <> 0 && r <> 11 && c <> 0 && c <> 11 && v < 0 then 0 else v)
  count0 + count2, c4

let day11a n consortium =
  applyNtimes step n (0, consortium)

let day11b consortium =
  let innerSum xs =
    xs |> List.map2di (fun r c v -> if r <> 0 && r <> 11 && c <> 0 && c <> 11 then v else 0)
       |> List.collect (List.sum >> fun x->[x])
       |> List.sum
  applyUntil (fun (i, s) -> (i+1, step s)) (fun (m, (_, t)) -> m = 500 || innerSum t = 0) (0, (0, consortium))