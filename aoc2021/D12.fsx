#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let tuple2 = function | [a;b] -> (a,b) | _ -> failwith "illogical"

let parse (input: string list) =
  let edges = input |> List.map words
  let nodes = edges |> List.collect id |> List.distinct
  let tracker = nodes |> List.map (fun x -> x, if x.ToUpper() = x then 999 else 1) |> Map.ofList
  tracker |> Map.add "start" 0, edges |> List.map tuple2

let rec step path state edges =
  let current = List.last path
  let exits1 = edges |> List.choose (fun (left, right) -> match (left, right) with
                                                            | (a, x) when a = current -> Some x
                                                            | (x, a) when a = current -> Some x
                                                            | _ -> None)
  let exits2 = Map.filter (fun k v -> v > 0 && List.contains k exits1) state
  if Map.isEmpty exits2
    then [path]  // dead end
    // go for a walk
    else
      exits2 |> Map.toList
             |> List.collect (fun (e, c) -> if e = "end"
                                               then [List.concat [path; [e]]]
                                               else step (List.concat [path; [e]]) (state |> Map.add e (c-1)) edges)

let day12a state edges = step ["start"] state edges |> List.filter (fun xs -> List.last xs = "end") |> List.distinct |> List.length

let day12b state edges =
  // now create extra state for each small cave to bump visits to 2, one at a time
  let smols = state |> Map.toList |> List.choose (fun (k: string, _) -> if (k.ToLower() <> k || k = "start" || k = "end") then None else Some k)
  let sn = smols |> List.map (fun n -> state |> Map.add n 2)
  // run the steps for each new state, and then uniquify
  let paths = sn |> List.collect (fun si -> step ["start"] si edges)
  paths |> List.filter (fun xs -> List.last xs = "end") |> List.distinct |> List.length


