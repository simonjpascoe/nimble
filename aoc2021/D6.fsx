#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let parse input =
  input |> List.head |> ints64

let day6 n (fish : int64 list) =
  // there are no `zero` fish to start with, not all states may be present
  let state00 = fish |> List.countBy id |> List.map (second int64) |> Map.ofList
  let state0 = [0L..8L] |> List.map (fun i -> Map.tryFind i state00 |> Option.defaultValue 0L)

  let step state =
    state |> List.mapi (fun i _ -> match i with
                                    | 6 -> state.[7] + state.[0]
                                    | 8 -> state.[0]
                                    | j -> state.[j+1]
                                    )
  applyNtimes step n state0 |> List.sum
