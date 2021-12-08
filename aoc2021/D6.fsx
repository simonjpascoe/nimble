#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let parse input =
  input |> List.head |> ints

let day6a days fish =
  let step1 fishy =
    match fishy with
      | 0 -> [6; 8]
      | n -> [n-1]
  let step fishes = List.collect step1 fishes
  applyNtimes step days fish

// of course part 1 is easy to brute force
// part 2 using the same algo, blows up

let day6b days fish =
  // determine the formula for fishy
  // given a single fish, after n steps, how many fish
  // do we have?

  1