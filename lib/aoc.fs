module Nimble.AOC

open System.IO

let loadInput year day =
  sprintf "./data/aoc/%d/d%d_input.txt" year day
    |> File.ReadAllLines
    |> List.ofArray