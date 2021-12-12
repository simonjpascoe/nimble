#load @"./lib/aoc.fs"
#load @"./aoc2020/D10.fsx"

open Nimble.AOC
open D10

let input = loadInput 2020 10 |> List.map int64

let output1 = day10a input
let output2 = day10b input
