#load @"./lib/aoc.fs"
#load @"./aoc2020/D12.fsx"

open Nimble.AOC
open D12

let input = loadInput 2020 12

let TIME = true
let f f = if TIME then timeit f else f()

let output1 = f (fun () -> day12a input)
let output2 = f (fun () -> day12b input)
