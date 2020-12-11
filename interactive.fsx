#load @"./lib/aoc.fs"
#load @"./aoc2020/D11.fsx"

open Nimble.AOC
open D11

let input = loadInput 2020 11

let TIME = true
let f f = if TIME then timeit f else f()

let output1 = f (fun () -> day11a input)
let output2 = f (fun () -> day11b input)
