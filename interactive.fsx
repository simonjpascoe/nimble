#load @"./lib/aoc.fs"
#load @"./lib/math.fs"
#load @"./lib/functional.fs"
#load @"./aoc2020/D13.fsx"

open Nimble.Math
open Nimble.AOC
open Nimble.Functional
open D13

let input = loadInput 2020 13

let TIME = true
let f f = if TIME then timeit f else f()

//let output1 = f (fun () -> day13a input)
let output1 = day13a input
let output2 = day13b input
