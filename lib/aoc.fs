module Nimble.AOC

open System.IO

let timeit f =
  let timer = System.Diagnostics.Stopwatch()
  timer.Start()
  let result = f()
  printfn "Elapsed = %i" timer.ElapsedMilliseconds
  result

let debugn a = printfn "%A" a; a

let loadInput year day =
  sprintf "./data/aoc/%d/d%d_input.txt" year day
    |> File.ReadAllLines
    |> List.ofArray

let between   left right x = (left < x) && (x < right)
let betweenL  left right x = (left <= x) && (x < right)
let betweenR  left right x = (left < x) && (x <= right)
let betweenLR left right x = (left <= x) && (x <= right)