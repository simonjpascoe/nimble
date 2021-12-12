module Nimble.AOC

open System
open System.IO
open System.Text.RegularExpressions

let timeit f =
  let timer = System.Diagnostics.Stopwatch()
  timer.Start()
  let result = f()
  printfn "Elapsed = %i" timer.ElapsedMilliseconds
  result

let flag = function true -> 1 | false -> 0

let applyNtimes f n input =
  List.fold (fun s _ -> f s) input [1..n]

let debugn a = printfn "%A" a; a

let loadInput year day =
  sprintf "./data/aoc/%d/d%d_input.txt" year day
    |> File.ReadAllLines
    |> List.ofArray

let loadSampleInput year day =
  sprintf "./data/aoc/%d/d%d_input_sample.txt" year day
    |> File.ReadAllLines
    |> List.ofArray

let between   left right x = (left < x) && (x < right)
let betweenL  left right x = (left <= x) && (x < right)
let betweenR  left right x = (left < x) && (x <= right)
let betweenLR left right x = (left <= x) && (x <= right)

let ints str =
  Regex.Matches(str, "(\d*)")
    |> Seq.choose (fun m -> if m.Value <> "" then Some (int m.Value) else None)
    |> List.ofSeq


let ints64 str =
  Regex.Matches(str, "(\d*)")
    |> Seq.choose (fun m -> if m.Value <> "" then Some (int64 m.Value) else None)
    |> List.ofSeq

let bin2int64 s = Convert.ToInt64(s, 2)
let inline char2int (s: char) = int s - int '0'

let pad_matrix (v : 'a) (m : 'a list list) =
  let m2 = m |> List.map (fun r -> List.concat [[v]; r; [v]])
  let n = [List.length m2 |> fun c -> List.replicate c v]
  List.concat [n;m2;n]

let adjacents diags (x, y) =
  if diags
    then [x-1, y-1; x,y-1; x+1, y-1; x-1, y; x+1, y; x-1, y+1; x, y+1; x+1, y+1]
    else [x-1,y; x, y-1; x+1,y; x, y+1]