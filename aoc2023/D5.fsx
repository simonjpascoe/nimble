#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions

let mkf (xs : int64 list list) = 
  let fs = xs |> List.map (fun [d;s;l] -> fun x -> if (s <= x) && (x <s + l) then Some (d + (x - s)) else None)
  fun seed -> List.fold (fun (x, m) f -> if m then match f x with Some v -> (v, false) | None -> (x, true) else (x, m)) (seed, true) fs
                 |> fst
  
let parse xs =
  let ys = xs |> List.map ints64
  let seeds = ys[0]
  let fs = ys[2..] |> List.fold (fun (all, coll) t -> if t = [] then (all @ [coll], []) else (all, coll @ [t])) ([], [])
                   |> fst
                   |> List.map mkf
  let g = fun seed -> List.fold (fun p h -> h p) seed fs
  seeds, g

let day5a() = 
  let input = loadInput 2023 5
  let seeds, f= parse input
  List.map f seeds

let splitRange start length n =
  let gs = [start .. max 1L n .. start + length]
  if List.last gs <> (start + length)
    then gs @ [start + length]
    else gs

let day5b() = 
  let input = loadInput 2023 5
  let seeds, f = parse input
  // now need to extend the seeds
  List.chunkBySize 2 seeds
    |> List.toArray
    |> Array.Parallel.map (fun [start;length] -> {start..start+length} |> Seq.fold (fun s i -> min (f i) s) Int64.MaxValue |> debugn)
    |> Array.min