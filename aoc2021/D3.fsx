#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

let count (input : string list) =
  let nx = input.[0].Length
  let places = [0..nx-1]

  let f1 i =  List.countBy (fun (s:string) -> s.[i])
  let counts = places |> List.map (fun p -> f1 p input |> List.sortBy fst)
  counts

let part1 input =
  let counts = count input
  let maxb = counts |> List.map (fun xs -> let (a,a1) = xs.[0]
                                           let (b,b1) = xs.[1]
                                           if a1 > b1 then a else b)
                    |> List.toArray
                    |> String |> bin2int64

  let minb = counts |> List.map (fun xs -> let (a,a1) = xs.[0]
                                           let (b,b1) = xs.[1]
                                           if a1 < b1 then a else b)
                    |> List.toArray
                    |> String |> bin2int64
  maxb,minb

let part2 (input: string list) =
  let xs = [0..input.[0].Length]

  let step oxygen state index =
    if List.length state = 1
      then state
      else let counts = debugn (count state)
           let [(_, c1); (_, c0)] = counts.[index]
           let keep = if oxygen then if c0 >= c1 then '1' else '0'
                                else if c0 >= c1 then '0' else '1'
           state |> List.filter (fun s -> s.[index] = keep) |> debugn

  let oxy = List.fold (step true) input xs |> List.head |> bin2int64
  let co2 = List.fold (step false) input xs |> List.head |> bin2int64

  oxy, co2


