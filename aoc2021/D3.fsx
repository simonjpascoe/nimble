#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

let data = loadInput 2021 3 

let count (input : string list) = 
  let n = input |> List.length
  let nx = input.[0].Length
  let places = [0..nx-1]

  let f1 i =  List.countBy (fun (s:string) -> s.[i])
  let counts = places |> List.map (fun p -> f1 p input)
  counts

let part1 input = 
  let counts = count input
  let maxb = counts |> List.map (fun xs -> let (a,a1) = xs.[0]
                                           let (b,b1) = xs.[1]
                                           if a1 > b1 then a else b)
                    |> List.toArray 
                    |> System.String
    
  let minb = counts |> List.map (fun xs -> let (a,a1) = xs.[0]
                                           let (b,b1) = xs.[1]
                                           if a1 < b1 then a else b)
                    |> List.toArray 
                    |> System.String
  maxb,minb

let data0 = [
"00100"
"11110"
"10110"
"10111"
"10101"
"01111"
"00111"
"11100"
"10000"
"11001"
"00010"
"01010"
]

let part2 data =
  let maxb, minb = part1 data
  let f1 n v xs = xs |> List.filter (fun (x: string) -> x.[n] = v)
  let sz = data.[0].Length

  // maxes, tie break = 1, min tie break = 0
  (data, data, 0) |> List.unfold (
      fun (maxs, mins, i) -> 
        if i = sz 
            then None
            else 
                let m2 = if List.length maxs = 1 then maxs
                            else 
            let [(a,a1);(b,b1)] = debugn (count maxs).[i]
                 let [(c,c1);(d,d1)] = (count mins).[i]
                 let aa = if a1 = b1 then '1' else if a1 > b1 then a else b
                 let cc = if c1 = d1 then '0' else if c1 < d1 then c else d
                 let m2 = if List.length maxs = 1 then maxs
                            else f1 i aa maxs
                 let m3 = if List.length mins = 1 then mins
                            else f1 i cc mins
                 Some ((m2, m3, i+1), (m2, m3, i+1))

  )
