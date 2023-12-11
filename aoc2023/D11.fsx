#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions



let parse xs : char array array = xs |> List.map (fun (s: string) -> s.ToCharArray()) |> List.toArray

let expand (xs: char array array) =
  let e1 ys = ys |> Array.map (fun row -> let unique = Set.ofSeq row = Set.singleton '.'
                                          match unique with true -> [|row; row|] | false -> [|row|])
                 |> Array.concat
  xs |> e1 |> Array.transpose |> e1 |> Array.transpose

let distances1 xs = 
  let points = xs |> Array.mapi (fun r col -> col |> Array.mapi (fun c v -> if v = '#' then Some (r,c) else None))
                  |> Array.collect id
                  |> Array.choose id
  let pairs = [for p1 in points do for p2 in points do if p1 <> p2 then yield (p1, p2)]
  pairs |> List.map (fun ((p1x, p1y),(p2x, p2y)) -> abs(p1x - p2x) + abs(p1y - p2y)) 

let points xs = 
  let points = xs |> Array.mapi (fun r col -> col |> Array.mapi (fun c v -> if v = '#' then Some ((int64)r,(int64)c) else None))
                  |> Array.collect id
                  |> Array.choose id
  points

let blanks xs = 
  let e1 ys = ys |> Array.mapi (fun i row -> let unique = Set.ofSeq row = Set.singleton '.'
                                             match unique with true -> Some ((int64)i) | false -> None)
                 |> Array.choose id
  let br = e1 xs
  let bc = e1 (xs |> Array.transpose)
  br, bc

let expand2 sz data points =
  let sz1 = sz - 1L
  let expr, expc = data
  let points2 = points |> Array.map (fun (pr, pc) -> let cr = expr |> Array.filter (fun r -> r < pr) |> Array.length
                                                     let cc = expc |> Array.filter (fun c -> c < pc) |> Array.length
                                                     (pr + sz1 * (int64)cr, pc + sz1 * (int64)cc))
  let pairs = [for p1 in points2 do for p2 in points2 do if p1 <> p2 then yield (p1, p2)]
  pairs |> List.map (fun ((p1x, p1y),(p2x, p2y)) -> abs(p1x - p2x) + abs(p1y - p2y)) 

let day11a =
  let input = loadInput 2023 11
  let expanded = parse input |> expand
  expanded |> distances1 |> List.sum |> fun v -> v/2

let day11b =
  let input = loadInput 2023 11
  let galaxies, expansions = parse input |> (points &&& blanks)
  expand2 1000000L expansions galaxies |> List.sum |> fun v -> v/2L