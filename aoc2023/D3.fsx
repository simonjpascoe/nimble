#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions

let words3 str =
  Regex.Matches(str, "(\w*)")
    |> Seq.choose (fun m -> if m.Value <> "" then Some (m.Value, m.Index, m.Length) else None)
    |> List.ofSeq

let symbols3 str =
  Regex.Matches(str, "[^.\d]")
    |> Seq.choose (fun m -> if m.Value <> "" then Some (m.Value, m.Index, m.Length) else None)
    |> List.ofSeq


let parse (input: string list) =
  let ws = List.mapi (fun i s -> words3 s |> List.map (fun (v,s,l) -> (int v, i, s, l))) input |> List.collect id
  let ss = List.mapi (fun i s -> symbols3 s |> List.map (fun (v,s,l) -> ((i, s), v))) input |> List.collect id |> Map.ofList
  ws, ss


let day3a () =
  let input = loadInput 2023 3
  let values, symbols = parse input
  let n = List.length input   // square
  // now look around each number to see if any adjacent cell has a symbol entry (it doesnt matter how many symbols)
  let check (v, row, col, length) = let cols = [col-1 .. col + length] |> List.capfloor 0 n
                                    let rows = [row-1 .. row+1] |> List.capfloor 0 n
                                    let flag = cols |> List.collect (fun col -> rows |> List.map (fun row -> row, col))
                                                    |> List.filter (fun (x, y) -> Map.containsKey (x,y) symbols)
                                                    |> List.isEmpty
                                    if flag then 0 else v

  List.map check values |> List.sum

let day3b () =
  let input = loadInput 2023 3
  let values, symbols = parse input
  let n = List.length input   // square
  // for each number, create the set of cells that it is covering
  let parts (v, row, col, length) = let cols = [col .. col + length - 1]
                                    v, cols |> List.map (fun c -> row, c) |> Set.ofList
  let stars = Map.filter (fun k v -> v = "*") symbols |> Map.toList
  let guide = List.map parts values
  let gears = 
    stars 
      |> List.map (fun (rc, _v) -> let checks = adjacents true (0, n) (0, n) rc |> Set.ofList
                                   let xs = guide |> List.map (fun (v, ps) -> v, Set.intersect checks ps)
                                                  |> List.filter (snd >> Set.isEmpty >> not)
                                                  |> List.map fst
                                   if List.length xs = 2 then xs[0] * xs[1] else 0)
  gears |> List.sum

day3b()