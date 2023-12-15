#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions

let calculate (grid: char array array) =
  let sz = Array.length grid[0]
  let fn (i, loc, dots, weight) x =
    match x with
      | '#' -> (i+1, i, 0, weight)
      | '.' -> (i+1, loc, dots+1, weight)
      | 'O' -> (i+1, i, dots, weight+(sz-i)+dots)

  grid |> Array.map (Array.fold fn (0, 0, 0, 0)) |> Array.sumBy fth4

let drop goleft (input : string) =
  input.Split('#')
    |> Array.map (fun block -> let n = block.Length
                               let s2 = block.Replace(".","")
                               if goleft then s2.PadRight(n,'.') else s2.PadLeft(n, '.'))
    |> String.concat "#"

let getrc isr rc (grid: string array) =
  if isr
    then grid[rc]
    else grid |> Array.map (fun row -> row[rc]) |> String

let toCols (grid: string array) =
  let cols = String.length grid[0]
  [|0..cols-1|] |> Array.map (fun i -> getrc false i grid)

let score grid =
  let sz = Array.length grid
  grid |> Array.mapi (fun i xs -> xs |> Array.sumBy (fun x -> if x='O' then sz-i else 0)) |> Array.sum

let cycle grid =
  let grid1 =
    grid |> toCols |> Array.map (drop true)
         |> toCols |> Array.map (drop true)
         |> toCols |> Array.map (drop false)
         |> toCols |> Array.map (drop false)
  let weight = grid1 |> Array.map (fun s -> s.ToCharArray()) |> score
  weight, grid1

let day14a =
  let input = yourPuzzleInput 2023 14 true
  input |> List.toArray
        |> toCols
        |> Array.map (fun s -> s.ToCharArray())
        |> calculate
        // or swap two lines with below
        //|> Array.map (drop true)
        //|> toCols
        //|> Array.map (fun s -> s.ToCharArray())
        //|> score

let findCycle n xs =
  let a = List.min xs
  let basic = xs |> List.mapi (fun i x -> if x = a then Some i else None) |> List.choose id
  let ys = basic |>List.pairwise |> List.map (fun (a,b) -> b - a) |> fun q -> List.skip (List.length q - n) q
                 |> Set.ofList
                 |> Set.toList
                 |> List.tryExactlyOne
  match ys with
    | None -> None
    | Some L -> xs |> List.findIndex (fun x -> x = a) |> fun i -> (i, List.skip i xs |> List.take L) |> Some

let cycleCalculator idx0 (cycle: 'a list) n =
  let pos = (n-idx0-1) % (List.length cycle)
  cycle[pos]

let day14b =
  let input = yourPuzzleInput 2023 14 false
  let C = input |> Array.ofList
                |> applyFnMF cycle 1000
                |> fst
                |> List.ofSeq
                |> debugn
                |> findCycle 8
  let fn = match C with
            | Some (i0, cs) -> fun n -> cycleCalculator i0 cs n
            | None -> fun n -> -1
  fn 1000000000