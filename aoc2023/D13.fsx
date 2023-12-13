#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions


let parse xs =
  xs |> List.splitByMarker ""
     |> List.map (List.map (fun (s:string) -> s.ToCharArray() |> List.ofArray))

let find1 (xs: 'a list list) =
  let n = xs[0] |> List.length
  [1..n-1] |> List.mapi (fun i v -> xs |> List.map ( List.paperfold1 v (=) >> List.all) |> List.all |> fun a -> if a then i+1 else 0) |> List.sum

let find2 (xs: 'a list list) =
  let n = xs[0] |> List.length
  // the error is the array which only has 1 false entry; i.e. one to correct
  [1..n-1] |> List.mapi (fun i v -> xs |> List.map (List.paperfold1 v (=) >> List.all) |> List.sumBy (function false -> 1 | true -> 0) |> function 1 -> i+1 | _ -> 0) |> List.sum

let day13a =
  let input = yourPuzzleInput 2023 13 false |> parse
  let verticals = input |> List.map find1
  let horizontals = input |> List.map (List.transpose >> find1)
  (List.sumBy (fun v -> v) verticals) + (List.sumBy (fun h -> 100 * h) horizontals)

let day13b =
  let input = yourPuzzleInput 2023 13 false |> parse
  let verticals = input |> List.map find2
  let horizontals = input |> List.map (List.transpose >> find2)
  (List.sumBy (fun v -> v) verticals) + (List.sumBy (fun h -> 100 * h) horizontals)