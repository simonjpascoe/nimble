#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions


let ints2 (xs : string) = xs.Split [|' '|] |> Array.map int64 |> List.ofArray

let set0 = [0L] |> Set.ofList



let calculateDiffs xs =
  let step ys =
    if Set.ofList ys = set0 then
      None
    else
      List.pairwise ys |> List.map (fun (a,b) -> b-a) |> fun a -> Some (a,a)
  List.unfold step xs

let day9a1() =
  loadInput 2023 9
    |> List.map ints2
    |> List.map ((calculateDiffs >> List.sumBy List.last) &&& List.last)
    |> List.sumBy (fun (a,b) -> a + b)

let day9b()=
  loadInput 2023 9
    |> List.map ints2
    |> List.map ((List.rev >> calculateDiffs >> List.sumBy List.last) &&& List.head)
    |> List.sumBy (fun (a,b) -> a + b)


// power series; calculate the n-th function
let calculateCoeffs xs =
  let step ys =
    if Set.ofList ys = set0 then
      None
    else
      List.pairwise ys |> List.map (fun (a,b) -> b-a) |> fun a -> Some (a,a)
  List.unfold step xs |> List.map List.head |> List.append [xs[0]] |> fun xs -> List.take (List.length xs - 1) xs

let nx n i = match i with | 0L -> 1L
                          | j -> [(n - j + 1L).. n] |> List.reduce (*)

// watch for overflow, may have to split
let fac n = [1L..n] |> List.fold (fun s x -> s * x) 1L

let mkf coeffs =
  let kx = List.mapi (fun i c -> c, fac i) coeffs
  fun n -> kx |> List.mapi (fun i (c,f) -> ((nx n i)/f) * c) |> List.sum

let day9a2() =
  let input = loadInput 2023 9
  let seqs = input |> List.map ints2
  seqs |> List.map (fun xs -> calculateCoeffs xs |> mkf |> fun f -> f (List.length xs)) |> List.sum