#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions

type P = P of group:string list * data:int list * score:int

let parse xs =
  xs |> List.map (fun (xs: string) -> xs.Split(' ') |> List.ofArray)
     |> List.map (fun (a::b::_) -> P(a.Split('.') |> Array.filter (fun x -> x <> "") |> Array.toList,ints b, 1))

let (|Qs|Hs|Mx|) (input: string) =
  let ax = input.ToCharArray()
  let n = input.Length
  match ax |> Array.fold (fun (sq, sh) t -> (sq && t = '?', sh && t = '#')) (true, true) with
    | true, false -> Qs n
    | false, true -> Hs n
    | _ -> Mx n

let doesMatch group data =
  match group with
    | Qs n -> if n = data then true else false
    | Hs n -> data = n
    | Mx n -> false

let rec removeTails (P(group, data, score)) =
  let n1 = List.length group
  let n2 = List.length data
  match n1 with
    | 0 -> P(group, data, score)
    | 1 -> if doesMatch group[0] data[0] then P([], [], score) else P(group, data, score)
    | _ -> let ht = doesMatch group[0] data[0]
           let tt = doesMatch (List.last group) (List.last data)
           match ht, tt with
             | false, false -> P(group, data, score)
             | true, false  -> P(List.tail group, List.tail data, score) |> removeTails
             | false, true  -> P(List.take (n1-1) group, List.take (n2-1) data, score) |> removeTails
             | true, true   -> P(group |> List.tail |> List.take (n1-2), data |> List.tail |> List.take (n2-2), score) |> removeTails

let countA single x =
  match single with
    | Qs n -> n - x + 1
    | Hs n -> if n = x then 1 else failwith "bug1"
    | Mx n -> 1 // TODO

let countB single xs = 1

let countEasy (P(group, data, score)) =
  if List.length group = List.length data then
    let score2 = List.map2 countA group data |> product
    P([], [], score * score2)
  else
    P(group, data, score)

let removeIllogicals1 (P(group, data, score)) =
  // if something like "?#???#" [1,1] the first can never be satisfied
  let l1 = group

let count1 pattern data =
  match pattern with
    | Qs n -> n - data + 1
    | Hs n -> if n = data then 1 else failwith "bug1"
    | Mx n -> -1

let brute pattern guide =
  // if in doubt... brute it out


let day12a =
  let input = yourPuzzleInput 2023 12 true |> parse
  input |> List.map (removeTails >> countEasy)