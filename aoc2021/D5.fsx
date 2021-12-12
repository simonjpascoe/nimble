#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

let parse (input : string list) =
  input |> List.map (fun x -> ints x |> function [a;b;c;d] -> (a,b),(c,d) | _ -> failwith "illogical")

let create (pairs: ((int * int) * (int * int)) list) =
  let gen ((a,b), (c,d)) =
    if a = c
      then [b .. (if b <= d then 1 else -1) .. d] |> List.map (fun y -> (a, y))
      else if b = d
             then [a .. (if a <= c then 1 else -1) .. c] |> List.map (fun x -> (x, b))
             else let xi = if a < c then 1 else -1
                  let yi = if b < d then 1 else -1
                  List.zip [a .. xi .. c] [b .. yi .. d]
  List.collect gen pairs

let day5 p1 (pairs: ((int * int) * (int * int)) list) =
  let ps = if p1 then pairs |> List.filter (fun ((a,b),(c,d)) -> a=c || b = d) else pairs
  ps |> create |> List.countBy id |> List.filter (fun p -> snd p >= 2) |> List.length