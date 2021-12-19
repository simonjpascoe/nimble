#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

type P3 = int * int * int

let d3d (x1, y1, z1) (x2, y2, z2) =
  ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)) |> double |> Math.Sqrt

let parse (input: string list) : Map<int, P3 list> =
  let data = List.map ints input
  data |>
    List.fold (fun (index, collection, storage) t ->
                    match t with
                      | []      -> (-1, [], storage |> Map.add index collection)
                      | [id]    -> (id, [], storage)
                      | [x;z;y] -> (index, (x,y,z) :: collection, storage)
                      | _       -> failwith "illogical"
              ) (-1, [], Map.empty)
     |> thd3

let analyse (data : Map<int, P3 list>) : Map<int, (int * int * double) list> =
  let d1 (xs : P3 list) =
    let ys = xs |> List.mapi (fun i x -> (i, x))
    [for (i,a) in ys do for (j,b) in ys do if i < j then yield (i, j, d3d a b)]
  data |> Map.map (fun _ v -> d1 v |> List.sortBy thd3)
