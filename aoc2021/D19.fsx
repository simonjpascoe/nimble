#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

type P3 = int * int * int
type M3c = P3 * P3 * P3

let sortIndex xs = xs |> List.mapi (fun i x -> i, x) |> List.sortBy snd |> List.map fst

let zero = (0,0,0)

let d0 n d = if d = 0 then 0 else n / d
let div (x1, y1, z1) (x2, y2, z2) = (d0 x1 x2, d0 y1 y2, d0 z1 z2)

let d3d (x1, y1, z1) (x2, y2, z2) =
  ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)) |> double |> Math.Sqrt

let vec (x1, y1, z1) (x2, y2, z2) = (x2-x1), (y2-y1), (z2-z1)

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

let analyse (data : Map<int, P3 list>) =
  let n = data |> Map.count
  let data2 = data |> Map.map (fun _ vs -> vs |> List.mapi (fun i x -> i,x))
  let d1 xs = [for (i,a) in xs do
               for (j,b) in xs do
               if i < j then yield (i, j, d3d a b)]
  let d2 xs = [for (i,a) in xs do
               for (j,b) in xs do
               for (k,c) in xs do
               if (i < j) && (j < k) then yield (i, j, k, d3d a b + d3d b c + d3d a c)]
  let a1 = data2 |> Map.map (fun _ v -> d2 v |> List.sortBy fth4)
  // now find pairs of scanners such that they see triangles of same perimeter, regardless
  // of orientation
  let a2 = [for s1 in [0..n-1] do
            for s2 in [0..n-1] do
              if s1 < s2 then
                let xs1 = a1.[s1]
                let xs2 = a1.[s2]
                yield xs1 |> List.choose (fun t1 -> let ys = xs2 |> List.filter (fun t2 -> fth4 t1 = fth4 t2)
                                                    if List.isEmpty ys then None else Some (s1, s2, t1, ys))]
           |> List.concat
           |> List.groupBy (fun (s1, s2, p1, ps) -> (s1, s2))
           |> List.map (second (List.map (fun x -> (thd4 x, fth4 x))))

  a2
  // // find rotations and translations that line up the triangles identified
  // // the detection is necessary but not sufficient ot identify a transform
  // // the resulting vectors below need to be equal and basis vectors to signal a similarity
  // // (even then, technically, it's not quite sufficient...)
  // // test with head
  // let align ((s1, s2), xs) =
  //   let align1 ((p1, p2, p3, _), ys) =
  //     let pair2 xs y = xs |> List.find (fun y -> (d3d zero y) = (d3d zero y))
  //     let pair3 xs y =
  //       let z = xs |> List.find (fun y -> (d3d zero y) = (d3d zero y))
  //       let y2 = y |> List.map abs |> sortIndex
  //       let z2 = z |> List.map abs |> sortIndex


  //     match ys with
  //       | [(q1, q2, q3, _)] -> let v1 = vec data.[s1].[p1] data.[s1].[p2]
  //                              let v2 = vec data.[s1].[p2] data.[s1].[p3]
  //                              let v3 = vec data.[s1].[p3] data.[s1].[p1]
  //                              let u1 = vec data.[s2].[q1] data.[s2].[q2]
  //                              let u2 = vec data.[s2].[q2] data.[s2].[q3]
  //                              let u3 = vec data.[s2].[q3] data.[s2].[q1]
  //                              let vs = [v1;v2;v3]
  //                              let us = [u1;u2;u3]
  //                              let vs1 = vs |> List.map (pair2 us)
  //                              let xform = List.map2 div vs vs1
  //                              let mag = d3d zero xform.[0] = Math.Sqrt(3.0)
  //                              let equal = xform |> List.forall (fun x -> x = xform.[0])
  //                              if mag && equal then Choice1Of2 xform.[0] else Choice2Of2 (xform, mag, us, vs)
  //       | _ -> failwith "illogical 1"
  //   xs |> List.map (fun x -> x, align1 x)
  // a2 |> List.map (fun a -> fst a, align a)


let day19a s =
  let xs = (if s then loadSampleInput 2021 19 else loadInput 2021 19) |> parse
  analyse xs