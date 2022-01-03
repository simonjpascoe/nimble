#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

type P3 = int * int * int
type M3 = P3 * P3 * P3

let sortIndex xs = xs |> List.mapi (fun i x -> i, x) |> List.sortBy snd |> List.map fst

let zero = (0,0,0)

let d0 n d = if d = 0 then 0 else n / d
let div (x1, y1, z1) (x2, y2, z2) = (d0 x1 x2, d0 y1 y2, d0 z1 z2)


let cr3 (x1,y1,z1) (x2,y2,z2) : M3 =
  // (assuming no degeneration here)
  let f a b = if a = b then 1 else if a = -b then -1 else 0
  let r1 = (f x1 x2, f x1 y2, f x1 z2)
  let r2 = (f y1 x2, f y1 y2, f y1 z2)
  let r3 = (f z1 x2, f z1 y2, f z1 z2)
  (r1,r2,r3)

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
                yield xs1 |> List.collect (fun (x10,x11,x12,p1) -> 
                                xs2 |> List.choose (fun (x20,x21,x22,p2) -> 
                                    if p1 = p2 then Some (s1, s2, (x10,x11,x12), (x20,x21,x22)) else None))]
           |> List.concat
          //  |> List.groupBy (fun (s1, s2, p1, ps) -> (s1, s2))
          //  |> List.map (second (List.map (fun (_,_,c,d) -> (c,d))))

  // now need to find the relevant rotation matrix for each triangle. because the coordinate systems
  // are inconsistent, we can align the vectors from points to points before finding the matrix that aligns these
  // as in absolute space, the vectors from a -> b -> c -> a all are the same
  // expand an example
  let align1 a3 =
    let (s1, s2, (p1,p2,p3), (q1,q2,q3)) = a3

    let vgen s ps = ps @ [List.head ps] |> List.pairwise |> List.map (fun (p, q) -> vec data.[s].[p] data.[s].[q]) |> debugn
      
    let pvs = vgen s1 [p1;p2;p3]
    let uvs = vgen s2 [q1;q2;q3]
    let pair p = uvs |> List.find (fun u -> d3d zero u = d3d zero p)
    let m3s = pvs |> List.map (fun p -> cr3 p (pair p))
    // if all the same, we accept this possibility...
    let test = m3s.[1..] |> List.forall (fun x -> x = m3s.[0])
    if test then Some (s1, s2, m3s.[0]) else None

  a2 |> List.choose align1


let day19a s =
  let xs = (if s then loadSampleInput 2021 19 else loadInput 2021 19) |> parse
  analyse xs