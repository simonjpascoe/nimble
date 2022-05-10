#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC


let parse (input: string list) =
  let i2 = input |> List.map (fun x -> x.ToCharArray())
  let ls = i2 |> List.mapi (fun i rs -> rs |> Array.mapi (fun j c -> if c <> '.' then Some ((i,j),c) else None))
              |> List.collect (Array.choose id >> List.ofArray)
  let di = input |> List.length
  let dj = input.[0] |> String.length
  Map.ofList ls, di, dj

let output (data:Map<int*int, char>) nx ny =
  let xs = [0..nx-1]
  let ys = [0..ny-1]
  xs |> List.map (fun x -> ys |> List.map (fun y -> printf "%O" (data |> Map.tryFind (x,y) |> Option.defaultValue '.')) |> fun _ -> printf "%A" Environment.NewLine)

let day25a (data, nx, ny) =
  // there are two iteration steps, (i) east (ii) south
  // sea cucumbers will loop their directions until stuck
  // ROW, COL

  let move east state =
    let step ((i, j), c) =
      match east, c with
       | true,  '>' -> let pp = if j+1 >= ny then (i,0) else (i,j+1)
                       match Map.tryFind pp state with
                         | None   -> (pp, c)
                         | Some _ -> ((i, j), c)
       | false, 'v' -> let pp = if i+1 >= nx then (0,j) else (i+1, j)
                       match Map.tryFind pp state with
                         | None   -> (pp, c)
                         | Some _ -> ((i, j), c)
       | _ -> ((i, j), c)
    state |> Map.toList |> List.map step |> Map.ofList

  let iterate = move true >> move false
  //iterate data
  applyUntil (fun (d, _, c) -> (iterate d, d, c+1)) (fun (a,b,_) -> a = b) (data, Map.empty, 0)