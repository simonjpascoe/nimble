#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC


let parse (input : string list) =
  input |> List.map (fun s -> s.ToCharArray() |> Array.map char2int |> List.ofArray)

let findSinks (input : int list list) =
  // pad the outsides with 999 then check the four directions around each
  // this is solvable in excel quicker!

  let bx = input.[0].Length - 1
  let by = input.Length - 1

  let v x y = let test = x < 0 || x > bx || y < 0 || y > by
              if test then 999 else input.[y].[x]

  let t x y =
    let X = v x y
    let a = v (x-1) y
    let b = v (x+1) y
    let c = v x (y-1)
    let d = v x (y+1)
    let ps = [a;b;c;d]
    List.forall (fun x -> x > X) ps

  let xs = [0 .. bx]
  let ys = [0 .. by]
  [ for y in ys do
      for x in xs do
        if t x y then yield (x, y, v x y)]

let day9a input =
  let sinks = findSinks input
  sinks |> List.sumBy (fun (_, _, v) -> 1 + v)

let day9b input =
  let sinks = findSinks input |> List.mapi (fun i (x,y,_) -> (x,y), i) |> Map.ofList
  // we only need to care about the sink and '9's, so lets create a neat map of our map
  let state0 = input |> List.mapi (fun row cols -> cols |> List.mapi (fun col value -> if value = 9 then -9
                                                                                                    else Map.tryFind (col, row) sinks |> Option.defaultValue -1 ))
  // so how to "fill" the basins?
  // -> can iterate so that each basin (+ve number) checks the up/down/left/right to be a -1. if so, change it to the basin number
  // -> repeat after each transition until fixed point

  let bx = input.[0].Length - 1
  let by = input.Length - 1

  // let's go! everything only belongs to one basin (v > 0)
  let f (state: int list list) r c =
    let v y x = let test = x < 0 || x > bx || y < 0 || y > by
                if test then -99 else state.[y].[x]

    let v0 = v r c
    if v0 = -9 || v0 >= 0
      then v0
      else
        let vs = [v (r-1) c
                  v r (c+1)
                  v (r+1) c
                  v r (c-1)]
        let v1 = List.max vs
        if v1 >= 0 then v1 else v0

  let step state = state |> List.mapi (fun r cs -> cs |> List.mapi (fun c v -> f state r c))

  let staten = applyUntil step (fun _ -> false) state0
  match staten with
    | Choice2Of2 t -> List.collect id t |> List.countBy id
                                        |> List.filter (fun p -> fst p >= 0)
                                        |> List.sortByDescending snd
                                        |> List.take 3
                                        |> List.map snd
                                        |> List.fold (fun s t -> s * t) 1
    | Choice1Of2 _ -> failwith "illogical"