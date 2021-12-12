#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

type P3 = int * int * int
type P4 = int * int * int * int

let input17A = [
  ".#."
  "..#"
  "###"
]

let preprocess3 (input : string list) : P3 Set=
  let z = 0
  input |> List.mapi (fun rx row -> row |> Seq.mapi (fun cx x -> if x = '#' then Some (rx,cx,z) else None))
        |> Seq.concat
        |> Seq.choose id
        |> Set.ofSeq

let preprocess4 (input : string list) : P4 Set =
  let p3 = preprocess3 input
  p3 |> Set.map (fun (a,b,c) -> (a,b,c,0))

let bounds3 (state: P3 Set) =
  let xb = state |> Set.map fst3 |> Seq.minmax
  let yb = state |> Set.map snd3 |> Seq.minmax
  let zb = state |> Seq.map thd3 |> Seq.minmax
  (xb,yb,zb)

let bounds4 (state: P4 Set) =
  let xb = state |> Set.map fst4 |> Seq.minmax
  let yb = state |> Set.map snd4 |> Seq.minmax
  let zb = state |> Seq.map thd4 |> Seq.minmax
  let wb = state |> Seq.map fth4 |> Seq.minmax
  (xb,yb,zb,wb)

// do not need to actually calculate the full set, just to know if it's <= 4 (to distinguish those above 3)
let getSurrounding3 (state : P3 Set) ((a,b,c) : P3) =
  // make sure not including self
  let state2 = state - Set.singleton (a,b,c)
  let qs = state2 |> Set.toSeq |> Seq.filter (fun (x,y,z) -> (betweenLR (a-1) (a+1) x) &&
                                                             (betweenLR (b-1) (b+1) y) &&
                                                             (betweenLR (c-1) (c+1) z))
  qs |> Seq.truncate 4 |> Seq.toList |> List.length

// actually no need to truncate it to 4, testing shows that 13s -> 15s. hardly a difference, but
// in general this should be more efficient as not enumerating the whole set
let getSurrounding4 (state : P4 Set) ((a,b,c,d) : P4) =
  // make sure not including self
  let state2 = state - Set.singleton (a,b,c,d)
  let qs = state2 |> Set.toSeq |> Seq.filter (fun (x,y,z,w) -> (betweenLR (a-1) (a+1) x) &&
                                                               (betweenLR (b-1) (b+1) y) &&
                                                               (betweenLR (c-1) (c+1) z) &&
                                                               (betweenLR (d-1) (d+1) w))
  qs |> Seq.truncate 4 |>Seq.toList |> List.length

let day17a (input : string list) =
  let state0 = preprocess3 input

  let stepb state =
    let ((xl,xu),(yl,yu),(zl,zu)) = bounds3 state
    let index = [for x in [xl-1 .. xu+1] do
                  for y in [yl-1 .. yu+1] do
                   for z in [zl-1 .. zu+1] do
                    yield (x,y,z)]
    let state2 = index |> List.choose (fun pt -> let count = getSurrounding3 state pt
                                                 match Set.contains pt state with
                                                   | false -> match count with
                                                                | 3 -> Some pt
                                                                | _ -> None
                                                   | true  -> match count with
                                                                | 2 | 3 -> Some pt
                                                                | _ -> None)
                       |> Set.ofList

    state2
  state0 |> applyFn stepb 6 |> Set.count

// part b is just a ~~simple extension~~ copy of part a; takes 13s locally.
let day17b (input : string list) =
  let state0 = preprocess4 input

  let stepa state =
    let ((xl,xu),(yl,yu),(zl,zu), (wl,wu)) = bounds4 state
    let index = [for x in [xl-1 .. xu+1] do
                  for y in [yl-1 .. yu+1] do
                   for z in [zl-1 .. zu+1] do
                    for w in [wl-1 .. wu+1] do
                    yield (x,y,z,w)]
    let state2 = index |> List.choose (fun pt -> let count = getSurrounding4 state pt
                                                 match Set.contains pt state with
                                                   | false -> match count with
                                                                | 3 -> Some pt
                                                                | _ -> None
                                                   | true  -> match count with
                                                                | 2 | 3 -> Some pt
                                                                | _ -> None)
                       |> Set.ofList

    state2
  state0 |> applyFn stepa 6 |> Set.count