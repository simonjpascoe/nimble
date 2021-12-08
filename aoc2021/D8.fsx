#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let parse (input: string list) =
  let g (s: string) = s.ToCharArray() |> Set.ofArray
  let f (s: string) = s.Split(' ') |> Array.map g |> List.ofArray
  input |> List.map (f >> (List.take 10 &&& List.skip 11))

let deduce1 xs =
  match Set.count xs with
    | 2 -> Choice1Of2 1
    | 3 -> Choice1Of2 7
    | 4 -> Choice1Of2 4
    | 5 -> Choice2Of2 (set [2;3;5])
    | 6 -> Choice2Of2 (set [0;6;9])
    | 7 -> Choice1Of2 8
    | _ -> failwith "illogical"

let deduce ((xs : Set<char> list), vals) =
  // easy ones
  let ys = xs |> List.map (deduce1 &&& id)
  let f1 = function | Choice1Of2 x, y -> Some (x,y) | _ -> None
  let f2 = function | Choice2Of2 x, y -> Some (x,y) | _ -> None
  let em = ys |> List.choose f1 |> Map.ofList
  let uk = ys |> List.choose f2

  let rminus p i ns (xs, s) =
    let v = p |> Map.tryFind i
    match v with
      | None -> (xs, s)
      | Some j -> match Set.isSubset j s with
                    | true -> (xs - set ns, s)
                    | false -> (xs, s)

  let rint p i ns (xs, s) =
    let v = p |> Map.tryFind i
    match v with
      | None -> (xs, s)
      | Some j -> match Set.isSubset j s with
                    | true -> (Set.intersect xs (set ns), s)
                    | false -> (xs, s)

  // we know 1,4,7,8
  // need to deduce 0,2,3,5,6,9
  let r1 p = rminus p 1 [2;5;6]
  let r4 p = rint p 4 [9]
  let r3 p = rint p 3 [9]
  // if we know 4, then can check if 5 is possible
  let r245 p (xs, s) =
    let c1 = xs = set [2;5]
    let c2 = p |> Map.tryFind 4
    match (c1, c2) with
      | true, Some j -> let r = s - j
                        match Set.count r with
                          | 2 -> (set [5], s)
                          | 3 -> (set [2], s)
                          | _ -> failwith "illogical"
      | _ -> (xs, s)

  let step (known, unknown) =
    let f = r1 known >> r4 known >> r3 known >> r245 known
    let x, y = List.map f unknown |> List.partition (fun s -> Set.count (fst s) = 1)
    let known2 = [Map.toList known; x |> List.map (first Set.minElement)] |> List.concat
    let ks = List.map fst known2 |> Set.ofList
    let unknown2 = y |> List.map (first (fun js -> js - ks))
    known2 |> Map.ofList, unknown2

  let pred (_, y) = List.isEmpty y
  let keys = applyUntil step pred (em, uk)
  let multi (xs: int list)= 1000 * xs.[0] + 100 * xs.[1] + 10*xs.[2] + xs.[3]
  match keys with
    | Choice2Of2 _ -> failwith "insufficient logic"
    | Choice1Of2 (ks, _) -> let ks2 = ks |> Map.toList |> List.map flip |> Map.ofList
                            vals |> List.map (fun v -> Map.find v ks2) |> multi
