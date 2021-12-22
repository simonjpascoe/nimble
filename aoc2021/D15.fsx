#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

type P2 = int * int
type Grid = int array array

let parse = List.map (fun (s: string) -> s.ToCharArray() |> Array.map char2int) >> List.toArray

// optimised store for the open set?

type PriorityQueue<'a,'b> = ('a * 'b) list

module PriorityQueue =
  let insert (p: 'a) (v: 'b) (pq : PriorityQueue<'a, 'b>) =
    let posn = List.tryFindIndex (fun (i, _) -> i > p) pq
    match posn with
      | Some o -> pq.[0..o-1] @ (p,v) :: pq.[o..]
      | None   -> pq @ [p,v]

  let pop (pq: PriorityQueue<'a, 'b>) : (('a*'b) * PriorityQueue<'a,'b>) option=
    match pq with | (x::xs) -> Some (x, xs)
                  | [] -> None

  let contains (v: 'b) pq = pq |> List.tryFind (fun (_, i) -> i = v) |> Option.isSome

// ignoring the actual path solution for now, this is transcribed from wikipedia
let astar (gridfn : int -> int -> int) (h: (P2 -> float) option) (start : P2) (goal: P2) =
  let xbound = (0, fst goal)
  let ybound = (0, snd goal)

  let defaultCost = 1.0e99
  let openSet0 : PriorityQueue<float, P2> = [0.0, start]
  let gScore0 = [start, 0.0] |> Map.ofList
  let fScore0 = [start, Option.fold (fun s hh -> s + hh start) 0.0 h] |> Map.ofList

  let ld p m d = m |> Map.tryFind p |> Option.defaultValue d

  let updateN (cc, gs, fs, os) (tx, ty) =
    let ts = cc + float(gridfn tx ty)
    if ts > ld (tx,ty) gs defaultCost
      then (cc, gs, fs, os)   // no update
      else let gs2 = gs |> Map.add (tx,ty) ts
           let fscore = Option.fold (fun s hh -> s + hh (tx, ty)) ts h
           let fs2 = fs |> Map.add (tx,ty) fscore
           let os2 = if PriorityQueue.contains (tx,ty) os
                       then os
                       else os |> PriorityQueue.insert fscore (tx, ty)
           (cc, gs2, fs2, os2)

  let step (openSet, gScore, fScore) =
    let res = PriorityQueue.pop openSet
    match res with
      | None -> None
      | Some ((_, current), openSet2) ->
          if current = goal
            then None // "completed"
            else let cc = gScore |> Map.find current
                 let neighbours = adjacents false xbound ybound current
                 let _, gScore2, fScore2, openSet3 = neighbours |> List.fold updateN (cc, gScore, fScore, openSet2)
                 Some (openSet3, gScore2, fScore2)
  let state0 = (openSet0, gScore0, fScore0)

  List.unfold (fun s -> step s |> Option.map (fun s->s,s)) state0 |> List.last

let day15a (grid: Grid) =
  let n = (grid |> Array.length) - 1
  astar (fun x y -> grid.[x].[y]) None (0,0) (n,n)

let day15b (grid: Grid) tiling =
  let n = grid |> Array.length
  // supersize me, but without wasting time building the whole array
  let gridfn x y = let xtile = x / n
                   let ytile = y / n
                   let v = grid.[x % n].[y % n] + xtile + ytile
                   if v > 9 then v - 9 else v
  let n2 = (tiling * n) - 1
  astar gridfn None (0,0) (n2,n2)
