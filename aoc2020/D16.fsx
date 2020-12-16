#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

let preprocess (input: string list) =
  let marker1 = input |> List.findIndex(fun x -> x = "your ticket:")

  // params
  let ranges = input |> List.take (marker1 - 1)
  let rx = Regex("(.*): (\w*)-(\w*) or (\w*)-(\w*)")
  let ps = ranges |> List.map (fun ln -> let ms = rx.Matches(ln).[0].Groups |> List.ofSeq
                                         let tag = ms.[1].Value
                                         let nums = ms |> List.skip 2 |> List.map (fun x -> int64 (x.Value))
                                         (tag, nums.[0], nums.[1], nums.[2], nums.[3]))

  // tickets
  let splitTicket (t : string) = t.Split(",") |> Array.map int64 |> Array.toList
  let myTicket = input.[marker1 + 1] |> splitTicket
  let otherTickets = input |> List.skip (marker1 + 4) |> List.map splitTicket

  ps, myTicket, otherTickets


let day16a (input: string list) =
  let ranges, ticket, others = preprocess input
  let check t =
    ranges |> List.fold (fun s (_, a,b,x,y) -> s || betweenLR a b t || betweenLR x y t) false
  let scan ts = ts |> List.filter (check >> not)
  others |> List.collect scan |> List.sum

let day16b (input: string list) =
  let ranges, ticket, others = preprocess input
  // first remove all invalid tickets (using part 1 copy)
  let check t =
    ranges |> List.fold (fun s (_, a,b,x,y) -> s || betweenLR a b t || betweenLR x y t) false
  let scan ts = ts |> List.map check
  let valids = others |> List.filter (scan >> List.fold (&&) true)

  // now we can solve part 2.
  // for each column in ticket, which of the parameters does it satisfy?

  let validFor t =
    ranges |> List.filter (fun (r, a,b,x,y) -> betweenLR a b t || betweenLR x y t)
           |> List.map (fun (r, _,_,_,_) -> r)
           |> Set.ofList

  let analysis0 = valids |> List.map (List.map validFor)
                         |> List.transpose
                         |> List.map (Set.intersectMany)

  let state0 = (analysis0, [])

  let satisfies1 state determined =
    let analysis = state |> List.mapi (fun ix s -> ix, s, Set.count s = 1)
    let newDetermined = analysis |> List.filter (fun (i,j,k) -> k) |> List.map (fun (i, j, k) -> i, j |> Set.toList |> List.head)
    let removals = newDetermined |> List.map snd |> Set.ofList
    let state2 = state |> List.map (fun s -> s - removals)
    let determined2 = determined @ newDetermined
    (state2, determined2)

  let finished state = state |> List.fold (fun s t -> s && Set.isEmpty t) true
  let index = state0 |> List.unfold (fun (s, d) -> if finished s
                                                    then None
                                                    else let (s2,d2) = satisfies1 s d
                                                         Some (d2, (s2, d2)))
                     |> List.last

  // now look for 'departure' fields and multiply
  index |> List.filter (fun (i, s) -> s.StartsWith("departure"))
        |> List.fold (fun s (i, _) -> s * ticket.[i] ) 1L