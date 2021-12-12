#load @"./../lib/aoc.fs"

open System
open Nimble.AOC

let input11A = [
  "L.LL.LL.LL"
  "LLLLLLL.LL"
  "L.L.L..L.."
  "LLLL.LL.LL"
  "L.LL.LL.LL"
  "L.LLLLL.LL"
  "..L.L....."
  "LLLLLLLLLL"
  "L.LLLLLL.L"
  "L.LLLLL.LL"
]

type SM = {w:int; h:int; xs:char [,]}


let preprocess (input : string list) : SM =
  let y = List.length input
  let x = input.[0].Length
  let combined = input |> List.toArray |> Array.map (fun x -> x.ToCharArray()) |> array2D
  {w = x; h = y; xs = combined}

let step1 (map : SM) =
  let count1 r c =
    List.sum <| [for i in [max 0 (r-1) .. min (map.h-1) (r+1)] do
                 for j in [max 0 (c-1) .. min (map.w-1) (c+1)] do
                 if (i,j) <> (r,c) then
                  yield if map.xs.[i,j] = '#' then 1 else 0]
  let result =
    Array2D.init map.h map.w (fun r c ->
      let cnt = count1 r c
      match map.xs.[r,c] with | '.' -> '.'
                              | '#' -> if cnt >= 4 then 'L' else '#'
                              | 'L' -> if cnt = 0 then '#' else 'L')

  {map with xs = result}

let day11a (input : string list) =
  let seatmap = preprocess input
  let steady = seatmap |> List.unfold (fun s -> let m = step1 s
                                                if m.xs = s.xs
                                                   then None
                                                   else Some (m, m))
  steady |> List.last
         |> fun x -> x.xs
         |> Seq.cast<char>
         |> List.ofSeq
         |> List.countBy id

let step2 (map : SM) =
  // count the number of occupied seats closest on each of 8 directions
  // looking through floors
  let vectors = [for i in [-1..1] do for j in [-1..1] do if (i,j) <> (0,0) then yield (i,j)]
  let rec step dr dc r c =
      let r2, c2 = r+dr, c+dc
      if (betweenL 0 map.h r2) && (betweenL 0 map.w c2)
        then match map.xs.[r2, c2] with
               | '.' -> step dr dc r2 c2
               | e -> Some e
        else None

  let count1 r c =
    vectors |> List.map (fun (dr,dc) -> step dr dc r c)
            |> List.filter (fun x -> x = Some '#')
            |> List.length

  let result =
    Array2D.init map.h map.w (fun r c ->
     let cnt = count1 r c
     match map.xs.[r,c] with | '.' -> '.'
                             | '#' -> if cnt >= 5 then 'L' else '#'
                             | 'L' -> if cnt = 0 then '#' else 'L')

  {map with xs = result}

let day11b (input : string list) =
  let seatmap = preprocess input
  let steady = seatmap |> List.unfold (fun s -> let m = step2 s
                                                if m.xs = s.xs
                                                   then None
                                                   else Some (m, m))
  steady |> List.last
         |> fun x -> x.xs
         |> Seq.cast<char>
         |> List.ofSeq
         |> List.countBy id