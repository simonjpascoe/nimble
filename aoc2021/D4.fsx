#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System.Text.RegularExpressions

let parse (input: string list) =
  // first line is the input sequence of numbers
  let xs = input.[0] |> ints
  // five rows per board, set an initial state of -1 for each number
  // indicating that it has not been drawn yet
  let ys = input |> List.skip 1
                 |> List.map (ints >> List.map (fun a -> (a, -1)))
                 |> List.filter (List.isEmpty >> not)
                 |> List.chunkBySize 5
  (xs, ys)

let day4 firstlast (stream, boards) =
  // play bingo. just run over the stream, mark state, check winner
  // for part 1, we want the first winning board
  // then calculate the result number
  // assume someone wins first and is unique (for now)
  let stream2 = List.mapi (fun i s -> (s, i)) stream

  let update1 (board: ((int * int) list) list) ((number, position) : int * int) =
    board |> List.map (List.map (fun (a, s) -> if a = number then (a, position) else (a, s)))

  let check1 (board : ((int * int) list) list) =
     let win = List.filter (fun (_, i) -> i = -1) >> List.isEmpty
     let hc = List.map win board
     let vc = List.map win (List.transpose board)
     hc @ vc |> List.tryFind (fun x -> x) |> Option.isSome

  let value (board : ((int * int) list) list) =
    // sum of all undrawn numbers
    let f = List.map (fun (a, i) -> if i = -1 then a else 0)
    let v = List.collect f board
    List.sum v

  match firstlast with
    | true -> (boards, stream2)
                      |> List.unfold (fun (bs, s::xs) -> let bs2 = bs |> List.map (fun b -> update1 b s)
                                                         let ws = List.tryFind check1 bs2 |> Option.map value
                                                         match ws with
                                                           | Some b -> debugn (b * fst s) |> function _ -> None
                                                           | None   -> Some (0, (bs2, xs))
                      ) |> ignore
              None
    | false ->  let boards2 = List.map (fun b -> (b, (-1, -1))) boards
                let bs2 = List.fold (fun s t -> s |> List.map (fun (board, n) -> match n with
                                                                                   | (-1, _) -> let board2 = update1 board t
                                                                                                match check1 board2 with
                                                                                                  | true -> (board2, t)
                                                                                                  | false -> (board2, (-1, -1))
                                                                                   | _ -> (board, n))) boards2 stream2
                let last, (k, _) = bs2 |> List.maxBy (fun (_, (_, n)) -> n)
                Some (value last * k)


