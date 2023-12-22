#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions

type D = N | E | S | W
type C = char array2d

let parse1 (xs: string list) : char array2d=
  Array2D.init (String.length xs[0]) (List.length xs) (fun x y -> xs[x][y])

// travel direction, item -> exit direction
let directionMap = Map.ofList [
  (N, '|'), [N]
  (E, '|'), [N;S]
  (S, '|'), [S]
  (W, '|'), [N;S]

  (N, '-'), [E;W]
  (E, '-'), [E]
  (S, '-'), [E;W]
  (W, '-'), [W]

  (N, '/'), [E]
  (E, '/'), [N]
  (S, '/'), [W]
  (W, '/'), [S]

  (N, '\\'), [W]
  (E, '\\'), [S]
  (S, '\\'), [E]
  (W, '\\'), [N]
]

let showPath (input:C) path =
  let cells = path |> Set.toList |> List.map (fst3 &&& snd3) |> Set.ofList
  Array2D.init (Array2D.length1 input) (Array2D.length2 input) (fun x y -> if Set.contains (x, y) cells then '#' else '.')

let trace1 (cave: C) d0 xy0=
  // trace the beam in direction d, having just arrived into at (x, y)
  // exit the cave okay
  // state is map of (x, y) -> D set
  //    (entry directions, output is always same if entry same)
  let Y = Array2D.length1 cave - 1
  let X = Array2D.length2 cave - 1

  let bc y x = (x >= 0) && (x<=X) && (y>=0) && (y<=Y)

  let mv d y x =
    match d with
      | N -> (y-1, x)
      | E -> (y, x+1)
      | S -> (y+1, x)
      | W -> (y, x-1)

  // returns the path taken, and any new starting points to trace due
  // to splits (rather than running in parallel)
  // d represents the direction travelling of the beam
  let rec trace (state, d, (y, x)) =
    match bc x y with
      | false -> state
      | true  -> let prev = Set.contains (y, x, d) state
                 match prev with
                   | true  -> state // loop detection
                   | false -> let state1 = Set.add (y,x,d) state
                              let exits = directionMap |> Map.tryFind (d, cave[y,x])
                                                       |> Option.defaultValue [d]
                              match exits with
                                | [exit]            -> trace (state1, exit, mv exit y x)
                                | (exit1::exit2::_) -> let state2 = trace (state1, exit1, mv exit1 y x)
                                                       let state3 = trace (state2, exit2, mv exit2 y x)
                                                       state3
                                | _ -> failwith "bug"

  trace (Set.empty, d0, xy0)


let day16a =
  let input = yourPuzzleInput 2023 16 false |> parse1
  let multipath = trace1 input E (0,0)
  let tileCount = multipath |> Set.toList |> List.map (fst3 &&& snd3) |> Set.ofList |> Set.count
  tileCount

let day16b =
  let input = yourPuzzleInput 2023 16 false |> parse1
  let Y = Array2D.length1 input - 1
  let X = Array2D.length2 input - 1
  let ys = [0..Y] |> List.collect (fun y -> [(E, y, 0); (W, y, X)])
  let xs = [0..X] |> List.collect (fun x -> [(S, 0, x); (N, Y, x)])
  let starts = List.concat [xs; ys]
  starts |> List.map (fun (d,y,x) -> trace1 input d (y, x) |> Set.toList |> List.map (fst3 &&& snd3) |> Set.ofList |> Set.count)
         |> List.max