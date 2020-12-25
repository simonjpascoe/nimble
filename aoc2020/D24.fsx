#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System
open System.Text.RegularExpressions


let input24A = [
  "sesenwnenenewseeswwswswwnenewsewsw"
  "neeenesenwnwwswnenewnwwsewnenwseswesw"
  "seswneswswsenwwnwse"
  "nwnwneseeswswnenewneswwnewseswneseene"
  "swweswneswnenwsewnwneneseenw"
  "eesenwseswswnenwswnwnwsewwnwsene"
  "sewnenenenesenwsewnenwwwse"
  "wenwwweseeeweswwwnwwe"
  "wsweesenenewnwwnwsenewsenwwsesesenwne"
  "neeswseenwwswnwswswnw"
  "nenwswwsewswnenenewsenwsenwnesesenew"
  "enewnwewneswsewnwswenweswnenwsenwsw"
  "sweneswneswneneenwnewenewwneswswnese"
  "swwesenesewenwneswnwwneseswwne"
  "enesenwswwswneneswsenwnewswseenwsese"
  "wnwnesenesenenwwnenwsewesewsesesew"
  "nenewswnwewswnenesenwnesewesw"
  "eneswnwswnwsenenwnwnwwseeswneewsenese"
  "neswnwewnwnwseenwseesewsenwsweewe"
  "wseweeenwnesenwwwswnew"
]

// everything starts white = 0
let flip = function | 1 -> 0 | 0 -> 1

// grid definition
// moving e/w moves 2 units
// moving the diagonals is 1 x and 2 y units

let preprocess (input: string list) =
  input |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

let rec walk (px, py) (path : char list) =
  if List.isEmpty path
    then (px, py)
    else match path.[0] with
           | 'e' -> walk (px+2, py) path.[1..]
           | 'w' -> walk (px-2, py) path.[1..]
           | 's' -> match path.[1] with
                      | 'e' -> walk (px+1, py-1) path.[2..]
                      | 'w' -> walk (px-1, py-1) path.[2..]
           | 'n' -> match path.[1] with
                      | 'e' -> walk (px+1, py+1) path.[2..]
                      | 'w' -> walk (px-1, py+1) path.[2..]

let calculateInitialFloor (input: string list) =
  let paths = preprocess input
  let state0 = Map.empty

  let f floor path =
    let target = walk (0,0) path
    floor |> Map.tryFind target
          |> Option.defaultValue 0
          |> flip
          |> fun v -> Map.add target v floor

  List.fold f state0 paths

let day24a (input : string list) =
  calculateInitialFloor input
    |> Map.toList
    |> List.sumBy snd

// game-of-life... again
let day24b (input : string list) =
  let floor0 = calculateInitialFloor input

  let gen6Coords (x, y) =
    [(x-2, y);   (x+2, y);
     (x+1, y-1); (x+1, y+1);
     (x-1, y-1); (x-1, y+1)]

  let evolve state =
    // found the bounds and define create xy ranges to check
    let ss = state |> Map.toList |> List.map fst
    let (mix, max) = ss |> List.map fst |> Seq.minmax
    let (miy, may) = ss |> List.map snd |> Seq.minmax
    let range = [for x in [mix-2 .. max+2] do for y in [miy-1 .. may+1] -> (x,y)]

    range |> List.map (fun p ->
                let ns = gen6Coords p
                          |> List.sumBy (fun p -> Map.tryFind p state |> Option.defaultValue 0)
                let b = state |> Map.tryFind p
                              |> Option.defaultValue 0
                let b2 = match b with | 1 -> if ns = 0 || ns > 2 then 0 else 1
                                      | 0 -> if ns = 2 then 1 else 0
                (p, b2))
          |> Map.ofList

  applyNtimes evolve 100 floor0 |> Map.toList |> List.sumBy snd