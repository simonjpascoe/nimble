module D3

open System.IO

let input0 = [
  "..##......."
  "#...#...#.."
  ".#....#..#."
  "..#.#...#.#"
  ".#...##..#."
  "..#.##....."
  ".#.#.#....#"
  ".#........#"
  "#.##...#..."
  "#...##....#"
  ".#..#...#.#"
]

let input1 = File.ReadAllLines("./inputs/d3_input.txt")
              |> List.ofArray


let day3a (input : string list) step_right step_down =
  let height = input |> List.length
  let width  = input.[0].Length

  let ys = [0 .. step_down .. height-1]
  let xs = List.init (List.length ys) (fun i -> (i * step_right) % width)
  let coords = List.zip ys xs

  let isTree (y,x) = input.[y].[x] = '#'
  coords |> List.filter isTree |> List.length |> int64

let routes = [
  1, 1
  3, 1
  5, 1
  7, 1
  1, 2
]

let day3b input rs =
  rs |> List.map (fun (a,b) -> day3a input a b) |> List.fold (*) 1L