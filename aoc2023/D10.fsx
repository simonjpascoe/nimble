#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions


let g2a = List.map List.toArray >> List.toArray
let g2l = Array.map Array.toList >> Array.toList

let disp = (List.map (List.toArray >> String)) >> Array.ofList >> fun xs -> String.Join(Environment.NewLine, xs)

let parse pc input sr =
  let grid = input |> List.map (fun (x : string) -> x.ToCharArray() |> Array.toList)
                   |> pad_matrix pc
                   |> g2a

  let start = grid |> Array.mapi (fun row line -> line |> Array.mapi (fun col xs -> if xs = 'S' then Some (row, col) else None))
                   |> Array.collect id
                   |> Array.choose id
                   |> Array.head
  // replace the starting space with the right value and pad the dataset
  let r, c = start
  grid.[r].[c] <- sr
  (grid, Array.length grid - 1, Array.length grid[0] - 1), (r, c)

let linkMap =
  let ML, MR, MU, MD, NO = "L-F", "J-7", "7|F", "J|L", ""
  // up, left, down, right possibilities
  [
    '|', [MU; NO; MD; NO]
    '-', [NO; ML; NO; MR]
    'F', [NO; NO; MD; MR]
    'J', [MU; ML; NO; NO]
    'L', [MU; NO; NO; MR]
    '7', [NO; ML; MD; NO]
  ]
   |> Map.ofList

let follow1 (grid: char array array, x1, y1) (row, col) =
  let adjs = adjacents false (0, x1) (0, y1) (row, col)
  let curr = grid.[row].[col]// up, left, down, right
  let values = adjs |> List.map (fun (a, b) -> (grid.[a].[b], (a,b)))
                    |> List.zip (Map.find curr linkMap)
                    |> List.filter (fun (m, (v, loc)) -> m.Contains(v))
                    |> List.map (snd >> snd)
  (List.head &&& List.last) values

let trace grid start cell1 =
  let rec step prev now path =
    let a, b = follow1 grid now
    let next = if a = prev then b else a
    if next = start
      then path
      else step now next (List.append path [next])
  step start cell1 [start; cell1]

let day10a =
  let input = loadInput 2023 10
  let grid, start = parse '.' input '|'
  // set initial state (current, steps-to-get-here), [map of (cells, steps)]
  // and then cleverly unfold the path until both ends are in the known set
  // trace the two paths back to the start
  let p1, p2 = follow1 grid start
  let route1 = trace grid start p1
  let route2 = trace grid start p2

  List.zip route1 route2 |> List.mapi (fun i (a, b) -> if a = b then Some i else None)
                         |> List.choose id
                         |> List.last

let extend spare (g : char array array) =
  // semi-"zoom in" cols then rows to give a gap between all walls
  // corners need to stay put, so we have to convert as necessary into extra | or -
  // anything not mapped gets converted to a ! symbol
  let hmap = [ '-', '-'; 'F', '-'; 'L', '-' ] |> Map.ofList
  let vmap = [ '|', '|'; 'F', '|'; '7', '|' ] |> Map.ofList
  let hzoom = g |> Array.map (fun col -> col |> Array.map (fun x -> match Map.tryFind x hmap with | Some n -> [|x;n|] | None -> [|x;spare|]) |> Array.concat)
  let vzoom = hzoom |> Array.transpose
                    |> Array.map (fun col -> col |> Array.map (fun x -> match Map.tryFind x vmap with | Some n -> [|x;n|] | None -> [|x;spare|]) |> Array.concat)
                    |> Array.transpose
  vzoom

let day10b =
  let input = loadInput 2023 10
  let grid, start = parse '.' input '|'
  let g0, x0, y0 = grid
  let p1, _ = follow1 grid start
  let route1 = trace grid start p1

  let g1 = g0 |> Array.mapi (fun r col -> col |> Array.mapi (fun c x -> if List.contains (r,c) route1 then x else if x = '!' then '.' else '.'))
  let g2 = extend ' ' g1 |> g2l |> pad_matrix 'O' |> g2a

  let update g =
    g |> Array.mapi (fun r col -> col |> Array.mapi (fun c x -> let adj = adjacents false (0, 2*x0+2) (0, 2*y0+2) (r, c)
                                                                let O = adj |> List.map (fun (a,b) -> g.[a].[b])
                                                                            |> List.contains 'O'
                                                                if x = '.' || x = ' ' then (if O then 'O' else x) else x))
  let g3 = applyUntilSteady update g2
  g3 |> Array.concat
     |> Array.filter (fun x -> x = '.')
     |> Array.length