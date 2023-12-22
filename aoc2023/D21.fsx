#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions
open System.IO
open System.Threading

let HASH = -2
let DOT  = -1

let parse mapping (xs: string list) : 'a array2d=
  Array2D.init (String.length xs[0]) (List.length xs) (fun x y -> mapping |> Map.find (xs.[x].[y]))

let pad2d value (matrix: 'a array2d) =
  let R = Array2D.length1 matrix
  let C = Array2D.length2 matrix
  Array2D.init (R+2) (C+2) (fun r c -> if (r = 0 || r=R+1 || c=0 || c=C+1) then value else matrix[r-1,c-1])

let disp (array: 'a array2d) =
  let iMax = Array2D.length1 array - 1
  let jMax = Array2D.length2 array - 1
  let vals = [for i in [0..iMax] do yield [|0..jMax|] |> Array.map (fun j -> string array[i,j]) |> String.concat ""]
  vals

let step n grid =
  let m = n + 1
  let R = Array2D.length1 grid - 1
  let C = Array2D.length2 grid - 1
  let g = grid |> Array2D.mapi (fun r c v -> let xs = adjacents false (0, R) (0, C) (r, c)
                                             let vs = xs |> List.fold (fun s (r1, c1) -> s || grid[r1,c1] = n) false
                                             if vs && v <> HASH then m else v)
  int m, g

let count n grid =
  let summation = ref 0
  grid |> Array2D.iter (fun v -> if v = n then summation.Value <- summation.Value + 1 else ())
  summation.Value

let tile n m grid =
  let R = Array2D.length1 grid
  let C = Array2D.length2 grid
  Array2D.init (R*n) (C*m) (fun r c -> grid[r % R, c % C])

let convertBack n mp grid =
  let w = grid |> Array2D.map (fun v -> if v = n then 'O' else mp |> Map.tryFind v |> Option.defaultValue '.')
  w

let day21a =
  let input = yourPuzzleInput 2023 21 false
  let mp = ['#',HASH; '.',DOT; 'S', 0] |> Map.ofList
  let grid0 = input |> parse mp |> pad2d -2
  let n, grid1 = applyFn (fun (i, g) -> step i g) 64 (0, grid0)
  count n grid1

let day21b =
  // did the below testing to generate various outputs
  // counted how many A and B repeat tiles there are per line
  // found overlap hard to determine, so did testing
  // worked out the gap using the power series differences
  let steps = 26501365L
  // remove the size of the tile/2 = 65, then the number of tesselation = 131
  let k = (steps - 65L) / 131L
  // then count the tiles, A = 2k(k+1) + 1, B = 2k(k+1)
  let Acount = 3778L
  let Bcount = 3804L
  let over_estimate = ((2L * k) * (k + 1L) + 1L) * Acount + 2L * k * (k + 1L)*Bcount
  // but there is an overlap since the brute force approach shows lower count
  // work out that overlap is 176k*k+97k by math (sequence differences!)
  let adjustment = 176L * k * k + 97L * k

  // result!
  over_estimate - adjustment

// let day21b =
//   let input = yourPuzzleInput 2023 21 false
//   // let mp = ['#','#'; '.',' '; 'S', 'O'] |> Map.ofList
//   // let grid0 = input |> parse mp ' ' |> tile 3 3
//   // let a = (Array2D.length1 grid0 - 1) / 2
//   // let b = (Array2D.length2 grid0 - 1) / 2
//   // grid0[a, b] <- 'S'
//   // grid0
//   // File.WriteAllLines ("d:\\test1.txt", disp grid0 )
//   let k = 5
//   let g = 2 * k + 1
//   let mp = ['#',-2; '.',-1; 'S', -1] |> Map.ofList
//   let grid0 = input |> parse mp |> tile g g

//   let a = (Array2D.length1 grid0 - 1) / 2
//   let b = (Array2D.length2 grid0 - 1) / 2
//   grid0[a, b] <- 0
//   //grid0[a, b-131] <- 0
//   //grid0[a, b-131-131] <- 0

//   //grid0[a-66, b-65] <- 0


//   //grid0[a-65, b+65] <- 0
//   //grid0[a-65, b+130+65] <- 0
//   //grid0[a-65, b-130-65] <- 0
//   //grid0[a-65, b-130-65-130] <- 0
//   let n, grid1 = applyFn (fun (i, g) -> step i g) (65+k*131) (0, grid0)
//   let mp2 = [-2, '#'; -1, '.'] |> Map.ofList
//   let grid2 = convertBack n mp2 grid1
//   File.WriteAllLines ("d:\\test1.txt", disp grid2)
//   count 'O' grid2
