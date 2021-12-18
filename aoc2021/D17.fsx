#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

// parameterise everything by t
// for vx = initial x velocity
// for vy = initial y velocity

// x(t) = -0.5 * t * t + (vx + 0.5) * t when t < vx
//      =  0.5 * vx * (vx + 1)               otherwise
// y(t) = -0.5 * t * t + (vy + 0.5) * t

let X0 = (20, 30)
let Y0 = (-10, -5)

let X = (248, 285)
let Y = (-85, -56)

let f t v = (-t * t + (2 * v + 1) * t) / 2
let g t v = if t >= v then f v v else f t v

//let fd t v = -t + v + 0.5
//let fdd t v = -1.0

let day17a (x1,x2) (y1, y2) =
  // only need to consider positive vy (otherwise 0 is the maximum)
  // for any v, the zero velocity will be the top of y-arc, i.d. fd(t;v) = 0
  // so this is when t = v + 0.5
  // the f function is symmetric around the apex, so the maximum vy
  // would occur as the maxium value that gets to y1, which means
  // at t-1 it has be at 0 and at travelling at y1 speed again
  // so the maximum vy is going to be max y1 y2 -1
  let max_vy = (max (abs y1) (abs y2)) - 1
  //let t = max_vy + 0.5
  // although we're in integer land, so it's going to be either max_vt, or at max_vy+1
  // but at it is quadratic, almost certainly going to be the same at both points
  max (f max_vy max_vy) (f (max_vy+1) max_vy)


let day17b (x1, x2) (y1, y2) =
  // we can bound the vx and vy values
  let yb = (min y1 y2, -(min y1 y2) - 1)
  // calculate all viable t for each y in the range
  // given the quadratic nature, max t is 2 (max y + 1)
  let ty = [for t in [1..2 * (snd yb + 1)] do
             for v in [fst yb .. snd yb] do
              let y = f t v
              if y >= y1 && y <= y2 then yield (t, v)]
           |> List.groupBy fst
           |> List.map (second (List.map snd))

  // now we can do similar for vx, noting that t is unbound as x stops moving
  // find all viable t such that t lies in the ty set and the x value is in the right range
  // note that for t > vx, it's stopped and so there is a t available.

  let ws = [for (t, ys) in ty do
            for v in [1 .. x2] do
              let x = g t v
              if x >= x1 && x <= x2 then yield [for y in ys do yield (v, y)]]
             |> List.concat
             |> List.distinct
             |> List.length
  ws


day17b X0 Y0 |> printfn "%A"
day17b X Y |> printfn "%A"

