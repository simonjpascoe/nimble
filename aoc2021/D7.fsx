#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC


let day7 p2 input =
  let xs = input |> List.head |> ints
  // an optimisation problem:
  // solve for position X such that sum(|x_i-X|) is minimal
  // then of course the movement should also be minimised
  // constraints for part 1 assume
  // X > 0, X <max(x_i)

  // HOWEVER for both parts, we can just brute force it quickly
  let minX = 0
  let maxX = List.max xs
  let objective X =
    match p2 with
      | false -> xs |> List.sumBy (fun x -> Math.Abs(x - X))
      | true -> let f n = (n * (n+1)) / 2
                xs |> List.sumBy (fun x -> f (Math.Abs(x-X)))
  let costs = [minX..maxX] |> List.map (fun x -> x, objective x)
  let optimal = List.minBy snd costs
  optimal