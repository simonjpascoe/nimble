#load "18.fsx"
open System.IO

let data67 =
  let triangle = File.ReadAllLines @"./euler/inputs/p067_triangle.txt"
  triangle |> Array.map (fun (xs: string) -> xs.Split(' ') |> Array.map int |> Array.toList) |> Array.toList