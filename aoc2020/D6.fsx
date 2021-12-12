#load "./../lib/functional.fs"

open System
open Nimble.Functional

let input6A = [
  "abc";""
  "a";"b";"c";""
  "ab";"ac";""
  "a";"a";"a";"a";"";
  "b"
]

let preprocess = List.fold (fun (s1, s2) t -> if t <> "" then (s1, s2 @ [t]) else (s1 @ [s2], [])) ([], [])
                   >> fun (a,b) -> a @ [b]
                   >> List.map (fun xs -> (List.length xs, String.Join("", xs)))

let day6a =
  // count uniques and report per group
  preprocess >> List.sumBy (fun (_,s) -> s.ToCharArray() |> Array.distinct |> Array.length)

let day6b =
  // count each per group, report count per group which = group size
  preprocess >> List.sumBy (fun (sz, s) -> s.ToCharArray() |> Array.groupBy id
                                                           |> Array.map (second Array.length)
                                                           |> Array.filter (fun (_,b) -> b = sz)
                                                           |> Array.length)