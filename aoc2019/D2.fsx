#load "./intcode.fsx"

open Intcode

let day2a (input : string list) =
  let code = input.[0].Split(",") |> Array.map int
  code.[1] <- 12
  code.[2] <- 2
  eval 0 code
  code.[0]

let day2b (input : string list) =
  let code0 = input.[0].Split(",") |> Array.map int
  [for noun in [0..99] do
    for verb in [0..99] do
      if run1 code0 noun verb = 19690720 then
       yield 100 * noun + verb]

