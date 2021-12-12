module D1

let day1a0 input = [for a in input do
                     for b in input do
                      if a+b = 2020 then yield (a,b,a*b)]

let day1b0 input = [for a in input do
                     for b in input do
                      for c in input do
                       if a+b+c = 2020 then yield (a,b,c,a*b*c)]

let day1a (input : string list) = input |> List.map int |> day1a0

let day1b (input : string list) = input |> List.map int |> day1b0
