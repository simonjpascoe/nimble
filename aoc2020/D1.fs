module D1

open System.IO

let input = File.ReadAllLines("./inputs/d1_input.txt")
              |> List.ofArray
              |> List.map int

let result1 = [for a in input do
                for b in input do
                if a+b = 2020 then yield (a,b,a*b)]

let result2 = [for a in input do
               for b in input do
                for c in input do
                 if a+b+c = 2020 then yield (a,b,c,a*b*c)]

printfn "%A,%A" (result1, result2)