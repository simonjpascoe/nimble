open System.IO
open System
open System.Text.RegularExpressions

let input5 = File.ReadAllLines "./inputs/d5_input.txt"
              |> List.ofArray

// zero based
let maxrow = 127
let maxcol = 7

let split upper (a,b) input =
  let half = (b-a) / 2
  if input = upper
    then (b-half, b)
    else (a, a+half)

let decode (identification : string) =
  let i2 = identification.ToCharArray()
  let fb = i2 |> Array.take 7
  let lr = i2 |> Array.skip 7
  let (row,_) = fb |> Array.fold (fun s t -> split 'B' s t) (0, maxrow)
  let (col,_) = lr |> Array.fold (fun s t -> split 'R' s t ) (0, maxcol)
  row, col

let day5a input =
  input |> List.map (decode >> fun (r,c) -> r*8 + c)
        |> List.max
let day5b input = 739 // solved with pen'n'paper
