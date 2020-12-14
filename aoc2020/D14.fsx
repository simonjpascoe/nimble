#load @"./../lib/math.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.AOC

open System
open System.Text.RegularExpressions


let input14A = [
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  "mem[8] = 11"
  "mem[7] = 101"
  "mem[8] = 0"
]

let input14B = [
  "mask = 000000000000000000000000000000X1001X"
  "mem[42] = 100"
  "mask = 00000000000000000000000000000000X0XX"
  "mem[26] = 1"
]

let zeroMask = (String.replicate 36 "0").ToCharArray()

type Input = BitMask of char array | MemSet of int64 * int64

let applyMask (mask : char array) (value: int64) =
  let bin1 = Convert.ToString(value, 2)
  let len1 = bin1.Length
  let padded = (String.replicate (36-len1) "0" + bin1) |> fun x -> x.ToCharArray()
  let masked = Array.zip mask padded
                 |> Array.map (fun (m,v) -> if m = 'X' then v else m)
  Convert.ToInt64(String.Join("", masked), 2)

let applyMask2 (mask : char array) (address: int64) =
  let bin1 = Convert.ToString(address, 2)
  let len1 = bin1.Length
  let padded = (String.replicate (36-len1) "0" + bin1) |> fun x -> x.ToCharArray()

  // number of outputs is 2^number of X as each X can be 0 or 1 at the end
  // first apply mask, leaving X in place
  let masked0 = Array.zip mask padded
                  |> Array.map (fun (m,v) -> match m with | '0' -> v | n -> n)

  let glue (input : char array) : int64 = Convert.ToInt64(String.Join("", input), 2)

  // now need to fan out all the X over [0,1] versions...
  // fan 1st 'X' detected
  let fan1 (input : char array) : (char array list) =
    let indexX = Array.IndexOf(input, 'X')
    match indexX with
      | -1 -> [input]
      |  i -> let i1 = Array.copy input
              let i2 = Array.copy input
              i1.[i] <- '0'
              i2.[i] <- '1'
              [i1; i2]

  let rec resolveFloating (inputs : char array list) =
    let inputs1 = inputs |> List.collect fan1
    match List.length inputs = List.length inputs1 with
      | true -> inputs1
      | false -> resolveFloating inputs1

  resolveFloating [masked0] |> List.map glue

let preprocess (input: string list) =
  let parse (s: string) =
    if s.StartsWith("mask")
      then BitMask (s.Substring(7).ToCharArray())
      else let re = Regex.Match(s, "mem\[(\d*)\] = (\d*)")
           MemSet (re.Groups.[1].Value |> int64, re.Groups.[2].Value |> int64)
  input |> List.map parse

let day14a (input : string list) =
  let commands = preprocess input
  let memory0 = Map.empty : Map<int64, int64>

  let state0 = (zeroMask, memory0)
  let apply (mask, memory) t =
    match t with
      | BitMask ax       -> (ax, memory)
      | MemSet (addr, v) -> (mask, Map.add addr (applyMask mask v) memory)

  commands |> List.fold apply state0
           |> snd
           |> Map.toList
           |> List.sumBy snd



let day14b (input : string list) =
  let commands = preprocess input
  let memory0 = Map.empty : Map<int64, int64>

  let state0 = (zeroMask, memory0)
  let apply (mask, memory) t =
    match t with
      | BitMask ax       -> (ax, memory)
      | MemSet (addr, v) ->
          let addrs = applyMask2 mask addr
          let m2 = addrs |> List.fold (fun s addr -> Map.add addr v s) memory
          (mask, m2)

  commands |> List.fold apply state0
           |> snd
           |> Map.toList
           |> List.sumBy snd
