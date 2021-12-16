#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC

let ex1 = "D2FE28"
let ex2 = "38006F45291200"
let ex3 = "EE00D40C823060"

let decode (input: string) =
   input.ToCharArray()
     |> Array.map (fun c -> Convert.ToInt32(String([|c|]), 16)
                             |> fun h -> Convert.ToString(h, 2).PadLeft(4, '0'))
     |> fun ax -> String.Join("", ax).ToCharArray()

type Packet =
  | Literal of int * int
  | Operator of int * int * int * Packet array

// this is a char parser... (yey FParsec! except not using it here...)
let rec parsePacket (stream: char array) =
  debugn stream |> ignore
  let version, rest = streamTake 3 stream |> first (String >> bin2int)
  let typeid, rest = streamTake 3 rest |> first (String >> bin2int)

  let subparse (stream: char array) =
    let step (s0, has_ended) =
      if has_ended
        then None
        else let s1, r1 = parsePacket s0
             Some (s1, (r1, Array.length r1 < 7))
    Array.unfold step (stream, false)

  match typeid with
    | 4 -> // need to take as many 5 char sets until the one starts with 0 which is
           // last value marker, except for padding zeros.
           let consume (s0, has_ended) =
             if has_ended
               then None
               else let s1, r1 = streamTake 5 s0
                    let s11, s12 = streamTake 1 s1
                    match s11.[0] with
                     | '0' -> Some ((s12 |> String, Some r1), (r1, true))
                     | '1' -> Some ((s12 |> String, None), (r1, false))
                     | _ -> failwith "illogical"
           let xs = Array.unfold consume (rest, false)
           let value = xs |> Array.map fst
                          |> fun xs -> String.Join("", xs)
                          |> bin2int
           let stream2 = xs |> Array.last |> snd |> Option.get

           Literal (version, value), stream2
    | p -> let ltypeid, rest = streamTake 1 rest
           match ltypeid.[0] with
             | '0' -> let totalbits, rest = streamTake 15 rest |> first (String >> bin2int)
                      let subpackets, rest = streamTake totalbits rest
                      Operator (version,p,0,subparse subpackets), rest
             | '1' -> let totalbits, rest = streamTake 11 rest |> first (String >> bin2int)
                      let subpackets, rest = streamTake totalbits rest
                      Operator (version,p,1,subparse subpackets), rest
             | _ -> failwith "illogical"


//decode ex3 |> parsePacket
