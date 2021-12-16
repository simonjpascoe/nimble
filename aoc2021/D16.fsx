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
let ex4 = "8A004A801A8002F478"
let ex5 = "620080001611562C8802118E34"
let ex6 = "C0015000016115A2E0802F182340"
let ex7 = "A0016C880162017C3686B18A3D4780"

let decode (input: string) =
   input.ToCharArray()
     |> Array.map (fun c -> Convert.ToInt32(String([|c|]), 16)
                             |> fun h -> Convert.ToString(h, 2).PadLeft(4, '0'))
     |> fun ax -> String.Join("", ax)

type Packet =
  | Literal of int * int64
  | Operator of int * int * int * Packet list

let parseLiteralElement (stream: string) = 
  let isFinal = stream.[0] = '0'
  let content = stream.[1..4]
  (isFinal, content), stream.[5..]

let parseLiteral (stream: string) =
  // peek characters 345 to see if it is a literal
  let notLiteral = stream.[3..5] <> "100"
  if notLiteral then None
    else let version = stream.[0..2] |> fun s -> Convert.ToInt32(s, 2)
         let remaining = stream.[6..]
         let value, stream1 = unfoldUntil parseLiteralElement (fun (x,_) _ -> x) remaining
                                |> first (List.map snd >> fun xs -> String.Join("", xs) |> fun s -> Convert.ToInt64(s, 2))
         Some (Literal (version, value), stream1)

let rec parseOperator (stream: string) = 
  let parseEither s = match parseLiteral s with | None -> parseOperator s | a -> a
  if stream = "" then None
    else let optype = stream.[3..5] |> bin2int
         let isLiteral = optype = 4
         if isLiteral then None
           else let version = stream.[0..2] |> bin2int
                let ltype = stream.[6..6] |> bin2int
                match ltype with 
                  | 0 -> let len = stream.[7..21] |> bin2int
                         // easier to branch off here with the substring than count the consumed characters
                         let substream = stream.[22..22+len-1] 
                         let subs = List.unfold parseEither substream
                         Some (Operator (version, optype, ltype, subs), stream.[22+len..])
                  | 1 -> let count = stream.[7..17] |> bin2int
                         let subs, stream2 = [1..count] |> List.fold (fun (xs, s) t -> let r, s2 = parseEither s |> Option.get
                                                                                       (r::xs, s2)) ([], stream.[18..])
                         Some (Operator (version, optype, ltype, List.rev subs), stream2)
                  | _ -> failwith "illogical"

let parse (input: string) = 
  match parseLiteral input with 
    | None -> parseOperator input 
    | a -> a
    |> Option.get
    |> fst

let versionSum (input: string) = 
  let rec get = function | Literal (v, _) -> v
                         | Operator (v, _, _, xs) -> v + List.sumBy get xs
  parse input |> get

let rec eval packet = 
  match packet with 
    | Literal (_, v) -> v
    | Operator (_, op, _, xs) ->
        let ys = List.map eval xs
        match op with 
          | 0 -> List.sum ys
          | 1 -> List.fold (fun s t -> s * t) 1L ys
          | 2 -> List.min ys
          | 3 -> List.max ys
          | 5 -> if ys.[0] > ys.[1] then 1L else 0L
          | 6 -> if ys.[0] < ys.[1] then 1L else 0L
          | 7 -> if ys.[0] = ys.[1] then 1L else 0L

let day16a hex_input = hex_input |> decode |> versionSum
let day16b hex_input = hex_input |> decode |> parse |> eval
