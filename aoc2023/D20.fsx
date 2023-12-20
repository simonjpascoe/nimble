#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions
open System.IO

// true = high signal, false = low signal

type Node = | Broadcaster of string list
            | Flipflop of bool * string list
            | Conjunction of Map<string, bool> * string list
            | Endpoint

let parse (xs: string list) =
  let p1 (x: string) =
    let x0 = x[0]
    let (name::ds) = words x
    name, match x0 with
            | '%' -> Flipflop (false, ds)
            | '&' -> Conjunction (Map.empty, ds)
            | 'b' -> Broadcaster ds

  let pass1 = xs |> List.map p1
  // now link the conjunction nodes and set their initial state (low)
  let pass2 = pass1 |> List.map (function | (name, Conjunction(_, ds)) -> let inputs = pass1 |> List.choose (fun (name2, node) -> match node with
                                                                                                                                    | Flipflop (_, ds2)    -> if List.contains name ds2 then Some name2 else None
                                                                                                                                    | Conjunction (_, ds2) -> if List.contains name ds2 then Some name2 else None
                                                                                                                                    | Broadcaster ds2      -> if List.contains name ds2 then Some name2 else None)
                                                                          let state = inputs |> List.map (fun i -> i, false) |> Map.ofList
                                                                          (name, Conjunction (state, ds))
                                          | x -> x)
  // add any orphan endpoint nodes that do not prop
  let mentioned = pass2 |> List.map fst
  let pass3 = pass2 |> List.collect (snd >> function | Broadcaster ds -> ds | Flipflop (_, ds) -> ds | Conjunction (_, ds) -> ds | Endpoint -> []) |> Set.ofList
                      |> Set.filter (fun a -> not (List.contains a mentioned))
                      |> Set.toList
                      |> List.map (fun n -> n, Endpoint)
                      |> List.append pass2
  pass3 |> Map.ofList

let prop0 network from signal name =
  let node = network |> Map.find name
  match node with
    | Broadcaster ds          -> ds |> List.map (fun d -> (name, signal, d)), network
    | Flipflop (state, ds)    -> if signal then ([], network)
                                           else let network2 = network |> Map.add name (Flipflop (not state, ds))
                                                (ds |> List.map (fun d -> (name, not state, d))), network2
    | Conjunction (state, ds) -> let state2 = state |> Map.add from signal
                                 let network2 = network |> Map.add name (Conjunction (state2, ds))
                                 let signal2 = Map.fold (fun s k v -> s && v) true state2 |> not
                                 (ds |> List.map (fun d -> (name, signal2, d))), network2
    | Tester                  -> [], network

let prop1 n network0 =
  let press network00 = applyUntil (fun (p0, n0, s0) -> List.mapFold (fun network (from, signal, destination) -> prop0 network from signal destination) n0 p0 |> first List.concat |> fun (pp, nn) -> (pp, nn, List.append s0 pp))
                          (fun (p, n, s) -> List.isEmpty p)
                          ([("button", false, "broadcaster")], network00, [("button", false, "broadcaster")])
                          |> function | Choice1Of2 (_, network1, signals) -> (network1, signals) | Choice2Of2 (_, network1, signals) -> (network1, signals)

  Seq.replicate n press |> Seq.mapFold (fun s0 fn -> let nn, s1 = fn s0
                                                     s1, nn) network0
                        |> first (Seq.collect id >> Seq.toList)

let prop2 network0 nodeC signalC =
  let press network00 = applyUntil (fun (p0, n0, s0) -> List.mapFold (fun network (from, signal, destination) -> prop0 network from signal destination) n0 p0 |> first List.concat |> fun (pp, nn) -> (pp, nn, List.append s0 pp))
                          (fun (p, n, s) -> List.isEmpty p)
                          ([("button", false, "broadcaster")], network00, [("button", false, "broadcaster")])
                          |> function | Choice1Of2 (_, network1, signals) -> (network1, signals) | Choice2Of2 (_, network1, signals) -> (network1, signals)

  applyUntil (fun (n, network, _) -> let n2, s2 = press network in (n+1, n2, s2))
             (fun (n1, _, signals) -> let flag = signals |> List.tryPick (fun (_,s,t) -> if (s = signalC) && (t = nodeC) then Some n1 else None)
                                      (if Option.isSome flag then debugn (nodeC, n1) else ("",1)) |> ignore
                                      false)
             (0, network0, [])

let day20a () =
  let input = yourPuzzleInput 2023 20 false
  let network = input |> parse
  let signals, newNetwork = prop1 1000 network
  let counts = signals |> Seq.countBy snd3 |> Seq.toList
  counts |> List.map snd |> List.fold1 (*)

let day20b =
  let input = yourPuzzleInput 2023 20 false
  let network = input |> parse
  prop2 network "dt" false

// draw a graph
let graph2 () =
  let network = yourPuzzleInput 2023 20 false |> parse
  let g = network |> Map.toList |> List.collect (fun (k,v) -> let colour, xs = match v with | Broadcaster ds      -> "blue", ds
                                                                                            | Flipflop (_, ds)    -> "red", ds
                                                                                            | Conjunction (_, ds) -> "green", ds
                                                                                            | Endpoint            -> "black", []
                                                              let n = sprintf "%s [color=%s]" (List.head (words k)) colour
                                                              let es = xs |> List.map (fun x -> sprintf "%s -> %s" k x)
                                                              n::es)
  File.WriteAllLines ("d:\\test1.txt", g)