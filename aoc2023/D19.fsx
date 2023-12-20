#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open FSharp.Collections
open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC
open System.Text.RegularExpressions


type Result = Goto of string | Accept | Reject
type Step =
  | Conditional of string * bool * int64 * Result   // bool TRUE = GT
  | Otherwise of Result

let parse xs =
  let pr (x: string) =
    let (name::step::_)= x.Split([|'{'; '}'|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let steps = step.Split(',', StringSplitOptions.RemoveEmptyEntries)
                      |> Array.map (fun q -> if q.Contains(':')
                                                then let (elt::num::dest::_) = words q
                                                     let r = match dest with | "A" -> Accept | "R" -> Reject | o -> Goto o
                                                     Conditional(elt, q.Contains('>'), int64 num, r)
                                                else let r = match q with | "A" -> Accept | "R" -> Reject | o -> Goto o
                                                     Otherwise r)
                      |> List.ofArray
    Choice1Of2 (name,steps)


  let pp (x: string) = let w = alphaWords x in
                       let v = ints64 x
                       let t = List.sum v
                       List.zip w v |> List.append ["total", t] |> Map.ofList |> Choice2Of2
  let p1 (x: string) = if x[0] = '{' then pp x else pr x

  let parsed = xs |> List.filter (fun x -> x <> "") |> List.map p1
  let rules = parsed |> List.choose (function (Choice1Of2 x) -> Some x | _ -> None) |> Map.ofList
  let parts = parsed |> List.choose (function (Choice2Of2 x) -> Some x | _ -> None)
  rules, parts

let process1 rules (part2: Map<string, int64>) : Result =
  let f flag = if flag then (>) else (<)
  let rec execute (step::rest) (part: Map<string, int64>) =
    let a = match step with
              | Conditional (n, isGT, v, action) -> if (f isGT) part[n] v then Some action else None
              | Otherwise action  -> Some action
    match a with
      | Some aa -> aa
      | None    -> execute rest part

  let rec step rule (part1: Map<string, int64>) =
    let r = Map.find rule rules
    match execute r part1 with
      | Goto rule2 -> step rule2 part1
      | a          -> a

  step "in" part2

let day19a =
  let input = yourPuzzleInput 2023 19 false
  let rules, parts = input |> parse
  parts |> List.map (fun part -> match process1 rules part with
                                   | Accept -> part["total"]
                                   | Reject -> 0
                                   | _ -> failwith "bug")
        |> List.sum


let preprocess rules =
  let expand (nm, steps) =
    let children = [1..List.length steps] |> List.map (fun i -> sprintf "%s%d" nm i)
                      |> List.append [nm]
                      |> List.pairwise
    List.map2 (fun (s1, s2) f -> s1, (f, Goto s2)) children steps

  rules |> Map.toList
        |> List.collect expand
        |> Map.ofList


let omnipart n = Map.ofList <| [
                            "x", (1L, n)
                            "m", (1L, n)
                            "a", (1L, n)
                            "s", (1L, n)
                            ]

let split nm isGT n (part: Map<string, int64 * int64>) =
  let lo, hi = part[nm]
  let r1 = (lo, n-(if isGT then 0L else 1L))
  let r2 = (n+(if isGT then 1L else 0L), hi)
  // could be invalid. deal with it later
  let m1 = part |> Map.add nm r1
  let m2 = part |> Map.add nm r2
  m1, m2


let calculate2 (rules : Map<string, Step * Result>) =
  let score part = debugn part |> Map.toList |> List.map (fun (_, (a,b)) -> max 0L (b - a + 1L)) |> List.fold1 (*)

  let rec step nm x =
    let check, otherwise = rules[nm]
    match check with
      | Conditional (var, isGT, n, result) -> let m1, m2 = split var isGT n x
                                              let mT = if isGT then m2 else m1
                                              let mF = if isGT then m1 else m2
                                              let bT = match result with
                                                          | Accept  -> score mT
                                                          | Reject  -> 0
                                                          | Goto n2 -> step n2 mT
                                              let bF = match otherwise with
                                                          | Accept  -> score mF
                                                          | Reject  -> 0
                                                          | Goto n2 -> step n2 mF
                                              bT + bF
      | Otherwise Accept      -> score x
      | Otherwise Reject      -> 0L
      | Otherwise (Goto nm2)  -> step nm2 x

  step "in" (omnipart 4000L)

let day19b =
  let input = yourPuzzleInput 2023 19 false
  let rules = input |> parse |> fst |> preprocess |> debugn
  calculate2 rules
