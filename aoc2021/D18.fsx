#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open System
open Nimble.Math
open Nimble.Functional
open Nimble.AOC


// work on snailfish numbers as elt lists, as writing out the ADT handler via tree is hard...
type elt = O of int | C of int | V of int * int

let deepen = function | O i      -> O (i+1)
                      | C i      -> C (i+1)
                      | V (i, v) -> V (i+1, v)

let parse (s: string) =
  s.ToCharArray() |> Array.mapFold (fun (depth, vs) c -> let cv () = match vs with
                                                                       | [] -> None
                                                                       | _  -> V (depth, (Convert.ToInt32(String(List.rev vs |> Array.ofList)))) |> Some
                                                         match c with
                                                           | '[' -> ([Some (O (depth+1))],    (depth+1 ,vs))
                                                           | ']' -> ([cv (); Some (C depth)], (depth-1, []))
                                                           | ',' -> ([cv ()],                 (depth,   []))
                                                           | v   -> ([],                      (depth,   c::vs)))
                                   (0, [])
    |> fst
    |> List.concat
    |> List.choose id

let repr snailfish =
  let p = function | O _ -> "["
                   | C _ -> "],"
                   | V (_, x) -> sprintf "%d," x

  snailfish |> List.map p |> fun s -> String.Join("", s).Replace(",]", "]")

let explode (snailfish: elt list) =
  // find the first 5 level deep no bracket = no explode
  // an exploding pair is the first O 5, V 5, V 5, C 5 sub pattern in the parsed string
  // which should also be the location of the first O 5
  let posn = snailfish |> List.tryFindIndex (fun x -> x = O 5)
  match posn with
    | None       -> None
    | Some posn1 -> let lportion = snailfish.[0..posn1-1]
                    let (v1, v2) = match snailfish.[posn1..posn1+3] with
                                     | ([_; V (_, v1); V (_, v2);_]) -> (v1, v2)
                                     | _ -> failwith (repr snailfish)
                    let rportion = snailfish.[posn1+4..]
                    let lidex = lportion |> List.tryFindIndexBack (function | V _ -> true | _ -> false)
                    let ridex = rportion |> List.tryFindIndex (function | V _ -> true | _ -> false)
                    let lportion2 = match lidex with
                                      | None -> lportion
                                      | Some li -> lportion |> List.mapi (fun i x -> if i <> li then x else let (V (d, v)) = x in V (d, v + v1))
                    let rportion2 = match ridex with
                                      | None -> rportion
                                      | Some ri -> rportion |> List.mapi (fun i x -> if i <> ri then x else let (V (d, v)) = x in V (d, v + v2))
                    Some (lportion2 @ [V (4, 0)] @ rportion2)

let split (snailfish: elt list) =
  let big = snailfish |> List.tryFindIndex (function | V (_,x) when x > 9 -> true | _ -> false)
  match big with
    | None -> None
    | Some idex -> let (V (d, v)) = snailfish.[idex]
                   let d2 = d+1
                   let v2a, v2b = let x = v/2 in if 2*x = v then (x, x) else (x, x+1)
                   let lportion = snailfish.[0..idex-1]
                   let mportion = [O d2; V (d2, v2a); V (d2, v2b); C d2]
                   let rportion = snailfish.[idex+1..]
                   Some (lportion @ mportion @ rportion)

let reduce1 snailfish =
  explode snailfish
    |> Option.defaultWith (fun () -> split snailfish
                                      |> Option.defaultValue snailfish)

let reduce snailfish = applyUntilSteady reduce1 snailfish

let add snail1 snail2 =
  let s = snail1 @ snail2 |> List.map deepen
  [O 1] @ s @ [C 1]

let magnitude snailfish =
  let collapse sn = [0..List.length sn - 4]
                      |> List.choose (fun i -> match sn.[i..i+3] with
                                                 | [O _; V (_, v1); V (_, v2); C _] -> Some (i, i+3, V (-1, 3*v1 + 2*v2))
                                                 | _ -> None)
                      |> List.rev
                      |> List.fold (fun s (i, j, v) -> s.[0..i-1] @ [v] @ s.[j+1..]) sn
  let collapsed = applyUntilSteady collapse snailfish |> List.head
  match collapsed with
    | V (_, v) -> v
    | _        -> failwith "illogical"

let day18a (input: string list) =
  let snails = input |> List.map parse
  let step s t = add s t |> reduce
  let result = snails |> List.fold1 step
  result |> magnitude

let day18b (input: string list) =
  let snails = input |> List.map parse
  let pairs = [for a in snails do
               for b in snails do
               if a <> b then yield [(a,b); (b,a)]] |> List.concat
  pairs |> List.map (fun (a,b) -> add a b |> reduce |> magnitude) |> List.max
