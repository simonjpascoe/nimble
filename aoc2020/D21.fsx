#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"
#load @"./../lib/aoc.fs"

open Nimble.Math
open Nimble.Functional
open Nimble.AOC

open System
open System.Text.RegularExpressions

let input21A = [
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  "sqjhc fvjkl (contains soy)"
  "sqjhc mxmxvkd sbzzf (contains fish)"
]

let preprocess (input: string list) =
  let f (line : string) =
    let words = line.Split(',', ' ', '(', ')') |> Array.filter (fun x -> x <> "") |> List.ofArray
    let split = words |> List.findIndex (fun t -> t = "contains")
    words.[0..split-1], words.[split+1..]
  input |> List.map f

let analyse (input: string list) =
  let food = preprocess input
  let ingredients = food |> List.collect fst |> List.distinct
  let allergens = food |> List.collect snd |> List.distinct

  // for a allegen to be in the ingredient, all foods containing this ingredient must also have the allergen
  // each ingredient contains 0 or 1 allergen, and each allegen -> ingredient is fixed
  let check allergen =
    let ingredients = food |> List.filter (fun (i, a) -> a |> List.contains allergen)
    let common = ingredients |> List.map (fst >> Set) |> Set.intersectMany
    common

  // <allergen, possible ingredient list>
  let possibleIngredients = allergens |> List.map (fun i -> i, check i) |> Map.ofList
  food, ingredients, allergens, possibleIngredients

let day21a (input : string list) =
  let food, ingredients, allergens, possibleIngredients = analyse input
  let missingIngredients = Set ingredients - (possibleIngredients |> Map.toList |> List.map snd |> Set.unionMany)

  let everything = food |> List.collect fst |> List.countBy id
  let count = everything |> List.sumBy (fun (i, n) -> if Set.contains i missingIngredients then n else 0)

  count

let day21b (input : string list) =
  let food, ingredients, allergens, possibleIngredients = analyse input

  // loop set reduction, take out what is known, like day 16, so update D16 code (I see a pattern...)
  let state0 = (possibleIngredients, [])

  let satisfies state determined =
    let det, others = state |> Map.partition (fun _ v -> Set.count v = 1)
                            |> first (Map.map (fun k v -> v |> Set.toList |> List.head) >> Map.toList)
    let dets = det |> List.map snd |> Set
    let state2 = others |> Map.map (fun k v -> v - dets)
    let determined2 = determined @ det
    (state2, determined2)

  let finished = Map.fold (fun s _ t -> s && Set.isEmpty t) true
  let result =
    state0
      |> List.unfold (fun (s, d) -> if finished s
                                      then None
                                      else let (s2, d2)= satisfies s d
                                           Some (d2, (s2, d2)))
      |> List.last


  result |> List.sortBy fst |> List.map snd |> String.concat ","