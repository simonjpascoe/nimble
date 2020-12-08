module D8


open Nimble.Functional
open System.IO
open System


let input8 = File.ReadAllLines "./inputs/d8_input.txt"
              |> List.ofArray

let input8a = [
  "nop +0"
  "acc +1"
  "jmp +4"
  "acc +3"
  "jmp -3"
  "acc -99"
  "acc +1"
  "jmp -4"
  "acc +6"
]

let input8b = [
  "nop +0"
  "acc +1"
  "jmp +4"
  "acc +3"
  "jmp -3"
  "acc -99"
  "acc +1"
  "nop -4"
  "acc +6"
]

type Op =
  | Acc of int
  | Jmp of int
  | Nop of int

let parse (input : string list) =
  let p1 (line : string) =
    let cmd = line.Substring(0,3)
    let sgn = line.Substring(4,1) |> fun v -> if v = "-" then -1 else +1
    let arg = line.Substring(5) |> int
    let sarg = sgn * arg
    match cmd with
      | "acc" -> Acc sarg
      | "nop" -> Nop sarg
      | "jmp" -> Jmp sarg
      | _ -> failwith ("Bad cmd: " + cmd)
  input |> List.map p1

let day8a input =
  let instructions = parse input
  let pointerN = instructions |> List.length

  let tracker0 = Map.empty
  let pointer0 = 0
  let accumulator0 = 0

  let eval (p : int) (a : int) (t : Map<int, bool>) : (int * (int * int * Map<int, bool>)) option =
    if p >= pointerN then None
      else let i = instructions.[p]
           let chk = Map.tryFind p t
           match chk with
             | Some true  -> None
             | _ -> match i with | Acc v -> Some (a+v, (p + 1, a + v, Map.add p true t))
                                 | Jmp v -> Some (a, (p + v, a, Map.add p true t))
                                 | Nop v -> Some (a, (p + 1, a, Map.add p true t))

  List.unfold (fun (p, a, t) -> eval p a t) (pointer0, accumulator0, tracker0) |> List.last


let loopCheck (instructions : Op list) pointer0 =
  let pointerN = instructions |> List.length
  let tracker0 = Map.empty

  let eval p t =
    if p>=pointerN then None
    else let i = instructions.[p]
         let chk = Map.tryFind p t
         match chk with
           | Some true -> None
           | _ -> match i with | Acc _ -> Some (p+1, (p+1, Map.add p true t))
                               | Nop _ -> Some (p+1, (p+1, Map.add p true t))
                               | Jmp v -> Some (p+v, (p+v, Map.add p true t))

  let result = List.unfold (fun (p, t) -> eval p t) (pointer0, tracker0)
  if List.isEmpty result then false else result |> List.last |> fun i -> i < pointerN


let day8b input =
  // it's only worth flipping if the change makes a difference
  let instructions = parse input
  let pointerN = instructions |> List.length

  let pointer0 = 0
  let accumulator0 = 0

  let eval (p : int) (a : int) : (int * (int * int)) option =
    if p >= pointerN then None
      else let i = instructions.[p]
           match i with | Acc v -> Some (a+v, (p + 1, a + v))
                        | Jmp v -> match (not (loopCheck instructions (p+1))) && (loopCheck instructions (p+v)) with
                                     | true  -> Some (a, (p + 1, a))  // treat as nop
                                     | false -> Some (a, (p + v, a))
                        | Nop v -> match (loopCheck instructions (p+1)) && (not (loopCheck instructions (p+v))) with
                                     | true  -> Some (a, (p + v, a))  // treat as jmp
                                     | false -> Some (a, (p + 1, a))

  List.unfold (fun (p, a) -> eval p a) (pointer0, accumulator0) |> List.last

