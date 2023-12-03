module Nimble.Functional

let flip (a, b) = (b, a)
let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let first3  f (a, b, c) = (f a, b, c)
let second3 f (a, b, c) = (a, f b, c)
let third3  f (a, b, c) = (a, b, f c)

let ( &&& ) f g a = (f a, g a)
let ( *** ) f g (a, b) = (f a, g b)

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let thd3 (_,_,c) = c

let fst4 (a,_,_,_) = a
let snd4 (_,b,_,_) = b
let thd4 (_,_,c,_) = c
let fth4 (_,_,_,d) = d

let applyFn f n state0 = List.replicate n f |> List.fold (fun s fn -> fn s) state0

let rec applyUntil f  pred state0 =
  let state1 = f state0
  match state0 = state1 with
    | true -> Choice2Of2 state0
    | false -> match pred state1 with
                 | true  -> Choice1Of2 state1
                 | false -> applyUntil f pred state1

let applyUntilSteady f state0 =
  match applyUntil f (fun _ -> false) state0 with
    | Choice1Of2 _ -> failwith "illogical"
    | Choice2Of2 x -> x

let rec unfoldUntil f pred state0 = 
  let result, state1 = f state0
  match state1 = state0 with
    | true  -> [result], state1
    | false -> match pred result state1 with
                 | true  -> [result], state1
                 | false -> let xs, sn = unfoldUntil f pred state1
                            result :: xs, sn

 
module Seq =
  let minmax xs = (Seq.min &&& Seq.max) xs
  let maxVvIx zv = Seq.fold (fun (i, j, msf) t -> if t > msf then (i+1, i, t) else (i+1, j, msf)) (0, -1, zv)
                     >> fun (i,j,k) -> (j, k)

module List =
  let fold1 fn xs =
    match xs with
      | [] -> failwith "Empty list provided, no state to lift"
      | x::xs -> List.fold fn x xs

  let rec treeReduce (fn: 'a->'a->'a) (xs: 'a list) =
    match xs with
      | [] -> failwith "Called with empty list"
      | [x] -> x
      | ys -> let ln = List.length ys / 2
              let ys1, ys2 = (List.take ln &&& List.skip ln) ys
              let zs1, zs2 = (treeReduce fn ys1, treeReduce fn ys2)
              fn zs1 zs2

  let map2di (fn: int -> int -> 'a -> 'b) (xs: 'a list list) =
    xs |> List.mapi (fun r cols -> cols |> List.mapi (fun c v -> fn r c v))

  let capfloor minv maxv = List.map (fun x -> max (min maxv x) minv)