module Nimble.Functional

let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

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
