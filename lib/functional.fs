module Nimble.Functional

let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let ( &&& ) f g a = (f a, g a)
let ( *** ) f g (a, b) = (f a, g b)


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
