#load @"./../lib/math.fs"
#load @"./../lib/functional.fs"

open Nimble.Math
open Nimble.Functional

let preprocess (input : string) =
  input.Split(",")
    |> List.ofArray
    |> List.mapi (fun i x -> (i, x))
    |> List.choose (fun (i, x) -> if x = "x" then None else Some (int64 i, int64 x))

// solved in excel first - rank 808!
// would have been faster if i had multiplied correctly, but lost 1min penalty because of that
// code version added later for completeness
let day13a (input: string list) =
  let t0 = input.[0] |> int64
  let data = preprocess input.[1] |> List.map snd
  data |> List.map (fun b -> b, b - (t0 % b))
       |> List.minBy snd
       |> fun (a,b) -> a * b

// delayed because of hard coded input typo earlier, and idiocy and bad memory. zomg wow damn.
// earlier tests included a sieve solution which would work fine too really for the input size.
// unless there was a typo. oops -> don't hardcode the input data again
let day13b (input: string list) =
  let data = preprocess input.[1] |> List.sortByDescending snd

  let coefficients =
    data |> List.map (fun (index, bus) -> (pmod (bus - index) bus), bus)

  let solve (a1,n1) (a2, n2) =
    let (_,m1,m2) = extendedEuclidean n1 n2
    let res = (n2 * (pmod (a1 * m2) n1)) + (n1 * (pmod (a2 * m1) n2))
    res, n1*n2

  let result = coefficients |> List.treeReduce solve

  // run a check on the numbers
  // let check = coefficients |> List.map (fun (a,b) -> (fst result % b, a))
  fst result

// just for fun, let's bring back the infinite sequence version, which works fine for
// small sample inputs but blows up otherwise, i.e. on the real one.
let day13b2 (input : string list) =
  let data = preprocess input.[1] |> List.sortByDescending snd

  let sq o n = Seq.initInfinite (fun i -> o + int64 i * n)

  let (offset, bus)::ds = data
  let s0 = sq offset bus

  ds |> List.fold (fun s (o,b) -> s |> Seq.filter (fun i -> (i+o) % b = 0L)) s0
     |> Seq.head
