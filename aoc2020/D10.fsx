// part 1 solved on a hunch about taking series differences after recognising that
// it basically put together an ordering. solved in excel; repeated here for fun
// (it's more effort here)
let day10a (input : int64 list) =
  let modified = [0L] @ input @ (input |> List.max |> fun x -> [x+3L])
  let differences = modified |> List.sort |> List.pairwise |> List.map (fun (x,y) -> y - x)
  let ones = differences |> List.filter (fun x -> x = 1L) |> List.length
  ones * (List.length differences - ones)

// mostly extended from excel version
// take each difference list, on each 3, how many adapters laid before hand
// work out the the number of combinations possible for those adapters, per group
// then product all of groups together to get the permutations
// (somewhat more annoying to write in code than excel...)
let day10b (input : int64 list) =
  let modified = [0L] @ input @ (input |> List.max |> fun x -> [x+3L])
  let differences = modified |> List.sort |> List.pairwise |> List.map (fun (x,y) -> y - x)

  let combinations = [1L; 1L; 2L; 4L; 7L]
  differences |> List.fold (fun (n,xs) t -> if t = 3L then (0,xs @ [n]) else (n+1, xs)) (0, [])
              |> snd
              |> List.map (fun x -> combinations.[x])
              |> List.fold (*) 1L
