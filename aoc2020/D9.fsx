

let day9a (input : string list) psize =
  let numbers = input |> List.map int64
  let w = numbers |> List.windowed (psize+1)

  // find the first number in last place not a summation of 2 of
  // the previous per window

  let check win =
    let pre = win |> List.take psize
    let post = win |> List.last
    let sums = [for a in pre do
                 for b in pre do
                 yield a + b]
    let valid = List.contains post sums
    (valid, post)

  w |> List.map check |> List.filter (fun (a,b) -> a = false)


let input9A = [
  "35"
  "20"
  "15"
  "25"
  "47"
  "40"
  "62"
  "55"
  "65"
  "95"
  "102"
  "117"
  "150"
  "182"
  "127"
  "219"
  "299"
  "277"
  "309"
  "576"
]
let day9b (input : string list) =
  let numbers = input |> List.map int64
  let length = List.length numbers
  let badnumber = 257342611L // from part 1

  List.unfold (fun (left, sz) -> let win = List.skip left numbers |> List.take sz
                                 let sum = List.sum win
                                 if sum = badnumber
                                  then Some (List.min win + List.max win, (length-1, 1))
                                  else if left+sz >= length
                                       then None
                                       else if sum > badnumber then Some (sum, (left+1, 1))
                                                               else Some (sum, (left, sz+1)))
              (0, 1)


