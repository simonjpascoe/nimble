let day1a (input : string list) = input |> List.sumBy (int >> fun x -> x / 3 - 2)

let fuel w = List.unfold (fun x -> let y = x/3 - 2 in if y > 0 then Some (y, y) else None ) w
              |> List.sum

let day1b (input : string list) = input |> List.sumBy (int >> fuel)