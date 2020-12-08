module D7

let input7A = [
  "light red bags contain 1 bright white bag, 2 muted yellow bags."
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  "bright white bags contain 1 shiny gold bag."
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  "faded blue bags contain no other bags."
  "dotted black bags contain no other bags."
]

let input7B = [
  "shiny gold bags contain 2 dark red bags."
  "dark red bags contain 2 dark orange bags."
  "dark orange bags contain 2 dark yellow bags."
  "dark yellow bags contain 2 dark green bags."
  "dark green bags contain 2 dark blue bags."
  "dark blue bags contain 2 dark violet bags."
  "dark violet bags contain no other bags."
]

let readRule (rule : string) =
  // (adj) (adj) bags contain [n (adj) (adj) bag(s)]
  let split1 = rule.IndexOf("bags contain ")
  let front = rule.Substring(0, split1-1) |> fun x -> x.Split(" ") |> fun x -> (x.[0], x.[1])
  let terminal = rule.IndexOf("no other bags") <> -1
  if terminal
    then (front, [])
    else let backs = rule.Substring(split1 + 13)
                       |> fun x -> x.Split(", ")
                       |> List.ofArray
                       |> List.map (fun x -> x.Split(" ") |> fun y -> (y.[0] |> int, y.[1], y.[2]))
         front, backs

let day7a input =
  let values = input |> List.map readRule |> Map.ofList
  // flatten it
  let rec flatten1 (a1, a2) : (string * string) list =
    let contents = values.[(a1, a2)]
    contents |> List.collect (fun (n, b1, b2) -> [b1,b2] @ flatten1 (b1, b2))
  values |> Map.map (fun k v -> flatten1 k)
         |> Map.filter (fun k v -> v |> List.contains ("shiny", "gold"))
         |> Map.toList
         |> List.length

let day7b input =
  let values = input |> List.map readRule |> Map.ofList

  let rec count (a1, a2) =
    let bag = values.[a1, a2]
    1 + (bag |> List.sumBy (fun (n, b1, b2) -> n * count (b1, b2)))

  count ("shiny", "gold") - 1