let divs n = 
    let c = float n |> sqrt |> floor |> int
    let ds = [1]::[for i in [2..c] do if n % i = 0 then yield [n / i; i]]
    ds |> List.collect id |> List.distinct

let v1 = [1..10000] |> List.map (divs >> List.sum)
let v2 = v1 |> List.mapi (fun i v -> if v-1 < 10000 && v1[v-1] = i+1 && i+1 <> v then Some [i+1;v] else None)
            |> List.choose id
            |> List.collect id
            |> List.distinct
            |> List.sum