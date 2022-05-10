let divs n = 
    let c = float n |> sqrt |> floor |> int
    let ds = [1]::[for i in [2..c] do if n % i = 0 then yield [n / i; i]]
    ds |> List.collect id |> List.distinct

let abuns = [1..28123] |> List.filter (fun n -> n < (divs n |> List.sum))

let solve () = 
    let possibles = [for i in abuns do for j in abuns do if i + j <= 28123 then yield i + j] |> List.distinct |> List.sum
    let total = 28123 * (28124 / 2)
    total - possibles