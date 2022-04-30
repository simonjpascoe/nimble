open System

let debugn a = eprintfn "%A" a; a
let r = Random()

let nextR () = r.Next 7

let rotate r (s: string) = 
  let n = 8 - r
  s + s |> fun x -> x.[n..n+7]

let xor (a:string) (b:string) = String(Seq.map2 (fun x y -> if x = y then '0' else '1') a b |> Array.ofSeq)

let count = Seq.filter (fun i -> i = '1') >> Seq.length

// let universe = [0..255] |> List.map (fun x -> let b = sprintf "%8B" x |> fun s -> s.Replace(' ', '0')
//                                               x,b,count b)

let universe = [0..255] |> List.map (fun x -> let b0 = Convert.ToString(x, 2)
                                              let b = (String.replicate (8 - b0.Length) "0") + b0
                                              x,b,count b)

let step prev guess r =
  let W = rotate r guess
  let X = xor prev W
  X, count X

let choices n0 guess n1 = 
    universe |> List.filter (fun (_,b,c) -> (c = n1) && (count (xor guess b) = n0))

let solveCase c =
  let mutable guess = "00000000"
  Console.WriteLine(guess)
  let mutable n0 = Console.ReadLine() |> int
  let mutable n1 = n0
  while n1 <> 0
    do let guesses = choices n0 guess n1
       match guesses with 
         | [] -> guess <- "00000000"
                 Console.WriteLine(guess)
                 n0 <- Console.ReadLine() |> int
                 n1 <- n0
         | xs -> guess <- List.last xs |> fun (_,b,_) -> b
                 Console.WriteLine(guess)
                 n0 <- n1
                 n1 <- Console.ReadLine() |> int

let caseCount = Console.ReadLine() |> int

[1..caseCount] |> List.map solveCase