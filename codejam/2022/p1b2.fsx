open System

let debugn a = eprintfn "%A" a; a

// LOADERS
let readInput1 () =
  let count = Console.ReadLine() |> int
  Array.init count (fun i -> let np = Console.ReadLine()
                             let (n::p::[]) = np.Split(' ') |> Array.map int |> Array.toList
                             Array.init n (fun i -> let xs = Console.ReadLine()
                                                    xs.Split(' ') |> Array.map int64)
  )

// PROBLEM
// Need to minimise the cost per customer = monotonic but which way?
// Need to minimise the between customer = affects the monotonicity as well
// There is also an initial setup cost -> insert a 0th customer of [0] products to handle 

// SOLUTION
let pump (customers : int64[][]) =
  // use dijsktra!
  let stats = customers |> Array.map (fun xs -> let a = Array.min xs
                                                let b = Array.max xs
                                                let c = b - a
                                                a,b,c)
  let s0 = ((0L, 0L), (0L, 0L))   // (sum so far, dial position)
  
  let step ((incT, incP), (decT, decP)) (a,b,c) =
    let incM = min (abs (incP - a) + c + incT) (abs (decP - a) + c + decT)
    let decM = min (abs (incP - b) + c + incT) (abs (decP - b) + c + decT)
    ((incM, b), (decM, a))
  let ((c0,_), (c1,_)) = Array.fold step s0 stats
  min c0 c1

// RUNNER
readInput1 () |> Array.mapi (fun i customers -> printfn "Case #%d: %d" (i+1) (pump customers))