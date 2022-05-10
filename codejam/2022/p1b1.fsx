open System

// LOADERS
let readInput1 () =
  let count = Console.ReadLine() |> int
  Array.init count (fun i -> Console.ReadLine() |> ignore
                             let xs = Console.ReadLine()
                             xs.Split(' ') |> Array.map int
  )

// SOLUTION
let serve (pancakes: int array) =

  let go (i, j, p, n) _ =
    let a = pancakes.[i]
    let b = pancakes.[j] 
    if a < b
      then (i+1, j, max a p, if a >= p then n+1 else n)
      else (i, j-1, max b p, if b >= p then n+1 else n)
    
  let (_, _, _, n) = Array.fold go (0, Array.length pancakes - 1, 0, 0) pancakes
  n


// RUNNER
readInput1 () |> Array.mapi (fun i cakes -> printfn "Case #%d: %d" (i+1) (serve cakes))