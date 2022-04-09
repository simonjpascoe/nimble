open System

// don't forget eprintf "%A" XYZ

let debugn a = printfn "%A" a; a

// LOADERS
let readInput1 () =
    let count = Console.ReadLine() |> Int32.Parse
    Array.init count (fun i -> Console.ReadLine())

// PROBLEM
// need to double a letter only if it would make it earlier in the lexographical ordering of the
// word, then move onto the next letter

// SOLUTION
let double (word: string) =
  let xs = word.ToCharArray() |> Array.rev
  Array.skip 1 xs |> Array.fold (fun st x -> let a = String [|x|] + st
                                             let b = String [|x;x|] + st
                                             if a < b then a else b)
                                ([|Array.head xs|] |> String)


// RUNNER
readInput1 () |> Array.mapi (fun i word -> printfn "Case #%d: %s" (i+1) (double word))