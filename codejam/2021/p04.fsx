// median sort

open System
open System.IO



type State<'a> =
  | Continue of 'a
  | Error of string
  | Done of string

let p a =
  if a >= 10 then Done "11" else Continue (a+1)


let rec repl f state =
  match f state with
    | Continue a   -> repl f a
    | Done s       -> printfn "Done %A" s
                      //exit 0
    | Error r      -> printfn "Error %A" r
                      //exit 1

let read3i () =
  Console.ReadLine()
  |> fun s -> s.Split(" ")
  |> Array.map int
  |> Array.take 3
  |> fun ([|a;b;c|]) -> (a,b,c)

let listen () =
  let c = Console.ReadLine() |> int
  match c with
    | -1  -> failwith "FaIlEd!"
    | a   -> a

let ask a b c = printfn "%i %i %i" a b c
                listen ()

let solve1 n q =

  let loop state =
    let it, st = state
    if it >= q then failwith "Iterations exceeded"
      else match st with
             | Choice2Of2 soln -> soln |> Array.map (sprintf "%i")
                                       |> fun xs -> String.Join(" ", xs)
                                       |> printfn "%s"
                                       |> listen |> ignore
             | Choice1Of2 xs
                -> match xs with [] -> ask 1 2 3

  loop (0, Choice1Of2 [])


let run () =
  let (t,n,q) = read3i()
  printfn "%A" (t,n,q)

//repl p 0