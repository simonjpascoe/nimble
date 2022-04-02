// median sort


//  python D:\Code\github\nimble\codejam\interactive_runner.py python D:\Code\github\nimble\codejam\2021\p04_testing_tool.py 0 -- dotnet fsi D:\Code\github\nimble\codejam\2021\p04.fsx
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

let ask memory a b c = 
  let k = Set.ofList [a;b;c]
  match Map.tryFind k memory with 
    | Some m -> (memory, false, m)
    | None   -> printfn "%i %i %i" a b c
                let m = listen ()
                (Map.add k m memory, true, m)

let triples xs = let sz = Array.length xs - 2
                 let ts = List.init sz (fun i -> i, xs.[i..i+2])
                 ts

let swap (arr: int array) i j = 
  let k = arr.[j]
  arr.[j] <- arr.[i]
  arr.[i] <- k
  arr

let insertAt (arr: 'a array) (i:int) (v:'a) =
  let before = arr.[0..i-1]
  let mid = [|v|]
  let after = arr.[i..]
  Array.concat [before; mid; after]

let rec leftSearch memory (xs: int array) nx i = 
  let x1, x2 = xs.[i], xs.[i+1]
  let memory1, asked, mid = ask memory x1 x2 nx
  if mid = nx
    then insertAt xs (i+1) nx, memory1
    else leftSearch memory1 xs nx (i+1)


let rec loop (xs, ys, memory) =
  // eprintfn "%A" (xs, ys)
  // initial case
  if Array.length xs = 0
    then let memory1, _, m = ask memory 1 2 3
         let q = Set.ofList [1;2;3]
         let ys2 = ys - q
         loop <| match m with 
                   | 1 -> ([|2;1;3|], ys2, memory1)
                   | 2 -> ([|1;2;3|], ys2, memory1)
                   | 3 -> ([|1;3;2|], ys2, memory1)
    else 
      // check for completion
      if Set.count ys = 0
        then 
          let result = xs |> Array.map (sprintf "%i") |> fun s -> String.Join(" ", s)
          eprintfn "%s" result
          printfn "%s" result
          listen ()
        else 
          // find next number
          let nx = Set.minElement ys
          let ln = Array.length xs
          let r1, r2 = xs.[ln-2], xs.[ln-1]
          let l1, l2 = xs.[0], xs.[1]
          let memory1, askedr, midr = ask memory  r1 r2 nx
          let memory2, askedl, midl = ask memory1 l1 l2 nx

          let ys2a = ys - Set.singleton nx
          match true with 
            | _ when midr = nx -> loop (insertAt xs (ln-1) nx, ys2a, memory2)
            | _ when midl = nx -> loop (insertAt xs 1 nx, ys2a, memory2)
            | _ when midr = r2 -> loop (insertAt xs ln nx, ys2a, memory2)
            | _ when midl = l1 -> loop (insertAt xs 0 nx, ys2a, memory2)
            | _ -> let xs2, memory3 = leftSearch memory2 xs nx 0
                   loop (xs2, ys2a, memory3)


let solve1 t n q = 
  Array.init t (fun i -> loop (Array.empty, Set.ofArray (Array.init n (fun i -> i+1)), Map.empty))

let run () =
  let (t,n,q) = read3i()
  eprintfn "%A" (t,n,q)
  solve1 t n q

run()