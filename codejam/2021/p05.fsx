// xheating detection

open System
open System.IO


// 100 lines of 10000 chars (0/1)
let parseTestCase (tc : string array) = 
  tc |> Array.map (fun (s:string) -> s.ToCharArray() |> Array.map (fun x -> if x='1' then 1.0 else 0.0))

let rescale mi ma (xs: double array) =
  let x1 = Array.min xs
  let f = (ma-mi) / (Array.max xs - x1)
  xs |> Array.map (fun x -> mi + f * (x-x1))

let sigmoid s q = 1.0 / (1.0 + Math.Exp(s-q))

let v1 n (tc: float array array) =
  // both ranges for S_i and Q_j are [-3,3] inclusive
  // lets rescale these to be like this
  // how many questions did each person get right -> S_i estimator
  let A = tc |> Array.map Array.sum
  let rA = rescale -3.0 3.0 A
  // how many people got the question right -> Q_j estimation 
  let B = tc |> Array.transpose |> Array.map Array.sum
  let rB = rescale -3.0 3.0 B |> Array.map (fun x -> -x)
  // map each S/Q against the sigmoid function, by person (rows)
  let qq = rA |> Array.mapi (fun i s -> i, rB |> Array.map (fun q -> if sigmoid s q > 0.7 then 1.0 else 0.0) |> Array.sum)
              |> Array.sortByDescending snd
  eprintfn "%A" qq
  let cheater = qq |> Array.head |> fst
  printfn "Case #%d: %d" n (cheater + 1)


let run () =
    let t = Console.ReadLine() |> int
    let p = Console.ReadLine()
    let n = 100
    Array.init (t*n) (fun i -> Console.ReadLine())
      |> Array.chunkBySize n
      |> Array.mapi (fun i -> parseTestCase >> (v1 i))

let loadSample () =
  let ls = File.ReadAllLines "D:\\Code\\github\\nimble\\codejam\\2021\\cheating_detection_sample_ts1_input.txt"
  let tc = parseTestCase (Array.skip 2 ls)
  v1 1 tc

loadSample()