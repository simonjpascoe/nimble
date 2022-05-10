(*
    Considering n^2 + an + b, |a| <= 1000, |b| <= 1000
    Which combination of (a,b), yields the most consequtive primes starting at n = 0?

    1) b must be a prime number
    2) a + b > 0

    nb, Factor to f(n) = n(n+a) + b
*)

let edebugn x = eprintfn "%A" x; x

let q n a b = n*n + a*n + b

let primes n = List.unfold (
    function | [] -> Some (2, [2])
             | st -> let x = List.head st
                     let ys = Seq.initInfinite (fun i -> x + 1 + i)
                     let y = ys |> Seq.pick (fun u -> match List.tryPick (fun s -> if u % s = 0 then Some u else None) st with
                                                        | None -> Some u
                                                        | _    -> None)
                     if y <= n then Some (y, y::st) else None) []

let ps2 = primes 100000 |> Set.ofList

let count a b =
  let ns = Seq.initInfinite ((+) 0)
  let vals = ns |> Seq.takeWhile (fun n -> Set.contains (q n a b) ps2)
  vals |> Seq.length

let solve () =
  let ps = primes 1000
  let bs = (List.map (fun x -> -x) ps) @ ps
  let aas = [-1000..1000]
  let coeffs = [for a in aas do 
                for b in bs do
                let p40 = q 40 a b
                if a + b > 0 && Set.contains p40 ps2 then yield a,b]
  coeffs |> List.map (fun (a,b) -> (a,b), (count a b))
         |> List.maxBy snd