module Nimble.Math

let pmod a b =
  let r = a % b
  if r < 0L then r + b else r

let iabs i = if i < 0 then -i else i

let extendedEuclidean a b =
  let r0 = (a, b)
  let s0 = (1L, 0L)
  let t0 = (0L, 1L)

  let step ((rp, r), (sp, s), (tp, t)) =
    if r = 0L
      then None
      else let q = rp / r
           let next = ((r, rp - q * r),
                       (s, sp - q * s),
                       (t, tp - q * t))
           Some (next, next)

  let ((gcd,_),(m1, _), (m2, _)) = (r0, s0, t0) |> List.unfold step |> List.last
  (gcd, m1, m2)

let product = List.fold (*) 1

// good for small numbers here without optimisations for overflows
let fact n = List.fold (*) 1 [1..n]
let nCr n r = (fact n) / ((fact r) * (fact (n-r)))
let nPr n r = (fact n) / (fact (n-r))