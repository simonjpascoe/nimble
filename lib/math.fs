module Nimble.Math

let pmod a b =
  let r = a % b
  if r < 0L then r + b else r

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