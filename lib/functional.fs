module Nimble.Functional

let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)
