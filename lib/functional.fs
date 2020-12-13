module Nimble.Functional

let first  f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let ( &&& ) f a b = (f a, f b)
let ( *** ) f g a b = (f a, g b)