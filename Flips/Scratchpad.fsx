#load "Domain.fs"
//#load "Solve.fs"
#load "SliceMap.fs"

open System
open Flips.Domain
open Flips.SliceMap

let f1 = SMap2.ofList [
    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
    (2, "a"), 1.0; (2, "b"), 2.0; (2, "c"), 3.0; 
    (3, "a"), 1.0; (3, "b"), 2.5; (3, "c"), 3.0; 
]

let f2 = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]

f1 + f2
f1 .* f2

let x = SMap.ofList [for i in 1..3 -> i, float i]
let y = SMap.ofList [for i in 1..3 -> i, 2.0 * float i]

x + y
x .* y

f1.[All, "b"] .* x
f1.[1, "b"]
//let x = .> "CHicken"

//f.[1, "a"]
