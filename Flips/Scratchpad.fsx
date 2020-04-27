#load "Domain.fs"
//#load "Solve.fs"
#load "SliceMap.fs"

open System
open Flips.Domain
open Flips.SliceMap

let f = SMap2.ofList [
    (1, "a"), 12.0; (1, "b"), 12.0; (1, "c"), 12.1; 
    (2, "a"), 13.0; (2, "b"), 11.0; (2, "c"), 12.3; 
    (3, "a"), 14.0; (3, "b"), 11.5; (3, "c"), 12.4; 
]

//let x = .> "CHicken"

//f.[1, "a"]
f.[All, "a"]
f.[All, GreaterThan "a"]
f.[All, GTE "a"]
f.[GT 2, "c"]
f.[Equals 2, LT "b"]
f.[All, All]

[1 .. 10].ToString()