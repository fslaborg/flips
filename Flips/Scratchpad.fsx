#load "Domain.fs"
#load "Solve.fs"

open Flips.Domain
open Flips.Solve

let x = Decision.create (DecisionName "Chicken") DecisionType.Boolean
let e = 1.0 * x
let e2 = e + 2.0
let e3 = e2 + x
let badX = Decision.create (DecisionName "Chicken") (DecisionType.Integer (1.0, 2.0))
e3 + badX

