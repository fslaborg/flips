#load "Domain.fs"
#load "Solve.fs"

open System
open Flips.Domain
open Flips.Solve

// let x = Decision.create (DecisionName "Chicken") DecisionType.Boolean
// let e = 1.0 * x
// let e2 = e + 2.0
// let e3 = e2 + x
// let badX = Decision.create (DecisionName "Chicken") (DecisionType.Integer (1.0, 2.0))
// e3 + badX

// let x1 = Decision.createContinuous "x1" 0.0M Decimal.MaxValue
// let x2 = Decision.createContinuous "x2" 0M Decimal.MaxValue

// let objExpr = 2.0 * x1 + 3.0 * x2
// let objective = Objective.create "Get big" Maximize objExpr
// let model = 
//     Model.create objective
//     |> Model.addConstraint (x1 <== 10.0)
//     |> Model.addConstraint (x2 <== 5.0)
//     |> Model.addConstraint (x1 + x2 <== 8.0)

// let settings = {
//     SolverType = SolverType.CBC
//     MaxDuration = 30_000L
//     WriteLPFile = Some "Test.lp"
// }

// let r = solve settings model

// let c2 = x2 <== 5.0
// let c3 = x1 + x2 <== 8.0