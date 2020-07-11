module Flips.Examples.SimpleExample

open Flips
open Flips.Types

let solve () =
    let x1 = Decision.createContinuous "x1" 0.0 infinity
    let x2 = Decision.createContinuous "x2" 0.0 infinity
    
    let objExpr = 2.0 * x1 + 3.0 * x2
    let objective = Objective.create "Get big" Maximize objExpr
    let model = 
        Model.create objective
        |> Model.addConstraint (Constraint.create "Max x1" (x1 <== 10.0))
        |> Model.addConstraint (Constraint.create "Max x2" (x2 <== 5.0))
        |> Model.addConstraint (Constraint.create "Max x1 and x2" (x1 + x2 <== 12.0))
    
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 30_000L
        WriteLPFile = Some "Test.lp"
    }
    
    let result = Solver.solve settings model
    printfn "%A" result

