# Intro Problem

```fsharp
module Flips.Examples.SimpleExample

open Flips
open Flips.Types

// Create Decisions for your problem
let x1 = Decision.createContinuous "x1" 0.0 infinity
let x2 = Decision.createContinuous "x2" 0.0 infinity

// Create a LinearExpression which quantifies your goal
let objExpr = 2.0 * x1 + 3.0 * x2
// Create an Objective for your model
let objective = Objective.create "Get big" Maximize objExpr

// Create a Model and pipe it through some `addConstraint` functions
// to put bounds on the solution
let model = 
    Model.create objective
    |> Model.addConstraint (Constraint.create "Max x1" (x1 <== 10.0))
    |> Model.addConstraint (Constraint.create "Max x2" (x2 <== 5.0))
    |> Model.addConstraint (Constraint.create "Max x1 and x2" (x1 + x2 <== 12.0))

// Create the settings you would like the solver the use
let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 30_000L
    WriteLPFile = Some "Test.lp"
}

// Solve the Model
let result = Solver.solve settings model
printfn "%A" result
```