module Flips.Examples.MultiObjective

open Flips
open Flips.Legacy
open SliceMap

type Job = Job of int
type Machine = Machine of int

let solve settings =
    let model, revenueObjective, wasteObjective = SampleProblems.MultiObjectiveProblem.model
    
    // Call the `solve` function in the Solve module to evaluate the model
    let result = Solver.solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Optimal solution ->

        printfn "Revenue: %f" (Objective.evaluate solution revenueObjective)
        
        printfn "Waste: %f" (Objective.evaluate solution wasteObjective)

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
