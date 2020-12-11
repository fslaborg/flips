module Flips.Examples.CoffeeRoastingExample

open SliceMap
open Flips
open Flips.Legacy
open Flips.UnitsOfMeasure

let solve settings =

    let model, objective, buildRoaster, buildWarehouse, locations = SampleProblems.CoffeeRoastingExample.model
    // Call the `solve` function in the Solve module to evaluate the model
    let result = Solver.solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Optimal solution ->
        printfn "Objective Value: %f" (Objective.evaluate solution objective)

        let roasterValues = Solution.getValues solution buildRoaster
        let warehouseValues = Solution.getValues solution buildWarehouse

        printfn "Plan Cost: $%.2f" (Objective.evaluate solution objective)

        printfn "Location\tRoaster\tWarehouse"

        for location in locations do
            let (SampleProblems.CoffeeRoastingExample.Location l) = location
            printfn "%-12s\t%12A\t%12A" l roasterValues.[location] warehouseValues.[location]
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
