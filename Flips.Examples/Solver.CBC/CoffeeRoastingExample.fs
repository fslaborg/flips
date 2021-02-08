module Flips.Examples.CBC.CoffeeRoastingExample
open Flips
open Flips.UnitsOfMeasure
open Flips.Solver.UnitsOfMeasure
open Flips.Solver.CBC

let solve () =
    let model, objective, buildRoaster, buildWarehouse, locations = Flips.Examples.SampleProblems.CoffeeRoastingExample.model
    let settings = Settings [set.WriteLPFile "Chicken.lp"]
    let solver = Solver settings
    // Call the `solve` function in the Solve module to evaluate the model
    let result = solver.Solve model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Ok solution ->
        printfn "Objective Value: %f" (Objective.evaluate solution objective)

        let roasterValues = Solution.getValues solution buildRoaster
        let warehouseValues = Solution.getValues solution buildWarehouse

        printfn "Plan Cost: $%.2f" (Objective.evaluate solution objective)

        printfn "Location\tRoaster\tWarehouse"

        for location in locations do
            let (Flips.Examples.SampleProblems.CoffeeRoastingExample.Location l) = location
            printfn "%-12s\t%12A\t%12A" l roasterValues.[location] warehouseValues.[location]
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
