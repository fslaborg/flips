module Flips.Examples.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample

open SliceMap
open Flips
open Flips.Legacy
open Flips.UnitsOfMeasure

let solve settings =
    let model, objective, numberOfItem = SampleProblems.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample.model
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
        
        CsvExport.exportVariablesToFile "foodtruck.party.slicemap.uom" solution.DecisionResults CsvExport.csvConfig
        
        let values = Solution.getValues solution numberOfItem

        for ((location, item), value) in values |> Map.toSeq do
            printfn "Item: %s\tLocation: %s\tValue: %f" item location value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase