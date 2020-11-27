module Flips.Examples.FoodTruckUnitsOfMeasureExample

open Flips
open Flips.Legacy
open Flips.UnitsOfMeasure

type [<Measure>] USD
type [<Measure>] Item
type [<Measure>] Lb

let solve settings =

    // Declare the parameters for our model
    // This time include Units of Measure on the floats
    let items = ["Hamburger"; "HotDog"]
    let profit = Map.ofList [("Hamburger", 1.50<USD/Item>); ("HotDog", 1.20<USD/Item>)]
    let maxIngredients = Map.ofList [("Hamburger", 300.0<Item>); ("HotDog", 200.0<Item>)]
    let itemWeight = Map.ofList [("Hamburger", 0.5<Lb/Item>); ("HotDog", 0.4<Lb/Item>)]
    let maxTruckWeight = 200.0<Lb>

    // Create Decision Variable Map<string,Decision> to represent how much of each item we should pack
    // with a Lower Bound of 0.0<Item> and an upper bound of 1_000_000.0<Item>
    // The infinity value does not work with Units of Measure
    let numberOfItem =
        [for item in items do
            item, Decision.createContinuous (sprintf "NumberOf%s" item) 0.0<Item> 1_000_000.0<Item>]
        |> Map.ofList

    // Create the Linear Expression for the objective
    let x = [for item in items -> profit.[item] * numberOfItem.[item]]
    let objectiveExpression = List.sum [for item in items -> profit.[item] * numberOfItem.[item]]

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create a Max Item Constraints using the `ConstraintBuilder` the first argument for the builder
    // is the prefix used for naming the constraint. The second argument is the F# expression which
    // it will use for generating the `ConstraintExpressions`
    let maxItemConstraints = ConstraintBuilder "MaxItem" {
        for item in items ->
            numberOfItem.[item] <== maxIngredients.[item]
    }

    // Create a Constraint for the Max combined weight of Hamburgers and Hotdogs
    let weightExpression = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[item]]
    let maxWeight = Constraint.create "MaxWeight" (weightExpression <== maxTruckWeight)

    // Create a Model type and pipe it through the addition of the constraints
    let model =
        Model.create objective
        |> Model.addConstraints maxItemConstraints
        |> Model.addConstraint maxWeight

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
        
        CsvExport.exportVariablesToFile "foodtruck.uom" solution.DecisionResults CsvExport.csvConfig
        
        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase