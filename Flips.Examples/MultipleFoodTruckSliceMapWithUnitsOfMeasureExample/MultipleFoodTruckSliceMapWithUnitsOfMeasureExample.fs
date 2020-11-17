module Flips.Examples.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

type [<Measure>] USD
type [<Measure>] Item
type [<Measure>] Lb

let solve settings =
    
    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"; "Pizza"]
    let locations = ["Woodstock"; "Sellwood"; "Portland"]
    let profit = 
        [
            (("Woodstock", "Hamburger"), 1.50<USD/Item>); (("Sellwood", "Hamburger"), 1.40<USD/Item>); (("Portland", "Hamburger"), 1.90<USD/Item>)
            (("Woodstock", "HotDog"   ), 1.20<USD/Item>); (("Sellwood", "HotDog"   ), 1.50<USD/Item>); (("Portland", "HotDog"   ), 1.80<USD/Item>)
            (("Woodstock", "Pizza"    ), 2.20<USD/Item>); (("Sellwood", "Pizza"    ), 1.70<USD/Item>); (("Portland", "Pizza"    ), 2.00<USD/Item>)
        ] |> SMap2.ofList

    let maxIngredients = SMap.ofList [("Hamburger", 900.0<Item>); ("HotDog", 600.0<Item>); ("Pizza", 400.0<Item>)]
    let itemWeight = SMap.ofList [("Hamburger", 0.5<Lb/Item>); ("HotDog", 0.4<Lb/Item>); ("Pizza", 0.6<Lb/Item>)]
    let maxTruckWeight = SMap.ofList [("Woodstock", 200.0<Lb>); ("Sellwood", 300.0<Lb>); ("Portland", 280.0<Lb>) ]

    // Create Decisions for each loation and item using a DecisionBuilder
    // Turn the result into a `SMap2`
    let numberOfItem =
        DecisionBuilder<Item> "NumberOf" {
            for location in locations do
            for item in items ->
                Continuous (0.0<Item>, 1_000_000.0<Item>)
        } |> SMap2.ofSeq

    // Create the Linear Expression for the objective
    let objectiveExpression = sum (profit .* numberOfItem)

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create Total Item Maximum constraints for each item
    let maxItemConstraints =
        ConstraintBuilder "MaxItemTotal" {
            for item in items ->
                sum (1.0 * numberOfItem.[All, item]) <== maxIngredients.[item]
        }

    // Create a Constraints for the Max combined weight of items for each Location
    let maxWeightConstraints =
        ConstraintBuilder "MaxTotalWeight" {
            for location in locations ->
                sum (itemWeight .* numberOfItem.[location, All]) <== maxTruckWeight.[location]
        }

    // Create a Model type and pipe it through the addition of the constraints
    let model =
        Model.create objective
        |> Model.addConstraints maxItemConstraints
        |> Model.addConstraints maxWeightConstraints

    // Call the `solve` function in the Solve module to evaluate the model
    let result = Solver.solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        let values = Solution.getValues solution (numberOfItem.AsMap())

        for ((location, item), value) in values |> Map.toSeq do
            printfn "Item: %s\tLocation: %s\tValue: %f" item location value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase