module Flips.Examples.FoodTruckDecisionBuilder


open Flips
open Flips.Types

let solve settings =

    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"]
    let profit = Map.ofList [("Hamburger", 1.50); ("HotDog", 1.20)]
    let maxIngredients = Map.ofList [("Hamburger", 300.0); ("HotDog", 200.0)]
    let itemWeight = Map.ofList [("Hamburger", 0.5); ("HotDog", 0.4)]
    let maxTruckWeight = 200.0

    // Use a DecisionBuilder to create the Decisions and have them be automatically name
    // The result is turned into a Map<string,Decision>
    let numberOfItem =
        DecisionBuilder "NumberOf" {
            for item in items ->
                Continuous (0.0, infinity)
        } |> Map.ofSeq

    // Create the Linear Expression for the objective
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
        printfn "Objective Value: %f" (Solution.evaluateObjective solution objective)

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase