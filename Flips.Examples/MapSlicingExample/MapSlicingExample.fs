module Flips.Examples.MapSlicingExample

open Flips
open Flips.Types
open Flips.SliceMap


let solve settings =
    let sources = [1 .. 3]
    let sourceMax = Map.ofList [for s in sources -> s, 10.0 * float s]

    let destinations = ["a"; "b"; "c"]
    let destinationMax = Map.ofList ["a", 12.0; "b", 14.0; "c", 9.0]

    let arcMax = Map.ofList [
        (1, "a"), 12.0; (1, "b"), 12.0; (1, "c"), 12.1; 
        (2, "a"), 13.0; (2, "b"), 11.0; (2, "c"), 12.3; 
        (3, "a"), 14.0; (3, "b"), 11.5; (3, "c"), 12.4; 
    ]

    let arcValues = SMap2.ofList [
        (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
        (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
        (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
    ]

    let decisions = 
        [for s in sources do
            for d in destinations ->
                (s, d), Decision.createContinuous (sprintf "%i_%s" s d) 0.0 infinity]
        |> SMap2.ofList

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let sourceConstraints = ConstraintBuilder "SourceMax" {
        for source in sources ->
            // Here we are using the ability to slice the Map across the first
            // dimentions of the 2D Tuple index
            // Slicing is coming from the Extensions module
            sum decisions.[source, All] <== sourceMax.[source]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let destinationConstraints = ConstraintBuilder "DestinationMax" {
        for dest in destinations ->
            // Here we are using the ability to slice the Map across the second
            // dimentions of the 2D Tuple index
            // Slicing is coming from the Extensions module
            sum decisions.[All, dest] <== destinationMax.[dest]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let arcConstraints = ConstraintBuilder "ArcMax" {
        for source in sources do
            for dest in destinations ->
                decisions.[source, dest] <== arcMax.[(source, dest)]
    }

    // Use combination of the `sum` function and the `.*` operator to combine two Maps
    let x = arcValues .* decisions
    let objExpr = sum (arcValues .* decisions)
    let objective = Objective.create "Max flow" Maximize objExpr

    let model =
        Model.create objective
        |> Model.addConstraints sourceConstraints
        |> Model.addConstraints destinationConstraints
        |> Model.addConstraints arcConstraints

    let result = Solver.solve settings model

    printfn "--Result--"
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