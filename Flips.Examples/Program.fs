// Learn more about F# at http://fsharp.org

open System
open Flips.Domain
open Flips.Solve
open Flips.SliceMap

let FoodTruckExample () =

    // Declare the parameters for our model
    let hamburgerProfit = 1.50
    let hotdogProfit = 1.20
    let hamburgerBuns = 300.0
    let hotdogBuns = 200.0
    let hamburgerWeight = 0.5
    let hotdogWeight = 0.4
    let maxWeight = 800.0

    let numberOfHamburgers = Decision.createContinuous "NumberOfHamburgers" 0.0 infinity
    let numberOfHotdogs = Decision.createContinuous "NumberOfHotDogs" 0.0 infinity

    let objectiveExpression = hamburgerProfit * numberOfHamburgers + hotdogProfit * numberOfHotdogs
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    let maxHamburger = Constraint.create "MaxHamburger" (numberOfHamburgers <== hamburgerBuns)
    let maxHotDog = Constraint.create "MaxHotDog" (numberOfHotdogs <== hotdogBuns)
    let maxWeight = Constraint.create "MaxWeight" (numberOfHotdogs * hotdogWeight + numberOfHamburgers * hamburgerWeight <== maxWeight)

    let model =
        Model.create objective
        |> Model.addConstraint maxHamburger
        |> Model.addConstraint maxHotDog
        |> Model.addConstraint maxWeight

    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    let result = solve settings model
    printfn "%A" result

let simpleModel () =
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
    
    let result = solve settings model
    printfn "%A" result


let constraintBuilderExample () =
    let sources = [1 .. 3]
    let sourceMax = Map.ofList [for s in sources -> s, 10.0 * float s]

    let destinations = ["a"; "b"; "c"]
    let destinationMax = Map.ofList ["a", 12.0; "b", 14.0; "c", 9.0]

    let arcMax = SMap2.ofList [
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
                (s, d), 1.0 * Decision.createContinuous (sprintf "%i_%s" s d) 0.0 infinity]
        |> SMap2.ofList

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let sourceConstraints = ConstraintBuilder "SourceMax" {
        for source in sources ->
            //let sourceDecs = decisions |> Map.filter (fun (s, d) v -> s = source)
            sum decisions.[source, All] <== sourceMax.[source]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let destinationConstraints = ConstraintBuilder "DestinationMax" {
        for dest in destinations ->
            sum decisions.[All, dest] <== destinationMax.[dest]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let arcConstraints = ConstraintBuilder "ArcMax" {
        for source in sources do
            for dest in destinations ->
                decisions.[source, dest] <== arcMax.[source, dest]
    }

    // Use combination of the `sum` function and the `.*` operator to perform an inner join
    // of two maps and calculate the product of the values for which there are matching keys
    let objExpr = sum (arcValues .* decisions)
    let objective = Objective.create "Max flow" Maximize objExpr

    let model =
        Model.create objective
        |> Model.addConstraints sourceConstraints
        |> Model.addConstraints destinationConstraints
        |> Model.addConstraints arcConstraints

    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 30_000L
        WriteLPFile = Some "ConstraintBuilderExample.lp"
    }

    let result = solve settings model

    printfn "--Result--"
    printfn "%A" result

let mapSlicingExample () =
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
                (s, d), 1.0 * Decision.createContinuous (sprintf "%i_%s" s d) 0.0 infinity]
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

    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 30_000L
        WriteLPFile = Some "ConstraintBuilderExample.lp"
    }

    let result = solve settings model

    printfn "--Result--"
    printfn "%A" result

[<EntryPoint>]
let main argv =
    
    //simpleModel ()
    constraintBuilderExample ()
    mapSlicingExample ()

    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
