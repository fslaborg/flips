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
    let maxTruckWeight = 200.0

    // Create Decision Variable with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfHamburgers = Decision.createContinuous "NumberOfHamburgers" 0.0 infinity
    let numberOfHotdogs = Decision.createContinuous "NumberOfHotDogs" 0.0 infinity

    // Create the Linear Expression for the objective
    let objectiveExpression = hamburgerProfit * numberOfHamburgers + hotdogProfit * numberOfHotdogs

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create a Constraint for the max number of Hamburger considering the number of buns
    let maxHamburger = Constraint.create "MaxHamburger" (numberOfHamburgers <== hamburgerBuns)
    // Create a Constraint for the max number of Hot Dogs considering the number of buns
    let maxHotDog = Constraint.create "MaxHotDog" (numberOfHotdogs <== hotdogBuns)
    // Create a Constraint for the Max combined weight of Hamburgers and Hotdogs
    let maxWeight = Constraint.create "MaxWeight" (numberOfHotdogs * hotdogWeight + numberOfHamburgers * hamburgerWeight <== maxTruckWeight)

    // Create a Model type and pipe it through the addition of the constraitns
    let model =
        Model.create objective
        |> Model.addConstraint maxHamburger
        |> Model.addConstraint maxHotDog
        |> Model.addConstraint maxWeight

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

let FoodTruckMapExample () =

    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"]
    let profit = Map.ofList [("Hamburger", 1.50); ("HotDog", 1.20)]
    let maxIngredients = Map.ofList [("Hamburger", 300.0); ("HotDog", 200.0)]
    let itemWeight = Map.ofList [("Hamburger", 0.5); ("HotDog", 0.4)]
    let maxTruckWeight = 200.0

    // Create Decision Variable Map<string,Decision> to represent how much of each item we should pack
    // with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfItem =
        [for item in items do
            item, Decision.createContinuous (sprintf "NumberOf%s" item) 0.0 infinity]
        |> Map.ofList

    // Create the Linear Expression for the objective
    let objectiveExpression = List.sum [for item in items -> profit.[item] * numberOfItem.[item]]

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create a Max Item Constraints
    let maxItemConstraints =
        [for item in items ->
            Constraint.create (sprintf "MaxOf%s" item) (numberOfItem.[item] <== maxIngredients.[item])]

    // Create a Constraint for the Max combined weight of Hamburgers and Hotdogs
    let weightExpression = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[item]]
    let maxWeight = Constraint.create "MaxWeight" (weightExpression<== maxTruckWeight)

    // Create a Model type and pipe it through the addition of the constraints
    let model =
        Model.create objective
        |> Model.addConstraints maxItemConstraints
        |> Model.addConstraint maxWeight

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

let FoodTruckConstraintBuilderExample () =

    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"]
    let profit = Map.ofList [("Hamburger", 1.50); ("HotDog", 1.20)]
    let maxIngredients = Map.ofList [("Hamburger", 300.0); ("HotDog", 200.0)]
    let itemWeight = Map.ofList [("Hamburger", 0.5); ("HotDog", 0.4)]
    let maxTruckWeight = 200.0

    // Create Decision Variable Map<string,Decision> to represent how much of each item we should pack
    // with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfItem =
        [for item in items do
            item, Decision.createContinuous (sprintf "NumberOf%s" item) 0.0 infinity]
        |> Map.ofList

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

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

let MultipleFoodTruckExample () =
    
    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"; "Pizza"]
    let locations = ["Woodstock"; "Sellwood"; "Portland"]
    let profit = 
        [
            (("Woodstock", "Hamburger"), 1.50); (("Sellwood", "Hamburger"), 1.40); (("Portland", "Hamburger"), 1.90)
            (("Woodstock", "HotDog"   ), 1.20); (("Sellwood", "HotDog"   ), 1.50); (("Portland", "HotDog"   ), 1.80)
            (("Woodstock", "Pizza"    ), 2.20); (("Sellwood", "Pizza"    ), 1.70); (("Portland", "Pizza"    ), 2.00)
        ] |> Map.ofList

    let maxIngredients = Map.ofList [("Hamburger", 900.0); ("HotDog", 600.0); ("Pizza", 400.0)]
    let itemWeight = Map.ofList [("Hamburger", 0.5); ("HotDog", 0.4); ("Pizza", 0.6)]
    let maxTruckWeight = Map.ofList [("Woodstock", 200.0); ("Sellwood", 300.0); ("Portland", 280.0) ]

    // Create Decision Variable which is keyed by the tuple of Item and Location.
    // The resulting type is a Map<(string*string),Decision> 
    // to represent how much of each item we should pack for each location
    // with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfItem =
        [for item in items do
            for location in locations do
                let decName = sprintf "NumberOf_%s_At_%s" item location
                let decision = Decision.createContinuous decName 0.0 infinity
                (location, item), decision]
        |> Map.ofList

    // Create the Linear Expression for the objective
    let objectiveExpression = 
        [for item in items do
            for location in locations ->
                profit.[location, item] * numberOfItem.[location, item]]
        |> List.sum            

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create Total Item Maximum constraints for each item
    let maxItemConstraints =
        [for item in items do
            // The total of the Item is the sum across the Locations
            let locationSum = List.sum [for location in locations -> 1.0 * numberOfItem.[location, item]]
            let name = sprintf "MaxItemTotal|%s" item
            Constraint.create name (locationSum <== maxIngredients.[item])
        ]


    // Create a Constraint for the Max combined weight of items for each Location
    let maxWeightConstraints = 
        [for location in locations -> 
            let weightSum = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[location, item]]
            let name = sprintf "MaxTotalWeight|%s" location
            Constraint.create name (weightSum <== maxTruckWeight.[location])
        ]

    // Create a Model type and pipe it through the addition of the constraints
    let model =
        Model.create objective
        |> Model.addConstraints maxItemConstraints
        |> Model.addConstraints maxWeightConstraints

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

let MultipleFoodTruckWithSliceMapExample () =
    
    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"; "Pizza"]
    let locations = ["Woodstock"; "Sellwood"; "Portland"]
    let profit = 
        [
            (("Woodstock", "Hamburger"), 1.50); (("Sellwood", "Hamburger"), 1.40); (("Portland", "Hamburger"), 1.90)
            (("Woodstock", "HotDog"   ), 1.20); (("Sellwood", "HotDog"   ), 1.50); (("Portland", "HotDog"   ), 1.80)
            (("Woodstock", "Pizza"    ), 2.20); (("Sellwood", "Pizza"    ), 1.70); (("Portland", "Pizza"    ), 2.00)
        ] |> SMap2.ofList

    let maxIngredients = SMap.ofList [("Hamburger", 900.0); ("HotDog", 600.0); ("Pizza", 400.0)]
    let itemWeight = SMap.ofList [("Hamburger", 0.5); ("HotDog", 0.4); ("Pizza", 0.6)]
    let maxTruckWeight = SMap.ofList [("Woodstock", 200.0); ("Sellwood", 300.0); ("Portland", 280.0) ]

    // Create Decision Variable which is keyed by the tuple of Item and Location.
    // The resulting type is a Map<(string*string),Decision> 
    // to represent how much of each item we should pack for each location
    // with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfItem =
        [for item in items do
            for location in locations do
                let decName = sprintf "NumberOf_%s_At_%s" item location
                let decision = Decision.createContinuous decName 0.0 infinity
                (location, item), decision]
        |> SMap2.ofList

    // Create the Linear Expression for the objective
    let objectiveExpression = sum (profit .* numberOfItem)

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create Total Item Maximum constraints for each item
    let maxItemConstraints =
        [for item in items do
            let name = sprintf "MaxItemTotal|%s" item
            Constraint.create name (sum (1.0 * numberOfItem.[All, item]) <== maxIngredients.[item])
        ]


    // Create a Constraint for the Max combined weight of items for each Location
    let maxWeightConstraints = 
        [for location in locations -> 
            let name = sprintf "MaxTotalWeight|%s" location
            Constraint.create name (sum (itemWeight .* numberOfItem.[location, All]) <== maxTruckWeight.[location])
        ]

    // Create a Model type and pipe it through the addition of the constraints
    let model =
        Model.create objective
        |> Model.addConstraints maxItemConstraints
        |> Model.addConstraints maxWeightConstraints

    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

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

let buildersExample () =
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
        DecisionBuilder "FlowAmount" {
            for s in sources do
                for d in destinations ->
                    Continuous (0.0, infinity)
        } |> SMap2.ofSeq

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let sourceConstraints = ConstraintBuilder "SourceMax" {
        for source in sources ->
            //let sourceDecs = decisions |> Map.filter (fun (s, d) v -> s = source)
            sum (1.0 * decisions.[source, All]) <== sourceMax.[source]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let destinationConstraints = ConstraintBuilder "DestinationMax" {
        for dest in destinations ->
            sum (1.0 * decisions.[All, dest]) <== destinationMax.[dest]
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
    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

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
    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value

[<EntryPoint>]
let main argv =
    
    FoodTruckExample ()
    FoodTruckMapExample ()
    FoodTruckConstraintBuilderExample ()
    MultipleFoodTruckExample ()
    MultipleFoodTruckWithSliceMapExample ()
    simpleModel ()
    buildersExample ()
    mapSlicingExample ()

    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
