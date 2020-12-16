module Flips.Examples.SampleProblems
open Flips
open SliceMap


module MultiObjectiveProblem =
    type Job = Job of int
    type Machine = Machine of int
    let model =
        let rng = System.Random(123)
        // Declare the parameters for our model
        let maxAssignments = 5.0
        let maxMachineAssignments = 2.0
        let jobs = [1..9] |> List.map Job
        let machines = [1..5] |> List.map Machine
        let jobRevenue =
            jobs
            |> List.map (fun j -> j, float (rng.Next(5, 10)))
            |> SMap.ofList

        let jobMachineWaste =
            [for j in jobs do
                for m in machines ->
                    (j, m), float (rng.Next(1, 5))
            ] |> SMap2.ofList

        let assignments =
            DecisionBuilder "Assignment" {
                for j in jobs do
                    for m in machines ->
                        Boolean
            } |> SMap2.ofSeq

        // Sum the total revenue for the jobs selected
        let revenueExpr = sum (jobRevenue .* assignments)

        // Sum the waste for running a job on a particular machine
        let wasteExpr = sum (assignments .* jobMachineWaste)

        // Create an objective to maximize the revenue
        let revenueObjective = Objective.create "MaxRevenue" Maximize revenueExpr

        // Create an objective to minimize the waste
        let wasteObjective = Objective.create "MinWaste" Minimize wasteExpr

        // Create a constraint which limits the number of jobs that can be assigned
        let assignmentConstraint =
            Constraint.create "MaxNumberOfAssignments" (sum assignments <== maxAssignments)

        // Create constraints for a job only being assigned once
        let oneAssignmentConstraints =
            ConstraintBuilder "OneAssignment" {
                for j in jobs ->
                    sum assignments.[j, All] <== 1.0
            }

        // Create constraints so that no machine can have more than max number of jobs
        let machineAssignmentConstraints =
            ConstraintBuilder "MaxMachineAssignement" {
                for m in machines ->
                    sum assignments.[All, m] <== maxMachineAssignments
            }
        
        let model =
            Model.create revenueObjective
            |> Model.addObjective wasteObjective
            |> Model.addConstraint assignmentConstraint
            |> Model.addConstraints machineAssignmentConstraints
            |> Model.addConstraints oneAssignmentConstraints
        model, revenueObjective, wasteObjective
        

module SimpleExample =
    let model =
        let x1 = Decision.createContinuous "x1" 0.0 infinity
        let x2 = Decision.createContinuous "x2" 0.0 infinity
    
        let objExpr = 2.0 * x1 + 3.0 * x2
        let objective = Objective.create "Get big" Maximize objExpr
        let model = 
            Model.create objective
            |> Model.addConstraint (Constraint.create "Max x1" (x1 <== 10.0))
            |> Model.addConstraint (Constraint.create "Max x2" (x2 <== 5.0))
            |> Model.addConstraint (Constraint.create "Max x1 and x2" (x1 + x2 <== 12.0))
    
        model


module FoodTruckExample =
    let model =
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
        model, objective
module FoodTruckMapExample =

    let model =
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

        model, objective


module FoodTruckDecisionBuilder =

    let model =    
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

        model, objective
  
  
module FoodTruckUnitsOfMeasureExample =
    open Flips.UnitsOfMeasure
    type [<Measure>] USD
    type [<Measure>] Item
    type [<Measure>] Lb
    let model =
            
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

        model, objective


module FoodTruckConstraintBuilderExample =
    let model =

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
        model, objective


module MultipleFoodTruckExample =
    let model =

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
                let name = sprintf "MaxItemTotal_%s" item
                Constraint.create name (locationSum <== maxIngredients.[item])
            ]


        // Create a Constraint for the Max combined weight of items for each Location
        let maxWeightConstraints = 
            [for location in locations -> 
                let weightSum = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[location, item]]
                let name = sprintf "MaxTotalWeight_%s" location
                Constraint.create name (weightSum <== maxTruckWeight.[location])
            ]

        // Create a Model type and pipe it through the addition of the constraints
        let model =
            Model.create objective
            |> Model.addConstraints maxItemConstraints
            |> Model.addConstraints maxWeightConstraints
        model, objective


module MultipleFoodTruckWithSliceMapExample =
    let model =
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
        let objectiveExpression = sum (numberOfItem)

        // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
        // the Objective Expression
        let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
        // Create Total Item Maximum constraints for each item
        let maxItemConstraints =
            [for item in items do
                let name = sprintf "MaxItemTotal_%s" item
                //let lhs : LinearExpression  = sum (numberOfItem.[All, item])
                Constraint.create name (sum (numberOfItem.[All, item]) <== maxIngredients.[item])
            ]


        // Create a Constraint for the Max combined weight of items for each Location
        let maxWeightConstraints = 
            [for location in locations -> 
                let name = sprintf "MaxTotalWeight_%s" location
                //let lhs = itemWeight .* numberOfItem.[location, All]
                Constraint.create name (sum (itemWeight .* numberOfItem.[location, All]) <== maxTruckWeight.[location])
            ]

        // Create a Model type and pipe it through the addition of the constraints
        let model =
            Model.create objective
            |> Model.addConstraints maxItemConstraints
            |> Model.addConstraints maxWeightConstraints
        model, objective


module CoffeeRoastingExample =
    open Flips.UnitsOfMeasure
    
    type [<Measure>] USD
    type [<Measure>] ft
    type [<Measure>] Ton
    type [<Measure>] Build

    type Location = Location of string
    let model = 
        let minRoastingCapacity = 30.0<Ton>
        let minWarehouseCapacity = 30_000.0<ft^3>

        let locations = 
            [
                "Sellwood"
                "Hawthorne"
                "The Pearl"
                "Eastmoreland"
                "St Johns"
                "Alberta"
                "Nob Hill"
                "Belmont"
            ] |> List.map Location

        let roasterCost = 
            [
                Location "Sellwood"      , 150_000.0<USD/Build>
                Location "Hawthorne"     , 100_000.0<USD/Build> 
                Location "The Pearl"     , 250_000.0<USD/Build>
                Location "Eastmoreland"  , 120_000.0<USD/Build>
                Location "St Johns"      , 130_000.0<USD/Build>
                Location "Alberta"       , 110_000.0<USD/Build>
                Location "Nob Hill"      , 135_000.0<USD/Build>
                Location "Belmont"       , 180_000.0<USD/Build>
            ] |> SMap.ofList

        let roasterCapacity = 
            [
                Location "Sellwood"      , 12.0<Ton/Build>
                Location "Hawthorne"     , 18.0<Ton/Build> 
                Location "The Pearl"     , 22.0<Ton/Build>
                Location "Eastmoreland"  , 13.0<Ton/Build>
                Location "St Johns"      , 14.0<Ton/Build>
                Location "Alberta"       , 10.0<Ton/Build>
                Location "Nob Hill"      , 17.0<Ton/Build>
                Location "Belmont"       , 12.0<Ton/Build>
            ] |> SMap.ofList

        let warehouseCost = 
            [
                Location "Sellwood"      ,  80_000.0<USD/Build>
                Location "Hawthorne"     ,  90_000.0<USD/Build> 
                Location "The Pearl"     , 120_000.0<USD/Build>
                Location "Eastmoreland"  ,  90_000.0<USD/Build>
                Location "St Johns"      ,  85_000.0<USD/Build>
                Location "Alberta"       ,  70_000.0<USD/Build>
                Location "Nob Hill"      ,  85_000.0<USD/Build>
                Location "Belmont"       ,  90_000.0<USD/Build>
            ] |> SMap.ofList

        let warehouseCapacity = 
            [
                Location "Sellwood"      ,  8_000.0<ft^3/Build>
                Location "Hawthorne"     ,  6_000.0<ft^3/Build> 
                Location "The Pearl"     , 12_000.0<ft^3/Build>
                Location "Eastmoreland"  ,  6_000.0<ft^3/Build>
                Location "St Johns"      ,  7_000.0<ft^3/Build>
                Location "Alberta"       ,  9_000.0<ft^3/Build>
                Location "Nob Hill"      ,  6_000.0<ft^3/Build>
                Location "Belmont"       ,  9_200.0<ft^3/Build>
            ] |> SMap.ofList

        let buildRoaster =
            DecisionBuilder<Build> "BuildRoaster" {
                for l in locations ->
                    Boolean
            } |> SMap.ofSeq

        let buildWarehouse =
            DecisionBuilder<Build> "BuildWarehouse" {
                for l in locations ->
                    Boolean
            } |> SMap.ofSeq

        let minRoastingCapacityConstraint =
            let lhs = buildRoaster .* roasterCapacity
            let s = sum lhs
            Constraint.create "MinRoastingCapacity" (sum (buildRoaster .* roasterCapacity) >== minRoastingCapacity)

        let minWarehouseCapacityConstraint =
            Constraint.create "MinWarehouseCapacity" (sum (buildWarehouse .* warehouseCapacity) >== minWarehouseCapacity)

        let warehouseWithRoasterConstraints =
            ConstraintBuilder "WarehouseWithRoaster" {
                for l in locations ->
                    buildRoaster.[l] <== buildWarehouse.[l]
            }

        let t = buildRoaster .* roasterCost
        let objExpr = sum (t) + sum (buildWarehouse .* warehouseCost)
        let objective = Objective.create "MinimizeCost" Minimize objExpr

        let model = 
            Model.create objective
            |> Model.addConstraint minRoastingCapacityConstraint
            |> Model.addConstraint minWarehouseCapacityConstraint
            |> Model.addConstraints warehouseWithRoasterConstraints

        model, objective, buildRoaster, buildWarehouse, locations
        

module MapSlicingExample =

    let model =
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

        model, objective


module BinaryProgrammingExample =
    let model =
        let indexes = [1..9]
        let value = [for i in indexes -> i, 10.0 - (float i)] |> SMap.ofList
        let decisions = 
            DecisionBuilder "Chose" {
                for i in indexes ->
                    Boolean
            } |> SMap.ofSeq

        let objExpr = sum (value .* decisions)
        let objective = Objective.create "Max" Maximize objExpr

        let uniqueConstraint = Constraint.create "Unique" (sum (1.0 * decisions) <== 1.0)

        let model =
            Model.create objective
            |> Model.addConstraint uniqueConstraint
        model, objective