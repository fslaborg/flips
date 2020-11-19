# Food Truck Intro

```fsharp
module Flips.Examples.FoodTruckExample

open Flips
open Flips.Types

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

// Call the `solve` function in the Solve module to evaluate the model
// using the basic settings
let result = Solver.solve Settings.basic model

printfn "-- Result --"

// Match the result of the call to solve
// If the model could not be solved it will return a `Suboptimal` case with a message as to why
// If the model could be solved, it will print the value of the Objective Function and the
// values for the Decision Variables
match result with
| Optimal solution ->
    printfn "Objective Value: %f" (Solution.evaluateObjective solution objective)

    for (decision, value) in solution.DecisionResults |> Map.toSeq do
        let (DecisionName name) = decision.Name
        printfn "Decision: %s\tValue: %f" name value
| _ -> printfn $"Unable to solve. Error: %A{result}"
```