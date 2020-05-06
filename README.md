# Flips
## **F**# **LI**near **P**rogramming **S**ystem

### Introduction

Flips is an F# library for modeling and solving Linear Programming and Mixed-Integer Programming problems. It is inspired by the work of the PuLP library for Python and the excellent Gurobi Python library. It builds on the work of the Google OR-Tools library which is excellent. In the future I hope to support more solver backends.

F# is a great language to work with but many of the existing APIs for modeling Optimization problems are heavily influenced by Object-Oriented concepts. While there is nothing wrong with OO, this is an attempt to take a functional-first approach to the problem.

This library tries to making the modeling of Optimization Models clean and simple. The idea was to make it as straightforward as possible for an Operation Researcher or an Optimization domain expert to express their ideas in F#. These practictioners are used to working with Mathematical constructs like Sets, Sigma-notation, and summations. Reducing the mental distance between the mathematical formulation of problems and the F# representation was a key design goal.

### Simple Example Problem

For anyone not familiar with Mathematical Optimization, the process of creating and solving a model is composed of the following steps:

1. Defining your parameters
2. Creating your Decision Variables
3. Formulating your Objective Function
4. Adding Constraints
5. Solving the Model

Let us work an example problem to see how this works. We are managing a Food Truck and we need to figure out what ingredients we need to pack for the day. In this example we only sell Hamburgers and Hotdogs. Each Hamburger we sell provides us $1.50 in profit. Each Hotdog we sell provides $1.20 in profit. We only have enough Hamburger buns for up to 300 Hamburgers and only 200 buns for Hot Dogs. The ingredients for a single Hamburger weight 0.5 kg and the ingredients for a Hot Dog weigh 0.4 kg. Our Food Truck can only up to 500 kg. The question becomes, how many ingredients do we pack for Hamburgers and how many for Hot Dogs? Let's answer this question by formulating an Optimization model to answer it.

```fsharp
let FoodTruckExample () =

    // Declare the parameters for our model
    let hamburgerProfit = 1.50
    let hotdogProfit = 1.20
    let hamburgerBuns = 300.0
    let hotdogBuns = 200.0
    let hamburgerWeight = 0.5
    let hotdogWeight = 0.4
    let maxTruckWeight = 500.0

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

    // Math the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value: %f" solution.ObjectiveResult

        for (DecisionName name, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %s\tValue: %f" name value
```

If we run this code we will get the following output

```console
Welcome to the CBC MILP Solver
Version: 2.10.3
Build Date: Jan 27 2020

command line - cbc -solve -quit (default strategy 1)
Presolve 0 (-3) rows, 0 (-3) columns and 0 (-4) elements
Empty problem - 0 rows, 0 columns and 0 elements
Optimal - objective value 690
After Postsolve, objective 690, infeasibilities - dual 0 (0), primal 0 (0)
Optimal objective 690 - 0 iterations time 0.002, Presolve 0.00
Total time (CPU seconds):       0.00   (Wallclock seconds):       0.00

-- Result --
Objective Value: 690.000000
Decision: NumberOfHamburgers    Value: 300.000000
Decision: NumberOfHotDogs       Value: 200.000000
Press any key to close...
```

The rows below the `-- Result --` line show the values the solver found. The solver estimates that we can achieve a profit of $690.00 if we pack for 300 Hamburgers and 200 Hot Dogs. You can run this exmaple problem for yourself by running the `FoodTruckExample` problem in the `Flips.Examples` project.