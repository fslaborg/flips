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
-- Result --
Objective Value: 690.000000
Decision: NumberOfHamburgers    Value: 300.000000
Decision: NumberOfHotDogs       Value: 200.000000
Press any key to close...
```

The rows below the `-- Result --` line show the values the solver found. The solver estimates that we can achieve a profit of $690.00 if we pack for 300 Hamburgers and 200 Hot Dogs. You can run this exmaple problem for yourself by running the `FoodTruckExample` problem in the `Flips.Examples` project.

### Using Indices

While the above formulation did work, it does not scale automatically with the number of food items. A better way to formulate this problem would be to store parameter data and decision variables in `Map` instances and use product names as the keys. This means we add a new first step: define the indices. This means the steps to modeling are now:

1. Define the Indices/Keys for your data
2. Define your data as Maps using your defined Keys
3. Create Decision Variables Maps using your defined Keys
4. Formulating your Objective Function
5. Adding Constraints
6. Solving the Model

We now show how to formulate the problems using the F# `Map` type so that it can scale to an arbitrary number of different items.

```fsharp
let FoodTruckMapExample () =

    // Declare the parameters for our model
    let items = ["Hamburger"; "HotDog"]
    let profit = Map.ofList [("Hamburger", 1.50); ("HotDog", 1.20)]
    let maxIngredients = Map.ofList [("Hamburger", 300.0); ("HotDog", 200.0)]
    let itemWeight = Map.ofList [("Hamburger", 0.5); ("HotDog", 0.4)]
    let maxTruckWeight = 500.0

    // Create Decision Variable Map<string,Decision> to represent how much of each item we should pack
    // with a Lower Bound of 0.0 and an Upper Bound of Infinity
    let numberOfItem =
        items
        |> List.map (fun x -> x, Decision.createContinuous (sprintf "NumberOf%s" x) 0.0 infinity)
        |> Map.ofList

    // Create the Linear Expression for the objective
    let objectiveExpression = items |> List.sumBy (fun item -> profit.[item] * numberOfItem.[item])

    // Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
    // the Objective Expression
    let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
    
    // Create a Max Item Constraints
    let maxItemConstraints =
        items
        |> List.map (fun item -> Constraint.create (sprintf "MaxItem|%s" item) (numberOfItem.[item] <== maxIngredients.[item]) )

    // Create a Constraint for the Max combined weight of Hamburgers and Hotdogs
    let weightExpression = items |> List.sumBy (fun item -> itemWeight.[item] * numberOfItem.[item])
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

We now have a formulation of the problem that will scale to an arbitrary number of items. This is a more maintainable formulation and will not require updating as what is and is not avilable to sell changes over time.

## SliceMaps

### Why do I care about SliceMaps?

Up to this point we have been using the built in `Map` type for holding our data and Decision Variables. One of the challenges we will quickly run into when modeling optimization problems is that it is common to take ranges of values. For these reasons it is common to use N-dimensional Arrays so that you can take slices across different dimensions. Just using the built-in F# `Array` type has limitations though since you can only index the values with `int`. What we really want is something that allows us to look up a particular value using an arbitrary index type but also allows us to select ranges of values.

So, we need something that has `Map` like lookup but also allows us to slice across different dimensions...? I know, let's create a new type, a `SliceMap`!

> **Aside**: There was an attempt to simply extend the existing F# `Map` type. Ultimately the combination of features that was required in `SliceMap` made that not possible. Specifically, `SliceMap` is not a single type but a family of types: `SMap`, `SMap2`, `SMap3`, `SMap4`. The numbers correspond to the dimensionality of the `Key` used in the map. `SMap` is keyed by a single value. `SMap2` is keyed by a tuple of two values. `SMap3` is keyed by a tuple of three values and so forth. These types also have some unique interactions that could not be implemented with just extending the built in `Map` type.

### Constraint Builder

Since the creation of constraints is such a common occurence in modeling, a `ConstraintBuilder` Computation Expression was made to streamline the naming of constraints. The idea is that you give a prefix for the set of constraints you are going to create and the Computation Expression takes care of naming the constraint you are creating. Here a side by side example is given of the Food Truck problem. This is showing how to create constraints across two dimensions: Items and Locations.

```fsharp
let items = ["Hamburger"; "HotDog"]
let locations = ["Woodstock"; "Sellwood"; "Portland"]

// Create Decision Variable which is keyed by the tuple of Item and Location.
let numberOfItem =
    [for item in items do
        for location in locations do
            let decName = sprintf "NumberOf_%s_At_%s" item location
            let decision = Decision.createContinuous decName 0.0 infinity
            (item, location), decision]
    |> Map.ofList

// How you would write the MaxItem constraints without `ConstraintBuilder`
let maxItemConstraints =
    [for item in items do
        for location in locations do
            let name = sprintf "MaxItem|%s_%s" item location
            Constraint.create name (numberOfItem.[item,location] <== maxIngredients.[item])]

// The equivalent statement using a `ConstraintBuilder`
let maxItemConstraints = ConstraintBuilder "MaxItem" {
    for item in items do
        for location in locations -> 
            numberOfItem.[item,location] <== maxIngredients.[item]
}
```

`ConstraintBuilder` simply removes the need to define how to name the constraints. It is not necessary to use the `ConstrantBuilder` but it is there to streamline your modeling.