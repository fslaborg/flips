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

### Motivating Problem

Sometimes the best way to see the utility of something is to experience working on a problem without the right tools. Let's take our Food Cart problem and add some complexity. Now we are not managing a single food cart but multiple. We have three different locations we are managing now and we have added Pizza to the menus. Each food cart has a different weight limit and a different profit amount per item.

```fsharp
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
        let locationSum = List.sum [for location in locations -> numberOfItem.[location, item]]
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
```

When we create out `maxItemConstraints` and `maxWeightConstraints` we are having to sum the decisions across a new dimension. For the `maxItemsConstraints` we have to sum the items across the Location dimensions:

```fsharp
let locationSum = List.sum [for location in locations -> numberOfItem.[location, item]]
```

For the `maxWeightConstraints` we have to sum across the Items dimension:

```fsharp
let weightSum = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[location, item]]
```

In this example this is not so bad but imagine problems where the dimensionsality is higher. You end up having nested `List` comprehensions. This summation across dimensions is so common that the `SliceMap` family of types was created. Let's revisit the problem but we will use SliceMaps instead of the built in `Map` type. Instead of storing our data and decisions in the `Map` type, we will be using a corresponding `SliceMap`. If the Key for the data in a single dimension we will use a `SMap`. If the key is two dimensional we will use an `SMap2`.

```fsharp
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

let numberOfItem =
    [for item in items do
        for location in locations do
            let decName = sprintf "NumberOf_%s_At_%s" item location
            let decision = Decision.createContinuous decName 0.0 infinity
            (location, item), decision]
    |> SMap2.ofList

// Use the .* operator to perform an element-wise multiplication of the two SliceMaps
// Sum the result of the element-wise multiplication using the `sum` function.
// The `sum` function calls the `sum` method of the type
let objectiveExpression = sum (profit .* numberOfItem)

let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression

// Create Total Item Maximum constraints for each item
let maxItemConstraints =
    [for item in items do
        let name = sprintf "MaxItemTotal|%s" item
        Constraint.create name (sum numberOfItem.[All, item] <== maxIngredients.[item])
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
```

We can see that things look a little different now. The first change is that we are storing our data in a `SMap` or `SMap2`. All of the SliceMaps support being created from a `List`, `Seq`, or `Array`. This behavior is the same as the equivalent functions for `Map`. You can even create a SliceMap from a `Map` if the dimensionality of the `'Key` matches what the SliceMap is expecting.

The next major change is in the `objectiveExpression` creation:

```fsharp
let objectiveExpression = sum (profit .* numberOfItem)
```

Here we are using two of the features that SliceMaps provide: summation and element-wise multiplication. The `.*` operator is an element-wise multiplication of the values in the SliceMaps. Where the keys match in both SliceMaps, the values are multiplied together. In the cases where the keys do not match, nothing is returned. If you are familiar with SQL, this behavior is the equivalent of an inner-join. The `.*` comes from Matlab and has been implemented in other languages.

The `sum` function is a convenience function to make modeling more streamlined. It can only be used on types which have a `sum` method declared on them. It simply looks at the type can calls its associated `sum` function. All of the SliceMaps have a `sum` method. When sum is called, all of the values in the SliceMap are summed together using the `+` operator. SliceMaps are intended to be used with types which implement `+`, `*`, and `Zero`. The mathematical term is a [Ring](https://en.wikipedia.org/wiki/Ring_(mathematics)).

The next change we see in the model formulation is in the creation of `maxItemConstraints`. Specifically on the line create we create the constraint.

```fsharp
Constraint.create name (sum numberOfItem.[All, item] <== maxIngredients.[item])
```

We are using the slicing capability of SliceMaps. For this constraint we are wanting to sum how much of a given Item we are sending across all the Locations. Before this was done using a List comprehension. Here we are slicing and then summing the resulting SliceMap. Remember that the first dimension to the `numberOfItem` SliceMap is the Location. This expression, `numberOfItem.[All, item]`, is saying to select items in the SliceMap for `All` the locations but only where the `item` key matches. This slicing then returns a new SliceMap. The returned SliceMap is summed to form the left hand side of our Constraint Expression.

SliceMaps have several different types of slices they support:

- All : All Keys will match
- Equal : Key must be qual to this value
- GreaterThan : Key must be greater than this value
- GreaterOrEqual : Key must be greater or equal to this value
- LessThan : Key must be less than this value
- LessOrEqual : Key must be less or equal to this value
- Between : Key must be between these values (inclusive)
- In : Key must be contained in this set
- NotIn : Key must not be in this set
- Where : Key must return true for the predicate

SliceMaps also support scalar multiplication.

```fsharp
let x = SMap.ofList [for i in 1..3 -> i, float i]

> x * 2.0
val it : SMap<int,float> =
  Map1D {Values = map [(1, 2.0); (2, 4.0); (3, 6.0)];}
```

You can also add SliceMaps together. The types of the Keys have to match and the contained values need to support addition with one another. In the cases where there are matching Keys, the values are added together. The values where the Keys do not match are still returned in the new SliceMap.

```fsharp
let x = SMap.ofList [for i in 1..3 -> i, float i]
let y = SMap.ofList [for i in 2..4 -> i, 2.0 * float i]
x + y
> 
val it : SMap<int,float> =
  Map1D {Values = map [(1, 1.0); (2, 6.0); (3, 9.0); (4, 8.0)];}
```

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