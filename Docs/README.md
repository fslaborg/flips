# Flips : **F**# **LI**near **P**rogramming **S**ystem

Flips is an F# library for modeling and solving Linear Programming (LP) and Mixed-Integer Programming (MIP) problems. It is inspired by the work of the PuLP library for Python and the excellent Gurobi Python library. It builds on the work of the outstanding [Google OR-Tools library](https://github.com/google/or-tools) and the [OPTANO library](https://optano.com/en/modeling/).

F# is a great language to work with but many of the existing APIs for modeling Optimization problems are heavily influenced by Object-Oriented concepts. While there is nothing wrong with OO, this is an attempt to take a functional-first approach to the problem.

This library tries to make the modeling of Optimization Models (LP/MIP) clean and simple. The idea was to make it straightforward for an Operation Researcher or Optimization domain expert to express their ideas in F#. These practitioners are used to working with Mathematical constructs like Sets, Sigma-notation, and summations. Reducing the mental distance between the mathematical formulation of problems and the F# representation was a key design goal.

F# developers should also find it comfortable to use this library. Over time I will be adding tutorials and training material on how to model Optimization Problems using this library. With a little training any F# developer will be able to add the powerful tool of Optimization to their repertoire.

## Installation

To use Flips, simply add the [nuget package](https://www.nuget.org/packages/Flips/) to whatever project you are working on. The library comes with the [CBC solver](https://github.com/coin-or/Cbc) for Mixed-Integer Programming and the [Google GLOPS solver](https://github.com/google/or-tools) for Linear Programming which are both free and open source.

Flips also supports the [Gurobi](https://www.gurobi.com/) and [IBM CPLEX](https://www.ibm.com/products/ilog-cplex-optimization-studio/details) commercial solvers through the use of the excellent [OPTANO library](https://optano.com/en/modeling/). You will need to get a separate license to use these libraries. The installation of these commercial libraries is not covered in this documentation since installation can depend on deployment and use case. Please refer to these vendors for commercial support in using their product. Flips currently only supports the latest version of each of these libraries.

## Intro Problem

For anyone not familiar with LP/MIP, the process of creating and solving a model is composed of the following steps:

1. Define Parameters: This is the input data for your model
2. Create Decisions: These are the choices that we want the Solver to make.
3. Define the Objective: This is how success is measured
4. Add Constraints: These are the rules that the Solver must objey for the solution to be valid
5. Solve the Model: We give the optimization model to the Solver for it to find the best solution.
6. Inspect the Result: Not all models are solvable. Depending on the formulation, there may be no solutions. We always check whether the Solver was able to find a solution to the model.

Let us go through an example problem to see how this works. We are managing a Food Truck and we need to figure out what ingredients we need to pack for the day. In this example we only sell Hamburgers and Hotdogs. Each Hamburger we sell provides us $1.50 in profit. Each Hotdog we sell provides $1.20 in profit. We only have enough Hamburger buns for up to 300 Hamburgers and only 200 buns for Hotdogs. The ingredients for a single Hamburger weight 0.5 kg and the ingredients for a Hotdog weigh 0.4 kg. Our Food Truck can only hold up to 500 kg. The question becomes, how many ingredients do we pack for Hamburgers and how many for Hotdogs? Let us answer this question by formulating an Optimization model.

```fsharp
open System
open Flips.Domain
open Flips.Solve

// Declare the parameters for our model
let hamburgerProfit = 1.50
let hotdogProfit = 1.20
let hamburgerBuns = 300.0
let hotdogBuns = 200.0
let hamburgerWeight = 0.5
let hotdogWeight = 0.4
let maxTruckWeight = 200.0
```

Next we need to create the Decisions for the Solver to make. Decisions come in three varities: Boolean, Integer, and Continuous. Every Decision must be given a name and a Lower and Upper bound. In the case of a Boolean Decision, the Lower and Upper bound is implictly 0 and 1. For the case of Integer or Continuous, the bounds must be provided. This tells the Solver what is the legal range of values the Decision can take on. Most real-world Decisions have some bound to them. It is rare that something can be positive or negative infinity. There are four functions for creating Decisions in the `Decision` module. 

```fsharp
create: (name:string) -> (decisionType:DecisionType) -> Decision
createBoolean: (name:string) -> Decision
createInteger: (name:string) -> (lowerBound:float) -> (upperBound:float) -> Decision
createContinuous: (name:string) -> (lowerBound:float) -> (upperBound:float) -> Decision
```

For simplicity we will use the `createContinuous` in our example to create the Decisions to represent the number of Hamburgers and Hotdogs we are going to pack.

```fsharp
// Create Decision Variable with a Lower Bound of 0.0 and an Upper Bound of Infinity
let numberOfHamburgers = Decision.createContinuous "NumberOfHamburgers" 0.0 infinity
let numberOfHotdogs = Decision.createContinuous "NumberOfHotDogs" 0.0 infinity
```

We then create the expression for measuring success. This is called an Objective Expression. It is a mathematical formula for calculating whatever it is that you want to maximize or minimize. In this case, it is the calculation for revenue. Once we have the Objective Expression, we create an `Objective` which contains the name of the objective, what our goal is (Maximize or Minimize), and the Objective Expression.

```fsharp
// Create the Linear Expression for the objective
let objectiveExpression = hamburgerProfit * numberOfHamburgers + hotdogProfit * numberOfHotdogs

// Create an Objective with the name "MaximizeRevenue" the goal of Maximizing
// the Objective Expression
let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression
```

We then must create the constraints which describe what is allowed. Whenever we create a `Constraint` we must provide a name and a `ConstraintExpression`. A `ConstraintExpression` is two `LinearExpression` with a comparison operator in between: `==`, `>==`, or `<==`.

> **Note:** Due to .NET reserving the normal comparison operators of `=`, `>=`, and `<=` for use with `IComparable` it was necessary to use a different set of operators. To keep it simple, it was decided to just add an additional equals sign so that there would not be conflict. This means when you want to create a `ConstraintExpression`, you use the following operators: `==`, `>==`, and `<==`.

```fsharp  
// Create a Constraint for the max number of Hamburger considering the number of buns
let maxHamburger = Constraint.create "MaxHamburger" (numberOfHamburgers <== hamburgerBuns)
// Create a Constraint for the max number of Hot Dogs considering the number of buns
let maxHotDog = Constraint.create "MaxHotDog" (numberOfHotdogs <== hotdogBuns)
// Create a Constraint for the Max combined weight of Hamburgers and Hotdogs
let maxWeight = Constraint.create "MaxWeight" (numberOfHotdogs * hotdogWeight + numberOfHamburgers * hamburgerWeight <== maxTruckWeight)
```

Now that we have an `Objective` and some `Constraint`s, we can create a `Model` and add the `Constraint`s to it.

```fsharp
// Create a Model type and pipe it through the addition of the constraitns
let model =
    Model.create objective
    |> Model.addConstraint maxHamburger
    |> Model.addConstraint maxHotDog
    |> Model.addConstraint maxWeight
```

To submit the `Model` to a solver we need to provide `SolverSettings`. Here we choose the use the CBC solver with a Max Duration of 10,000 ms. If you want to see the model written out as an LP file, you can provide a path to where you would like to see it written. Now that we have our `Model` and `SolverSettings`, we call the `solve` function in the `Solve` module to get the result.

```fsharp
// Create a Settings type which tells the Solver which types of underlying solver to use,
// the time alloted for solving, and whether to write an LP file to disk
let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
}

// Call the `solve` function in the Solve module to evaluate the model
let result = solve settings model
```

The `solve` function returns a `SolveResult` which is a DU with two cases: Optimal or Suboptimal. If the `SolveResult` is the `Optimal` case, it will contain a `Solution`. If the `SolveResult` case is `Suboptimal`, it will hold a `string` which describes what went wrong. In this case the `SolveResult` case is `Optimal` so we can match the case and print out the solution values. The `Solution` type contains `DecisionResult` which is a `Map<Decision,float>`. `DecisionResult` tells you which values the `Solver` picked for the `Decision`s you provided it. The field in the `Solution` is `ObjectiveResult` which is a `float` which tells you how well the `Solver` was able to do.

```fsharp
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
        let (DecisionName name) = decision.Name
        printfn "Decision: %s\tValue: %f" name value
```

If we run this code, we will get the following output

```console
-- Result --
Objective Value: 600.000000
Decision: NumberOfHamburgers    Value: 240.000000
Decision: NumberOfHotDogs       Value: 200.000000
Press any key to close...
```

The rows below the `-- Result --` line show the values the solver found. The solver estimates that we can achieve a profit of $600.00 if we pack for 240 Hamburgers and 200 Hotdogs. You can run this example problem for yourself by running the `FoodTruckExample` problem in the `Flips.Examples` project.

### Using Indices

While the above formulation did work, it does not scale automatically with the number of food items. A better way to formulate this problem would be to store parameter data and decision variables in `Map` instances and use product names as the keys. This means we add a new first step: define the indices. The steps to modeling are now:

1. Define the Indices/Keys for your data
2. Define your data as Maps using your defined Keys
3. Create Decision Variables Maps using your defined Keys
4. Formulating your Objective Function
5. Adding Constraints
6. Solving the Model

We now show how to formulate the problems using the F# `Map` type so that it can scale to an arbitrary number of different items.

```fsharp
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
let maxWeight = Constraint.create "MaxWeight" (weightExpression <== maxTruckWeight)

// Create a Model type and pipe it through the addition of the constraints
let model =
    Model.create objective
    |> Model.addConstraints maxItemConstraints
    |> Model.addConstraint maxWeight
```

We now have a formulation of the problem that will scale to an arbitrary number of items. This is a more maintainable formulation and will not require updating as the variety of items changes over time. Only the input data to the model formulation code need change.


## Why Optimization?

The word "Optimization" is an overloaded term and is used in many domains. It can mean something as simple as, "We looked at the process and made some minor tweaks," all the way to more formal definitions found in [Mathematical Optimization](https://en.wikipedia.org/wiki/Mathematical_optimization). Linear Programming (LP) and Mixed-Integer Programming (MIP) are sub-fields of Mathematical Optimization. It is important to note that LP/MIP emerged as fields of study before Software Programming. The word "Programming" used to mean "Planning". Therefore, it is best to think of LP/MIP as tools for Mathematical Planning. These fields focus on answering the question: Given a set of decisions to be made, what is the best possible plan, while obeying a set of constraints?

The tools of Linear/Mixed-Integer Programming are underutilized. Businesses frequently have problems that would be trivially solved using these tools but instead use complex heuristics involving copy/paste in Excel files instead. There have been many instances where I have observed a domain-expert slaving away for hours at a time with a giant Excel file to answer the question, "What should we do?" After sitting down and talking with these domain-experts it becomes clear they have a planning problem which could be reduced to a 5-minute problem for a computer. There are at least two major reasons for the lack of adoption: lack of awareness and lack of tools.

### Lack of Awareness

LP/MIP is typically found in Operations Research curriculum. In some cases, people are exposed to it when they take a course on Excel and are shown the Solver that is built into Excel as an add-in. The Solver found in Excel is great for small, simple problems but begins to become unwieldly as the complexity and size grow. You eventually need to graduate to more powerful and expressive tools but if you haven't studied LP/MIP, you may not be aware of them. This lack of awareness has lead LP/MIP being relegated to expert users who likely studied it more seriously in university.

### Lack of Tools

When LP/MIP emerged, they were severely limited by the computational power of computers at the time. Eventually, computers become far more powerful and enormous advances in the underlying algorithms were made. We are now able to solve huge LP/MIP problems. Until recently though, to utilize that power you had to learn a specialized language to formulate your problems. This relegated the use of LP/MIP to those who had studied these domain-specific languages.

In the last few years Python has done an incredible service to the software development community by providing a gentle onramp to writing code. Combine Python with Jupyter notebooks and you have a powerful tool for experimenting without all the formalism of "professional" software development. There are some in the LP/MIP community who have recognized this trend and have created libraries for using LP/MIP from Python. I recommend looking at the work of [PuLP](https://github.com/coin-or/pulp) and the excellent [Gurobi Python library](https://www.gurobi.com/documentation/9.0/quickstart_mac/py_python_interface.html).

## Why do we need Flips?

Many of the existing libraries for using LP/MIP are either array-based or heavily Object-Oriented. There is nothing wrong with these approaches, but they run counter to idiomatic F#. F# provides a rich set of tools for expressing problems and algorithms in a functional-first style. After spending several years working in F# and Mathematical Planning, it appeared that there was a gap in the market. A library was needed that allowed an F# developer to express their LP/MIP in a functional way.

Flips is intended to make building and solving LP/MIP problems in F# simple. The hope is that by filling in the gap between current LP/MIP libraries and F# developers the adoption of the awesome tool of LP/MIP will be accelerated.