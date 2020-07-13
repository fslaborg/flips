# The Flips Domain

## Scalar

A `Scalar` is a single case DU which wraps a `float`. The reason for this was to ensure proper equality behavior. Equality is incredibly difficult with any floating-point type. In order to ensure that modeling works as intended it was necessary to wrap the `float` value and specify the equality behavior.

You should not be constructing the `Scalar` type yourself. Just work with `float`, `Decision`, and `LinearExpression` and the library will take care of wrapping these values.

## Decision

A `Decision` represents a choice that the Solver needs to make. It is a DU with three cases: `Boolean`, `Integer`, and `Continuous`. The `Boolean` case is meant to model a True/False, Yes/No type of decision. Examples are whether to build a factory or not, whether to open a store or not, start a power plant or not, or any other type of decision where a state must be true or false and nothing in between. The `Integer` case represents a decision that can take on discrete values. When you create them, you must specify a Lower and Upper bound. The bounds are the limits for the Solver so that it knows what the legal range of values for the decision is. The `Continuous` case has Lower and Upper bounds like the `Integer` case except that any value within the bounds is allowed, not just the discrete values. `Continuous` decisions are the most common and should be the default for better performance from the Solver.

## LinearExpression

A `LinearExpression` is the addition of one or more `Scalar` or `Decision` values. It is meant to represent math expressions like [`2.3 + 3.4*x1 + 4.5*x2`] where `x1` and `x2` are `Decision`s. `LinearExpression`s are meant as the unit of compositions for Optimization Models. Constraints and Objectives are expressed in terms of `LinearExpression`s. The `LinearExpression` type forms a Monoid with the `+` operator.

## Constraint

A `Constraint` is composed of a Name and a comparison (`==`, `<==`, `>==`) of two `LinearExpression`s. The Name for a `Constraint` should be indicative of what it is controlling for. Best practice is to prefix a set of `Constraint` with an identifier followed by the indices that the individual `Constraint` correspond to. There is a `ConstraintBuilder` Computation Expression to streamline this.

## Objective

An `Objective` is composed of three parts: a `ObjectiveName`, a `ObjectiveSense`, and a `LinearExpression`. The `ObjectiveName` is meant to clarify what the goal of the Optimization Model is. For example, "MaximizeRevenue" or "MinimizeWaste" or "MaximizeSafety". The `ObjectiveName` has no bearing on what the Solver does. It is meant to document what the purpose of the Model was when it was written.

The `ObjectiveSense` tells the Solver whether it is trying to Maximize or Minimize the `LinearExpression` in the `Objective`. The `LinearExpression` is the way the Solver will measure success. It will either try to make the expression be as large as possible for `Maximize` or as small as possible for `Minimize`.

## Model

The `Model` type contains the full description of the problem. It holds the `Decision`s that need to be made, the `Constraint`s that must be adhered to, and the `Objective` that is trying to be achieved. An `Objective` must be provided when creating a `Model`. From there `Constraint`s can be added to the `Model` using the `Model.addConstraint` or `Model.addConstraints` functions. 

Once all the `Constraint`s have been added the `solve` function can be called. The `solve` function returns a `SolveResult` type. The `SolveResult` type is a DU with two cases: an `Optimal` case containing a `Solution` or the `Suboptimal` case containing an error message.

## Solution

In the case that the `Model` was solved succesfully, a `SolveResult` is returned containing a `Solution`. The `Solution` contains two things: DecisionResults and ObjectiveResult. The DecisionResults is a `Map<Decision,float>` which provides the value the Solver found for the given `Decision`. The ObjectiveResult is a `float` representing the final value of the `LinearExpression` for the `Objective`.

## Decision Builder

It is common to need to create a `Decision` which corresponds to a set of one or more indices. In order to streamline this work a Computatino Expression was created called `DecisionBuilder`. `DecisionBuilder` takes the prefix you would like to use for the names of the `Decision`s you are about to create. It then automatically adds meaningful suffixes which correspond to the index you are create the `Decision` for. Here is an example of how this works without the `DecisionBuilder` and then again with the `DecisionBuilder` to show the difference.

```fsharp
let indexes = [1..3]
let locations = ["CityA"; "CityB"; "CityC"]

// Creating a Map of decisions without the DecisionBuilder
let withoutDecisionBuilder =
    [for i in indexes do
        for l in locations ->
            let name = sprintf "Test|%i_%s" i l
            let decisionType = DecisionType.Continuous (0.0, infinity)
            (i, l), Decision.create name decisionType
    ] |> Map.ofList

// Creating a Map of decisions with the DecisionBuilder
let withDecisionBuilder =
    DecisionBuilder "Test" {
        for i in indexes do
            for l in locations ->
                Continuous (0.0, infinity)
    } |> Map.ofSeq
```

You will see that the `DecisionBuilder` removed some of the ceremony around naming and indexing the `Decision`s.

## Constraint Builder

Since the creation of constraints is such a common occurrence in modeling, a `ConstraintBuilder` Computation Expression (CE) was made to streamline the naming of constraints. The idea is that you give a prefix for the set of constraints you are going to create, and the Computation Expression takes care of naming the constraints you are creating. Here is a side by side comparison of creating constraints without and with the `ConstraintBuilder` CE. The results of both methods are equivalent. The method with `ConstraintBuilder` CE is more succinct. Over time, the added brevity is appreciated. This is showing how to create constraints across two dimensions: Items and Locations.

```fsharp
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

