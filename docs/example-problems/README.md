# Example Problems

The following are a series of example problems which illustrate the capabilities of Flips. It is meant to slowly introduce new concepts with each problem. Each problem is intended to only add a single new concept. By the end you will have seen everything Flips has to offer. Each code example can be copy and pasted into a `.fsx` file which is the file format for F# scripts.

## Intro Series

These problems are meant to build on each other. Each one adds a single new concept. This is meant to provide a gentle introduction to all of the features of the Flips library for modeling.

### [Intro Problem](intro-problem.md)

The simplest problem which shows walks through the basic components of a Optimization Model.

### [Food Truck Intro](food-truck-intro-problem.md)

A simple real world problem which is extended in future examples.

### [Food Truck using Map](food-truck-using-map.md)

This is the same as the Food Truck Intro problem but shows how to formulate the problem using the `Map` type from F# to make models that scale with the number of variables.

### [Food Truck Constraint Builder](food-truck-constraint-builder.md)

Take the previous model and add the use of a `ConstraintBuilder` to automatically create and name constraints for a set of values.

### [Food Truck Decision Builder](food-truck-decision-builder.md)

Introduces the use of the `DecisionBuilder` to more easily create a set of `Decision` for use in your model.

## Intermediate Models

### [Multi Food Truck with Slicing and Units of Measure](multi-food-truck-units-of-measure.md)

This is the final evoluation of the Food Truck series of models. It shows what can be done with all of the features of Flips.

### [Coffee Facilities Planning](coffee-planning.md)

This shows a small facilities planning problem. There are minimum requirements for roasting capacity, warehousing space, and a requirement that a wherever a Roaster is built, a warehouse is also built.

### [Stock Selection](stock-selection.md)

An example provided by [Zbigniew Gajewski](https://github.com/zbigniew-gajewski). This shows how Flips can be used for selecting the best possible collection of stocks.
