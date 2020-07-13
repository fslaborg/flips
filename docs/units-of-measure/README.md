# Units of Measure

It is also possible to build Optimization Models using Units of Measure to ensure that you formulations make mathematical sense. The idea with Units of Measure is that you annotate your numbers with the units they are meant to represent. This means you no longer just have the number `2.5`. You have the number `2.5<Kg>`. The F# compiler will ensure that when you add, subtract, multiply, and divide with these numbers that the units are carried through. There is not performance for using this. The compiler checks that the units make sense but they are erased when the code is compiled so there is no performance impact.

The Flips library enables you to use Units of Measure but does not force you to. Units of Measure are meant to be an all-in feature, should you wish to enable them. To use Units of Measure, you just open the `Flips.UnitsOfMeasure` namespace. This will override the default implementations of types and functions with the Units of Measure aware alternatives. Your opening statements should be in this order.

```fsharp
open Flips
open Flips.Types
open Flips.UnitsOfMeasure
open Flips.UnitsOfMeasure.Types
```

The ordering is critical because if there is a name conflict, the last `open` statement will shadow previous declarations. Below you will find a disussion of which types are updated with Units of Measure aware versions. Please refer to the Examples section to see fully worked problems which include Units of Measure.

## Decision

The `Decision` type now has a `Decision<'Measure>` version which is aware of units of measure. This means that when you want to perform a `+` operation, the Units of Measure must be the same on both sides of the `+` operator. For example, normally adding a `Decision` and a `float` would yield a `LinearExpression`. Now, if the units of the `Decision` and the `float` do not match, there will be a compiler error.

```fsharp
let d = Decision.createContinuous "Test" 0.0<Kg> 100.0<kg>
// d : Decision<Kg>

d + 1.0<Kg> // This will work
d + 1.0<Lb> // This will throw a compiler error
```

When you multiply a `Decision<'Measure>` by a `float<'Measure>`, the `LinearExpression` that is produced will have Units of Measure that are the product of the two types. For example:

```fsharp
let d = Decision.createContinuous "Test" 0.0<Item> 100.0<Item>
// d : Decision<Item>
let expr = d * 10.0<sec>
// expr : LinearExpression<Item sec>
```

Units of Measure will also cancel out appropriately should the same unit be in the nominator and the divisor.

```fsharp
let d = Decision.createContinuous "Test" 0.0<Item> 100.0<Item>
// d : Decision<Item>
let expr = d * 10.0<Kg/Item>
// expr : LinearExpression<Kg>
```

### Create Decision with Units of Measure

There are new ways for creating a `Decision` with units of measure. All of the `Decision.create*` functions now take a `'Measure` type annotation. This will force the values of the Upper and Lower Bounds to match the units of the `Decision` you are trying to create.

```fsharp
// Creating Boolean Decision with Kg units
let d1 = Decision.createBoolean<Kg> "d1"
// d1 : Decision<Kg>

// Creating Continuous Decision with Kg units
let d2 = Decision.createContinuous<Kg> "d2" 0.0<Kg> 100.0<Kg>
// d2 : Decision<Kg>

// Creating Integer Decision with Kg units
let d3 = Decision.createInteger<Kg> "d3" 0.0<Kg> 100.0<Kg>
// d3 : Decision<Kg>
```

### DecisionBuilder

It is also possible to create `Decision<'Measure>` with a `DecisionBuilder`. This method requires adding a `'Measure` type annotation to the builder.  Here is example of building a decision for each `Location` and `Item` where the `Decision` has a Units of Measure of `Item`.

```fsharp
let items = ["Hamburger"; "HotDog"; "Pizza"]
let locations = ["Woodstock"; "Sellwood"; "Portland"]

let numberOfItemDecisions =
    DecisionBuilder<Item> "NumberOf" {
        for location in locations do
        for item in items ->
            Continuous (0.0<Item>, 1_000_000.0<Item>)
    } 
```

> **Note**: It is not possible to add a Unit of Measure annotation to the value `infinity` which is what is often used as an upper bound for Continuous decision variables. It is suggested to declare a `MAX_VALUE` constant for your model and use that value throughout your model to simplify development. In the previous example you could declare `let MAX_ITEM_VALUE = 1_000_000.0<Item>` and use that for the Upper Bound.

## LinearExpression

The type `LinearExpression` is now also aware of Units of Measure. This means that when you try to add them together, the units of both expressions must match. When you multiply a `LinearExpression` by a `float` which has units of measure, the units are appropriatly updated.

```fsharp
expr1 // LinearExpression<Item>
expr2 // LinearExpression<Item>
expr3 // LinearExpression<Kg>

expr1 + expr2 // This will work
expr1 + expr3 // This will throw a compiler error

expr1 * 1.0<1/Kg> // Yields LinearExpression<Item/Kg>
expr1 * 1.0<Lb> // Yields LinearExpression<Item Lb>
```

This need for the units to match also extends to when you express constraints. The comstraint operators `==`, `<==`, and `>==` all require that the units on both sides of the operator match. If not, there will be a compiler error.

```fsharp
expr1 // LinearExpression<Item>
expr2 // LinearExpression<Item>
expr3 // LinearExpression<Kg>

expr1 >== expr2 // This will work
expr1 >== expr3 // This will throw a compiler error
```