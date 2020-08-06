# SliceMaps

## What is Slicing

Up to this point we have been using the built in `Map` type for holding our data and Decision Variables. One of the challenges we will quickly run into when modeling optimization problems is that it is common to operate on subsets of values, often referred to as slices. For these reasons it is common to use N-dimensional Arrays so that you can take slices across different dimensions. Just using the built-in F# `Array` type has limitations though since you can only index the values with an `int`. What we really want is something that allows us to look up a value using an arbitrary index type but also allows us to select ranges of values.

So, we need something that has `Map` like lookup but also allows us to slice across different dimensions...? I know, let us create a new type, a `SliceMap`!

> **Aside**: There was an attempt to simply extend the existing F# `Map` type. Ultimately the combination of features that was required in `SliceMap` made that not possible. Specifically, `SliceMap` is not a single type but a family of types: `SMap`, `SMap2`, `SMap3`, `SMap4`, `SMap5`. The numbers correspond to the dimensionality of the `Key` used in the `Map`. `SMap` is keyed by a single value. `SMap2` is keyed by a tuple of two values. `SMap3` is keyed by a tuple of three values and so forth. These types also have some unique interactions that could not be implemented with just extending the built in `Map` type.

## Types of SliceMaps

SliceMaps are not a single type, they are a family of types akin to Tuples. Tuples can have any number elements: 2, 3, 4, etc. SliceMaps are like tuples in that they have different levels of dimensionality. An `SMap` has a key which is a single element. An `SMap2` has a key which is made up of two elements. An `SMap3` has a key which is made up of three elements. The family of SliceMaps goes up to 5-element keys currently. The type signatures for SliceMaps are the following:

```fsahrp
SMap<'Key, 'Value>
SMap2<'Key1, 'Key2, 'Value>
SMap3<'Key1, 'Key2, 'Key3, 'Value>
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value>
SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value>
```

> **Note:** If I come across a compelling reason to create SMap6, SMap7, etc. I will. At this time, I have not come across a real-world problem which would benefit for having higher dimensional SliceMaps.

SliceMaps support the ability to perform lookup by key, just like the standard F# `Map` type.

```fsharp
// 1D SliceMap
let x1 = SMap.ofList [for i in 1..3 -> i, float i]
x1.[1] // Returns val it : int = 1

// 2D SliceMap
let x2 = [for i in 1..3 do
            for j in 1..3 ->
                    (i, j), i + j]
        |> SMap2.ofList
x2.[1, 2] // Returns val it : int = 3
```

The real power of SliceMaps for Optimization Modeling comes from their ability to "slice" the values across the dimensions of the key. This takes the ability to slice `Array` and `List` but extends it. The slicing built into F# is powerful but it is constrained to a limited set of slicing types. SliceMaps support the same types of slices but offer more advanced filtering criteria which is useful in the Optimization domain.


## Types of Slicing

SliceMaps have several different types of slices they support:

- `All` : All Keys will match
- `Equal x` : Key must be equal to `x`
- `GreaterThan x` : Key values in this dimension must be greater than `x`
- `GreaterOrEqual x` : Key values in this dimension must be greater or equal to `x`
- `LessThan x` : Key values in this dimension must be less than `x`
- `LessOrEqual x` : Key values in this dimension must be less or equal to `x`
- `Between (x, y)` : Key value in this dimension must be between `x` and `y` (inclusive)
- `In (x:Set<'a>)` : Key value in this dimension must be contained in the set `x`
- `NotIn (x:Set<'a>)` : Key value in this dimension must not be in this set `x`
- `Where ('a -> bool)` : Key value in this dimension must return true for the predicate

### `GreaterThan` Slicing

This creates a SliceMap where the Key element is greater than the corresponding value.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterThan 3] 
// Returns SMap<int,int> = SMap map [(4, 4); (5, 5)]
```

### `GreaterOrEqual` Slicing

This creates a SliceMap where the Key element is greater than or equal the corresponding value.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterOrEqual 3] 
// Returns SMap map [(3, 3); (4, 4); (5, 5)]
```

### `LessThan` Slicing

This creates a SliceMap where the Key element is greater than the corresponding value.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
x.[LessThan 3] 
// Returns SMap<int,int> = SMap map [(1, 1); (2, 2)]
```

### `LessOrEqual` Slicing

This creates a SliceMap where the Key element is greater than the corresponding value.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
x.[LessOrEqual 3] 
// Returns SMap<int,int> = SMap map [(1, 1); (2, 2); (3, 3)]
```

### `Between` Slicing

This creates a SliceMap where the Key element is between the Lower Bound and Upper Bound inclusive.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
x.[Between (3, 4)] 
// Returns SMap<int,int> = SMap map [(3, 3); (4, 4)]
```

### `In` Slicing

The `In` slice takes a set of values. If the Key value for that dimension is in that set, then the filter will include the entry.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
let indexSet = Set.ofList [2; 4]
x.[In indexSet] 
// Returns SMap<int,int> = SMap map [(2, 2); (4, 4)]
```

### `NotIn` Slicing

The `NotIn` slice takes a set of values. If the Key value for that dimension is NOT in that set, then the filter will include the entry.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
let indexSet = Set.ofList [2; 4]
x.[NotIn indexSet] 
// Returns SMap<int,int> = SMap map [(1, 1); (3, 3); (5, 5)]
```

### `All` Slicing

The `All` Slice is used to select all of the values in a given dimension. It is the akin of a wildcard.

```fsharp
let x1 = SMap.ofList [for i in 1..5 -> i, i]
x1.[All] 
// Return SMap map [(1, 1); (2, 2); (3, 3); ... ]
```

In the case of an `SMap` (1-dimensional SliceMap) the `All` slicing is not exciting. It is simply returning all the values. It becomes more useful when dealing with higher dimensional SliceMaps.

### `Where` Slicing

 The `Where` slice is provided to give you the ability to slice with some arbitrary predicate. You provide a function which will be used to evaluate the key along the dimension. For the keys where the predicate returns `true`, the entry will be returned. Where the predicate returns `false` the value will not be included in the returned SliceMap.

```fsharp
let x = SMap.ofList [for i in 1..5 -> i, i]
let isDivisibleBy2 x = x % 2 = 0
x.[Where isDivisibleBy2] 
// Return SMap<int,int> = SMap map [(2, 2); (4, 4)]
```

## Slicing for 2D, 3D, and 4D SliceMaps

The examples above have shown what the slicing behavior is for a 1D SliceMap, a `SMap`. While useful for a single dimensional SliceMap, the utility of slicing is increased when you have higher dimensional data. When working with 2, 3, and 4 dimensional SliceMaps, it is important to note that the returned keys and values must meet all the conditions of the slices. Let us see how this plays out with a `SMap2`.

```fsharp
let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
x.[GreaterThan 1, LessThan "b"] 
// Returns SMap2 map [((2, a), 3); 
//                    ((3, a), 4)]
```

In this case we are saying that the keys of the first dimension must be greater than 1 and the keys of the second dimension must be less than "b". This leaves only two entries from the original `SMap2`. Let us look at another example.

```fsharp
let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
x.[GreaterOrEqual 2, LessOrEqual "b"] 
// Returns SMap2 map [((2, "a"), 3.0); ((2, "b"), 1.0); 
//                    ((3, "a"), 4.0); ((3, "b"), 1.5)]
```

Here we are only returning the entries where the value of the first key dimension is greater or equal to 2 and the value of the second dimension is less or equal to "b". The great thing about F# is that you can use any type for the key dimensions if they support `comparison`.

## Sub-setting SliceMaps

In many mainstream programming languages, programmers are not required to think about the dimensionality of their data. Most languages will have scalar values and collections of values. Though we may not think about it, a Scalar (`int`, `string`, `float`) value is a 0-Dimensional piece of data. When we add collections, the dimensionality can increase. An `Array` in F# is a 1-Dimensional storage of data. The same can be said of `Map` and `List`. Now, F# also has 2D and 3D Array. To look up data in these data structures you need to specify multiple index values because data is organized in multiple dimensions.

Now, let us see what item look and slicing looks like for a 2D Array in F#.

```fsharp
let x = array2D [ [ 1; 2]; [3; 4] ] // Create a 2D Array
// Returns int [,] = [[1; 2]
//                    [3; 4]]
let y = x.[1, 1] // Select an element
// Returns val y : int = 4
let xSlice = x.[*, 1] // Take all rows, but only the second column
// Returns val xSlice : int [] = [|2; 4|]
```

At first, we create our 2D Array `x`. You will see that `x` is a 2D Array by its type signature, `int [,]`. We then assign `y` the value at row index `1` and column index `1`. You will notice that `y` is 0-Dimensional, it is a Scalar. We then assign `xSlice` a slice of `x` by taking all the rows but only the column at index `1`. You will notice that `xSlice` is a 1-Dimensional array.

What I am trying to drive home is that when you look up an item or slice an Array, you can get different dimensionality of data. SliceMaps have the same behavior as F# Arrays. Let us walk through some examples using a 2-dimensional SliceMap, a `SMap2`.

```fsharp
// Create our `SMap2`
let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
let a = x.[1, "a"] // Select a single element
// val a : float = 2.0
```

Notice that `a` is a Scalar, a 0-Dimensional piece of data

```fsharp
// Slice across the first dimension, select only the "a" values in the second
let b = x.[GreaterThan 1, "a"] 
// val b : SMap<int,float> = SMap map [(2, 3); (3, 4)]
```

Here `b` no longer has the second-dimension index. It is an `SMap`. This is because we selected only the data where the second index was equal to "a".

```fsharp
// Slice across the first and second dimensions
let c = x.[GreaterThan 1, LessThan "b"]
// val c : SMap2<int,string,float> = SMap2 map [((2, a), 3); ((3, a), 4)]
```

Note that `c` is a subset of `x` but it is still 2-dimensional, an `SMap2`. This type of behavior holds true for all the SliceMaps.

## Operators for SliceMaps

SliceMaps support scalar multiplication using `*`.

```fsharp
let x = SMap.ofList [for i in 1..3 -> i, float i]

> x * 2.0
// Returns SMap<int,float> = SMap map [(1, 2.0); (2, 4.0); (3, 6.0)]
```

SliceMaps can also be added together. In the case where the keys match, the underlying values are added together. In the cases where the key exists in only one map, the value is still returned. The assumption is that the value in the other SliceMap was a `Zero` equivalent. The dimensionality of the SliceMap must match and the key types must align as well.

```fsharp
let x = SMap.ofList [for i in 1..3 -> i, i]
let y = SMap.ofList [for i in 2..5 -> i, i]
x + y 
// Returns SMap<int,int> = SMap map [(1, 1); (2, 4); (3, 6); (4, 4); (5, 5)]
```

Finally, SliceMaps support element-wise multiplication using the `.*` operator. This operator was first seen in MatLab but has been adopted by other languages as well. The SliceMaps will have their values multiplied together where the keys match. When the keys do not match, no value is returned. This behavior is similar to inner-joins in SQL. The behavior is intended to be the same as the [Hadamard Opertor](https://en.wikipedia.org/wiki/Hadamard_product_(matrices)) in Linear Algebra. The difference is that SliceMaps can be keyed by any type that supports `comparison`, not just `int`.

```fsharp
let x = SMap.ofList [for i in 1..3 -> i, i]
let y = SMap.ofList [for i in 2..5 -> i, i]
x .* y 
// Return SMap<int,int> = SMap map [(2, 4); (3, 9)]
```

The `.*` becomes incredibly useful in formulating Constraints as you will see in future examples.

### Broadcasting for SliceMaps

The `.*` operator also supports the idea of Broadcasting. This concept is similar to Broadcasting in the [Numpy Library](https://numpy.org/doc/stable/user/theory.broadcasting.html#array-broadcasting-in-numpy) and the [Julia language](https://docs.julialang.org/en/v1/manual/arrays/#Broadcasting-1). The idea is that when performing the element-wise multiplication of two SliceMaps of different dimensions, the values are repeated along the unmatched dimension. This behavior is not exactly the same as what happens in Numpy and Julia, but the concept is the same.

Here is an example of element-wise multiplication of a `SMap` and a `SMap2`.

```fsharp
let x = SMap2.ofList [
    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
]

let y = SMap.ofList [(1, 1.0); (2, 2.0); (3, 3.0)]

y .* x
// Returns 
// SMap2 map [((1, "a"),  1.0); ((1, "b"),  2.0); ((1, "c"),  3.0);
//            ((2, "a"),  8.0); ((2, "b"), 10.0); ((2, "c"), 12.0);
//            ((3, "a"), 21.0); ((3, "b"), 24.0); ((3, "c"), 27.0)];}
```

You will notice that where the key of `y` matches the first element of the key for `x`, the values are multiplied. The resulting type is still a 2-dimensional SliceMap, `SMap2`. Something important to note is that in the `.*` operator is not commutative (i.e. positioning matters) for SliceMaps of different dimensions. If you tried the following, you would get a compiler error saying no overloads can be found.

```fsharp
x .* y // No matching operator can be found
```

When broadcasting with the `.*` operator, it is required that the types used for the keys to line up. In the first case, the key for `y` is an `int`. `y` is on the left side of `x` so it is comparing to the first element of the key for `x`. The type of the first element of the key for `x` is an `int` therefore you can use `.*`.

When you move `y` to the right side, the `.*` operator is checking the second element (or right-most element) of the key for `x`. In this case, the type of the second element of the `x` key is a `string`. The types do not match. Let us create a new `SMap` using a `string` for the key and try this again.

```fsharp
let x = SMap2.ofList [
    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
]
let z = SMap.ofList [("a", 1.0); ("b", 2.0); ("c", 3.0)]

x .* z
// Returns
// SMap2 map [((1, "a"), 1.0); ((1, "b"),  4.0); ((1, "c"),  9.0);
//            ((2, "a"), 4.0); ((2, "b"), 10.0); ((2, "c"), 18.0);
//            ((3, "a"), 7.0); ((3, "b"), 16.0); ((3, "c"), 27.0)];}

z .* x // Compiler error: No matching operator can be found
```

You will notice now that the value in `x` is multiplied by the value in `z` where the second element of the key for `x` matches. The requirement for broadcasting can be summarized with these type signatures

```fsharp
// SMap and SMap2
SMap<'Key1, 'Value> .* SMap2<'Key1, 'Key2, 'Value>
SMap2<'Key1, 'Key2, 'Value> .* SMap<'Key2, 'Value>

// SMap and SMap3
SMap<'Key1, 'Value> .* SMap3<'Key1, 'Key2, 'Key3, 'Value>
SMap3<'Key1, 'Key2, 'Key3, 'Value> .* SMap<'Key3, 'Value>

// SMap and SMap4
SMap<'Key1, 'Value> .* SMap4<'Key1, 'Key2, 'Key3, 'Key4 'Value>
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> .* SMap<'Key4, 'Value>

// SMap and SMap5
SMap<'Key1, 'Value> .* SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Key5 'Value>
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> .* SMap<'Key5, 'Value>

// SMap2 and SMap3
SMap2<'Key1, 'Key2, 'Value> .* SMap3<'Key1, 'Key2, 'Key3, 'Value>
SMap3<'Key1, 'Key2, 'Key3, 'Value> .* SMap2<'Key2, 'Key3, 'Value>

// Smap2 and SMap4
SMap2<'Key1, 'Key2, 'Value> .* SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value>
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> .* SMap2<'Key3, 'Key4, 'Value>

// Smap2 and SMap5
SMap2<'Key1, 'Key2, 'Value> .* SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value>
SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> .* SMap2<'Key4, 'Key5, 'Value>

// SMap3 and SMap4
SMap3<'Key1, 'Key2, 'Key3, 'Value> .* SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value>
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> .* SMap3<'Key2, 'Key3, 'Key4, 'Value>

// SMap3 and SMap5
SMap3<'Key1, 'Key2, 'Key3, 'Value> .* SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value>
SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> .* SMap3<'Key3, 'Key4, 'Key5, 'Value>

// SMap4 and SMap5
SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> .* SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value>
SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> .* SMap4<'Key2, 'Key3, 'Key4, 'Key5, 'Value>
```

The important thing to remember, when multiplying a high-dimensional SliceMap by a lower-dimensional SliceMap, the keys used for matching are dependent on the side of the lower-dimensional SliceMap. If the lower-dimensional SliceMap is on the left then it is the first elements of the keys that must match. If the lower-dimensional SliceMap is on the right, then the last elements of the keys must match.

Using the `.*` operator with SliceMaps of different dimensions will always yield a SliceMap of the higher dimensionality. `SMap .* SMap2` will yield a `SMap2`. `SMap2 .* SMap3` will yield a `SMap3` etc.

## Slicing and Domain Driven Design

One of the most powerful facilities of F# is the ability to accurately model a domain. Instead of a `string` just being a string, it is a City Name. Instead of a `int` just being an `int`, it is an Index. This is often done using Single-Case Discriminated Unions. The topic of Domain Driven Design is beyond the scope of this intro. For further reading, please refer to the excellent book [Domain Modeling Made Functional](https://pragprog.com/book/swdddf/domain-modeling-made-functional) by Scott Wlaschin.

For our use case, the use of Single-Case DUs allows us to keep track of what the primitive types (`int`, `float`, `string`) correspond to. It is highly encouraged to wrap these primitives in single-case DUs when they are being used as keys in SliceMaps. The slicing behavior still work as you would expect.

```fsharp
type City = City of string
type Index = Index of int
let x = SMap2.ofList [
    (Index 1, City "a"), 2.0; (Index 1, City "b"), 2.0; (Index 1, City "c"), 2.1; 
    (Index 2, City "a"), 3.0; (Index 2, City "b"), 1.0; (Index 2, City "c"), 2.3; 
    (Index 3, City "a"), 4.0; (Index 3, City "b"), 1.5; (Index 3, City "c"), 2.4; 
]
x.[GreaterOrEqual (Index 2), LessOrEqual (City "b")]
// Returns SMap2 map [((Index 2, City "a"), 3.0); ((Index 2, City "b"), 1.0);
//                    ((Index 3, City "a"), 4.0); ((Index 3, City "b"), 1.5)]
```

You can see that the slicing behavior understands it needs to operate on the inner value of the single-case DU. This is due to the magic of F#. While the wrapping of the primitives in single-case DUs may feel over the top for simple modeling, it has payed dividends in real-world scenarios.

## Example Using SliceMaps

Sometimes the best way to see the utility of a new tool is to experience working on a problem without it. Let us take our Food Cart problem and add some complexity. Now we are not managing a single food cart but multiple. We have three different locations we are managing, and we have added Pizza to the menus. Each food cart has a different weight limit and a different profit amount per item.

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

When we create our `maxItemConstraints` and `maxWeightConstraints` we are having to sum the decisions across a new dimension. For the `maxItemsConstraints` we must sum the items across the Location dimension:

```fsharp
let locationSum = List.sum [for location in locations -> numberOfItem.[location, item]]
```

For the `maxWeightConstraints` we have to sum across the Items dimension:

```fsharp
let weightSum = List.sum [for item in items -> itemWeight.[item] * numberOfItem.[location, item]]
```

In this example this is not so bad but imagine problems where the dimensionality is higher. You end up having nested `List` comprehensions. This summation across dimensions is so common that the `SliceMap` family of types was created. Let us revisit the problem but we will use SliceMaps instead of the built in `Map` type. If the Key for the data is a single dimension, we will use an `SMap`. If the key is two dimensional, we will use an `SMap2`.

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

We can see that things look a little different now. The first change is that we are storing our data in a `SMap` or `SMap2`. All the SliceMaps support being created from a `List`, `Seq`, or `Array`. This behavior is the same as the equivalent functions for `Map`. You can even create a SliceMap from a `Map` if the dimensionality of the `'Key` matches what the SliceMap is expecting.

The next major change is in the `objectiveExpression` creation:

```fsharp
let objectiveExpression = sum (profit .* numberOfItem)
```

Here we are using two of the features that SliceMaps provide: summation and element-wise multiplication. The `.*` operator is an element-wise multiplication of the values in the SliceMaps. When the keys match in both SliceMaps, the values are multiplied together. In the cases where the keys do not match, nothing is returned. If you are familiar with SQL, this behavior is the equivalent of an inner-join. The `.*` comes from MATLAB and has been implemented in other languages. It is the [Hadamard Opertor](https://en.wikipedia.org/wiki/Hadamard_product_(matrices)) if you are curious.

The `sum` function is a convenience function to make modeling more streamlined. It can only be used on types which have a `sum` method declared on them. It simply looks at the type and calls its associated `sum` method. All the SliceMaps have a `sum` method. When sum is called, all the values in the SliceMap are summed together using the `+` operator. SliceMaps were intended to be used with types which implement `+`, `*`, and `Zero`. The mathematical term is a [Ring](https://en.wikipedia.org/wiki/Ring_(mathematics)).

The next change we see in the model formulation is in the creation of `maxItemConstraints`. Specifically, on the line where we create the constraint.

```fsharp
Constraint.create name (sum numberOfItem.[All, item] <== maxIngredients.[item])
```

We are using the slicing capability of SliceMaps. For this constraint we are wanting to sum how much of a given Item we are sending across all the Locations. Before this was done using a List comprehension. Here we are slicing and then summing the resulting SliceMap. Remember that the first dimension to the `numberOfItem` SliceMap is the Location. This expression, `numberOfItem.[All, item]`, is saying to select items in the SliceMap for `All` the locations but only where the `item` key matches. This slicing then returns a new SliceMap. The returned SliceMap is summed to form the left-hand side of our Constraint Expression.

## The `sum` and `sumAll` Functions

A couple of convenience functions were added for summing up the values held inside of collections. The `sum` functions calls the `Sum` method defined on the type. The `sumAll` functions is meant to sum a collection of summable types (Ex: `List<float>`). These are to help a Domain Expert write code that more closely matches the math notation of optimization problems.