# Flips 3.0 Goals

Flips has now been out for over a year. It has been exciting to see people find the project and pick it up. I've had the joy of using it in several major projects. It's now being used as a core solver in a new simulation project that I am working on. It has been a joy answering people questions and the support from the community has been great. I still believe that people come to F# for the awesome language but stay for the wonderful community.

Over the last year the need for Flips to evolve has become evident. There are some significant design flaws that need to be addressed. Instead of trying to quickly patch these, I decided to sit back and see how people were using the library. I didn't want us to jump from one design problem to the next. I wanted to understand what the community really needed out of the tool before making more significant changes. The next major change of Flips will definitely have some breaking changes, but we want to make that as painless as possible. There is new functionality we want to add that we had no visibility of before.

The major changes breakdown into 3 categories:

- Make Boolean Logic Easy
- Make Solvers independently configurable
- Make `SliceMap` fast with sparse data

## Make Boolean Logic Easy

It is common in Mixed-Integer programming to use Boolean decision variables to model business requirements. An example I gave in a workshop is a facility planning problem for coffee manufacturing. The problem states that wherever you place a coffee roasting facility, you must also build a warehouse. For our example let's call these decision variables `buildRoaster` and `buildWarehouse`.

```fsharp
let buildRoaster = Decision.boolean "BuildWarehouse"
let buildWarehouse = Decision.boolean "BuildWarehouse"
```

Now we need to express a constraint which states that if we build the roaster, we must also build the warehouse. Well, if you are experienced writing these models that may seem easy. You can just use a `<==` operator to create a constraint expression.

```fsharp
let constraintExpression = 
    buildRoaster <== buildWarehouse
let warehouseRequired = 
    Constraint.create "WarehouseRequired" constraintExpr
```

In this model, `Boolean` decision variables take on values of `0` or `1`. This means that if we decide to build the roaster, the `buildRoaster` decision variable will take on the value of `1`. When that happens, `buildWarehouse` must also take on a value of `1` for the constraint expression `buildRoaster <== buildWarehouse` to be satisfied. This may now make sense to you, but it took me awhile to explain it. [Tyson Williams](https://twitter.com/tyson_mn) actually suggested to me providing some convenience methods for expressing these constraints in a way a developer would be more familiar with. My thought is to add methods to the `BooleanDecision` type which would allow this:

```fsharp
let constraintExpression = 
    buildRoaster.Requires buildWarehouse
let warehouseRequired = 
    Constraint.create "WarehouseRequired" constraintExpr
```

There could be a whole suite of methods/functions for being able to express these complex logic relationships that would be easier than figuring out the correct incantation using `==`, `<==`, and `>==`.

```fsharp
// Example methods
let a = Decision.boolean "A"
let b = Decision.boolean "B"

// A excludes B
a.Excludes b

// A requires B
a.Requires b
```

We could even have functions for generating constraints on larger groups of `Boolean` decision variables.

```fsharp
let a = Decision.boolean "A"
let b = Decision.boolean "B"
let c = Decision.boolean "C"

// When we only want one from the list to be 1
let x = BooleanConstraint.exactlyOne [a; b; c]

// We could require at least decision to be 1
let x = BooleanConstraint.atLeastOne [a; b; c]

// Really those first two examples are special cases.
// We could say we want exactly N decisions to be 1 or
// at least N must be 1
let x = BooleanConstraint.exactlyN n [a; b; c]
let x = BooleanConstraint.atLeastN n [a; b; c]
```

There's a lot more that can be done here but this is a small sample. Modeling boolean logic with Flips is possible right now but is not as intuitive as it could be.

## Make Solvers independently configurable

This is a big one and I discussed it in an earlier [post](https://flipslibrary.com/#/blog/2020-11-21-next-steps-for-flips/README). Right now, Flips uses a single solver config type. This means that it is restricted to the intersect of the features all the solvers support. This is less than ideal. Going forward we plan to separate the modeling code from the solving code. This means Flips will be decomposed into separate packages. This will lead to a lighter install. It will enable us to support most of the features of the different solvers.

The original design was intended to keep Flips simple to use. Over time the need for flexibility has grown and we want to enable Flips to be used in more demanding scenarios. [Gauthier](https://twitter.com/zerolehero) has been a big help here and has taken over the code for the CPLEX solver.

## Make `SliceMap` fast with sparse data

The `SliceMap` family of types was intended to make formulating models more streamlined. I enjoy using them, but they were originally designed for densely populated data. Many real-world problems involve sparse data which can cause models using `SliceMap` to slow down. I have some ideas for how to speed this up but first, I need to separate the `SliceMap` code out of Flips. I have already migrated the code for `SliceMap` to its own repo but it has not been removed from Flips. When we go to Flips 3.0, it will set the stage for `SliceMap` be able to evolve independently. The speed ups will likely me a few weeks to play around with and test.

## Next Steps

I have settled into my new job, so I am now turning back to growing Flips. I am hoping to have beta out within a month. I then plan on soliciting feedback from people who are using it. If things go well. I'm hoping to have a new major version of Flips out in the next few months. There is also a plan to add an Experimental package for Flips. This will allow us to play around with some new features that we don't want to support yet since their API is not stable yet.

Thank you for your patience. Flips is a labor of love and I hope to continue helping people by making Mathematical Planning easy and approachable for the F# community.
