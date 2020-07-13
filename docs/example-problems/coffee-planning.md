# Coffee Facilities Planning

This problems provides and example of planning the necessary facilities to roast and warehouse coffee production. There is a minimum roasing capacity of 30.0 Ton, a minimum warehousing capacity of 30,000 sq.ft. and a requirement that anywhere where there is a Roaster, there is also a Warehouse. This shows the use of Units of Measure and some mild Domain modeling using single case discriminated union `Location`.

```fsharp
open Flips
open Flips.Types
open Flips.UnitsOfMeasure
open Flips.UnitsOfMeasure.Types
open Flips.SliceMap

type [<Measure>] USD
type [<Measure>] ft
type [<Measure>] Ton
type [<Measure>] Build

type Location = Location of string

let minRoastingCapacity = 30.0<Ton>
let minWarehouseCapacity = 30_000.0<ft^3>

let locations = 
    [
        "Sellwood"
        "Hawthorne"
        "The Pearl"
        "Eastmoreland"
        "St Johns"
        "Alberta"
        "Nob Hill"
        "Belmont"
    ] |> List.map Location

let roasterCost = 
    [
        Location "Sellwood"      , 150_000.0<USD/Build>
        Location "Hawthorne"     , 100_000.0<USD/Build> 
        Location "The Pearl"     , 250_000.0<USD/Build>
        Location "Eastmoreland"  , 120_000.0<USD/Build>
        Location "St Johns"      , 130_000.0<USD/Build>
        Location "Alberta"       , 110_000.0<USD/Build>
        Location "Nob Hill"      , 135_000.0<USD/Build>
        Location "Belmont"       , 180_000.0<USD/Build>
    ] |> SMap.ofList

let roasterCapacity = 
    [
        Location "Sellwood"      , 12.0<Ton/Build>
        Location "Hawthorne"     , 18.0<Ton/Build> 
        Location "The Pearl"     , 22.0<Ton/Build>
        Location "Eastmoreland"  , 13.0<Ton/Build>
        Location "St Johns"      , 14.0<Ton/Build>
        Location "Alberta"       , 10.0<Ton/Build>
        Location "Nob Hill"      , 17.0<Ton/Build>
        Location "Belmont"       , 12.0<Ton/Build>
    ] |> SMap.ofList

let warehouseCost = 
    [
        Location "Sellwood"      ,  80_000.0<USD/Build>
        Location "Hawthorne"     ,  90_000.0<USD/Build> 
        Location "The Pearl"     , 120_000.0<USD/Build>
        Location "Eastmoreland"  ,  90_000.0<USD/Build>
        Location "St Johns"      ,  85_000.0<USD/Build>
        Location "Alberta"       ,  70_000.0<USD/Build>
        Location "Nob Hill"      ,  85_000.0<USD/Build>
        Location "Belmont"       ,  90_000.0<USD/Build>
    ] |> SMap.ofList

let warehouseCapacity = 
    [
        Location "Sellwood"      ,  8_000.0<ft^3/Build>
        Location "Hawthorne"     ,  6_000.0<ft^3/Build> 
        Location "The Pearl"     , 12_000.0<ft^3/Build>
        Location "Eastmoreland"  ,  6_000.0<ft^3/Build>
        Location "St Johns"      ,  7_000.0<ft^3/Build>
        Location "Alberta"       ,  9_000.0<ft^3/Build>
        Location "Nob Hill"      ,  6_000.0<ft^3/Build>
        Location "Belmont"       ,  9_200.0<ft^3/Build>
    ] |> SMap.ofList

let buildRoaster =
    DecisionBuilder<Build> "BuildRoaster" {
        for l in locations ->
            Boolean
    } |> SMap.ofSeq

let buildWarehouse =
    DecisionBuilder<Build> "BuildWarehouse" {
        for l in locations ->
            Boolean
    } |> SMap.ofSeq

let minRoastingCapacityConstraint =
    Constraint.create "MinRoastingCapacity" (sum (buildRoaster .* roasterCapacity) >== minRoastingCapacity)

let minWarehouseCapacityConstraint =
    Constraint.create "MinWarehouseCapacity" (sum (buildWarehouse .* warehouseCapacity) >== minWarehouseCapacity)

let warehouseWithRoasterConstraints =
    ConstraintBuilder "WarehouseWithRoaster" {
        for l in locations ->
            buildRoaster.[l] <== buildWarehouse.[l]
    }

let objExpr = sum (buildRoaster .* roasterCost + buildWarehouse .* warehouseCost)
let objective = Objective.create "MinimizeCost" Minimize objExpr

let model = 
    Model.create objective
    |> Model.addConstraint minRoastingCapacityConstraint
    |> Model.addConstraint minWarehouseCapacityConstraint
    |> Model.addConstraints warehouseWithRoasterConstraints

let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
}

// Call the `solve` function in the Solve module to evaluate the model
let result = Solver.solve settings model

printfn "-- Result --"

// Match the result of the call to solve
// If the model could not be solved it will return a `Suboptimal` case with a message as to why
// If the model could be solved, it will print the value of the Objective Function and the
// values for the Decision Variables
match result with
| Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
| Optimal solution ->
    printfn "Objective Value: %f" solution.ObjectiveResult

    let roasterValues = Solution.getValues solution buildRoaster.AsMap
    let warehouseValues = Solution.getValues solution buildWarehouse.AsMap

    printfn "Plan Cost: $%.2f" solution.ObjectiveResult

    printfn "Location\tRoaster\tWarehouse"

    for location in locations do
        let (Location l) = location
        printfn "%-12s\t%12A\t%12A" l roasterValues.[location] warehouseValues.[location]
```