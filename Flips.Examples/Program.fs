// Learn more about F# at http://fsharp.org

open System
open Flips.Types
open Flips.Examples
 

[<EntryPoint>]
let main argv =
    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
        WriteMPSFile = None
    }

    FoodTruckExample.solve { settings with WriteLPFile = Some "foodtruck.lp" }
    FoodTruckMapExample.solve { settings with WriteLPFile = Some "foodmaptruck.lp" }
    FoodTruckConstraintBuilderExample.solve { settings with WriteLPFile = Some "foodtruckconstraintbuilder.lp" }
    FoodTruckDecisionBuilder.solve { settings with WriteLPFile = Some "foodtruckdecisionbuilder.lp" }
    FoodTruckUnitsOfMeasureExample.solve { settings with WriteLPFile = Some "foodtruckuom.lp" }
    MultipleFoodTruckExample.solve { settings with WriteLPFile = Some "multiplefoodtruck.lp" }
    MultipleFoodTruckWithSliceMapExample.solve { settings with WriteLPFile = Some "multiplefoodtruckslicemap.lp" }
    MultipleFoodTruckSliceMapWithUnitsOfMeasureExample.solve  { settings with WriteLPFile = Some "multiplefoodtruckslicemapuom.lp" }
    SimpleExample.solve { settings with WriteLPFile = Some "simpleproblem.lp" }
    MapSlicingExample.solve { settings with WriteLPFile = Some "ConstraintBuilder.lp" }
    BinaryProgrammingExample.solve { settings with WriteLPFile = Some "ConstraintBuilderWithBinary.lp" }
    StocksExample.solve  { settings with WriteLPFile = Some "stocks.lp" }
    CoffeeRoastingExample.solve  { settings with WriteLPFile = Some "coffeeroasting.lp" }
    MultiObjective.solve { settings with WriteMPSFile = Some "multiobjective.mps" }
    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
