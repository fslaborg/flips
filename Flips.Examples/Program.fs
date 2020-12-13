// Learn more about F# at http://fsharp.org

open System
open Flips
open Flips.Types
open Flips.Examples
 

[<EntryPoint>]
let main argv =

#if USE_FLIPS_SOLVER_LEGACY
    // Legacy Examples
    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = Settings.basic
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
#endif

#if USE_FLIPS_SOLVER_CBC
    // CBC Solver Examples
    Flips.Examples.CBC.CoffeeRoastingExample.solve ()
#endif
#if USE_FLIPS_SOLVER_CPLEX
    Flips.Examples.Cplex.Samples.runFoodTruckExample ()
    Flips.Examples.Cplex.Samples.runFoodTruckMapExample ()
    Flips.Examples.Cplex.Samples.runFoodTruckConstraintBuilderExample ()
    Flips.Examples.Cplex.Samples.runFoodTruckDecisionBuilder ()
    Flips.Examples.Cplex.Samples.runFoodTruckUnitsOfMeasureExample ()
    Flips.Examples.Cplex.Samples.runMultipleFoodTruckExample ()
    Flips.Examples.Cplex.Samples.runMultipleFoodTruckWithSliceMapExample ()
    Flips.Examples.Cplex.Samples.runMultipleFoodTruckSliceMapWithUnitsOfMeasureExample ()
    Flips.Examples.Cplex.Samples.runSimpleExample()
    Flips.Examples.Cplex.Samples.runMapSlicingExample ()
    Flips.Examples.Cplex.Samples.runBinaryProgrammingExample ()
    Flips.Examples.Cplex.Samples.runStocksExample()
    Flips.Examples.Cplex.Samples.runCoffeeRoastingModel ()
    Flips.Examples.Cplex.Samples.runMultiobjectiveModel ()
    
#endif
    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
