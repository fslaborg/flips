// Learn more about F# at http://fsharp.org

open System
open Flips.Examples
 

[<EntryPoint>]
let main argv =
    
    FoodTruckExample.solve ()
    FoodTruckMapExample.solve ()
    FoodTruckConstraintBuilderExample.solve ()
    FoodTruckUnitsOfMeasureExample.solve ()
    MultipleFoodTruckExample.solve ()
    MultipleFoodTruckWithSliceMapExample.solve ()
    MultipleFoodTruckSliceMapWithUnitsOfMeasureExample.solve ()
    SimpleExample.solve ()
    MapSlicingExample.solve ()
    BinaryProgrammingExample.solve ()
    StocksExample.solve ()
    CoffeeRoastingExample.solve ()
    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
