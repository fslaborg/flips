// Learn more about F# at http://fsharp.org

open System
open Flips.Examples
 

[<EntryPoint>]
let main argv =
    
    FoodTruckExample.solve ()
    FoodTruckMapExample.solve ()
    FoodTruckConstraintBuilderExample.solve ()
    MultipleFoodTruckExample.solve ()
    MultipleFoodTruckWithSliceMapExample.solve ()
    FoodTruckUnitsOfMeasureExample.solve ()
    SimpleExample.solve ()
    MapSlicingExample.solve ()
    BinaryProgrammingExample.solve ()
    StocksExample.solve ()

    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
