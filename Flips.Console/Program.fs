// Learn more about F# at http://fsharp.org

open System

open Flips

[<EntryPoint>]
let main argv =
    printfn "Press any key to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code
