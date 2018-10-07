// Learn more about F# at http://fsharp.org

open System
open Flips.Types

[<EntryPoint>]
let main argv =


    let v = DenseVector [|1.0; 2.0; 3.0|]


    printfn "Elem 1:%A" v.[1]

    printfn "Press any key to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code
