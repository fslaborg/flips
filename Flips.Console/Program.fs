// Learn more about F# at http://fsharp.org

open System

open Flips
open Flips.Types

[<EntryPoint>]
let main argv =


    let rhs = Vector.Dense [|3.0; 3.0; 4.0|]
    let A = Matrix.Dense (array2D [[5.0; 1.0; 2.0]; [3.0; 1.0; 4.0]; [2.0; 6.0; 2.0]])
    let plu = LinearAlgebra.factorize A
    let x = LinearAlgebra.solve plu rhs

    printfn "%A" plu
    printfn "%A" x

    printfn "Press any key to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code
