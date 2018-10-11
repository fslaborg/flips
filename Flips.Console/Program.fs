// Learn more about F# at http://fsharp.org

open System

open Flips
open Flips.Types

[<EntryPoint>]
let main argv =


    let rhs = Vector.Dense (DenseVector [|1.0; 2.0; 3.0|])
    let A = Matrix.Dense (DenseMatrix (array2D [[1.0; 0.0; 0.0]; [0.0; 1.0; 0.0]; [0.0; 0.0; 1.0]]))
    let lu = LinearAlgebra.factorize A
    let x = LinearAlgebra.solve lu rhs


    printfn "%O" x

    printfn "Press any key to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code
