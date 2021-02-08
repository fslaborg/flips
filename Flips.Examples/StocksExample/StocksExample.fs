module Flips.Examples.StocksExample

open System
open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Statistics 
open Flips
open Flips.Legacy

let solve settings =

    let model, objective = SampleProblems.StocksExample.model
    // Solve
    let results = Solver.solve settings model

    // Printing results:
    printfn "-- Result --"
    match results with
    | Optimal solution ->
        printfn "Objective Value - Risk: %f %%" ((Objective.evaluate solution objective) * 100.0)
    
        CsvExport.exportVariablesToFile "stocks" solution.DecisionResults CsvExport.csvConfig
    
        for (decision, value) in solution.DecisionResults |> Map.toSeq |> Seq.filter(fun (key, value) -> value > 0.0) do
            printfn "Decision: %s\tValue: %f %%" decision.Name (value * 100.0)
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
    