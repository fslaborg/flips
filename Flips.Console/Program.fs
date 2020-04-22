// Learn more about F# at http://fsharp.org

open System
open Flips.Domain
open Flips.Solve
open Flips.Extensions


let simpleModel () =
    let x1 = Decision.createContinuous "x1" 0.0M Decimal.MaxValue
    let x2 = Decision.createContinuous "x2" 0M Decimal.MaxValue
    
    let objExpr = 2.0 * x1 + 3.0 * x2
    let objective = Objective.create "Get big" Maximize objExpr
    let model = 
        Model.create objective
        |> Model.addConstraint (Constraint.create "Max x1" (x1 <== 10.0))
        |> Model.addConstraint (Constraint.create "Max x2" (x2 <== 5.0))
        |> Model.addConstraint (Constraint.create "Max x1 and x2" (x1 + x2 <== 12.0))
    
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 30_000L
        WriteLPFile = Some "Test.lp"
    }
    
    let result = solve settings model
    printfn "%A" result


let constraintBuilderExample () =
    let sources = [1 .. 3]
    let sourceMax = Map.ofList [for s in sources -> s, 10.0 * float s]

    let destinations = ["a"; "b"; "c"]
    let destinationMax = Map.ofList ["a", 12.0; "b", 14.0; "c", 9.0]

    let arcValues = Map.ofList [
        (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
        (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
        (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
    ]

    let decisions = 
        [for s in sources do
            for d in destinations ->
                (s, d), 1.0 * Decision.createBoolean (sprintf "%i_%s" s d)]
        |> Map.ofList

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let sourceConstraints = ConstraintBuilder "SourceMax" {
        for source in sources ->
            let sourceDecs = decisions |> Map.filter (fun (s, d) v -> s = source)
            sum (sourceDecs) <== sourceMax.[source]
    }

    // Using a ConstraintBuilder ComputationExpression to generate a set of constraints
    // with sensible names
    let destinationConstraints = ConstraintBuilder "DestinationMax" {
        for dest in destinations ->
            let destDecs = decisions |> Map.filter (fun (s, d) v -> d = dest)
            sum (destDecs) <== destinationMax.[dest]
    }

    // Use combination of the `sum` function and the `.*` operator to combine two Maps
    let objExpr = sum (arcValues .* decisions)
    let objective = Objective.create "Max flow" Maximize objExpr

    let model =
        Model.create objective
        |> Model.addConstraints sourceConstraints
        |> Model.addConstraints destinationConstraints

    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 30_000L
        WriteLPFile = Some "ConstraintBuilderExample.lp"
    }

    let result = solve settings model

    printfn "--Result--"
    printfn "%A" result

[<EntryPoint>]
let main argv =
    
    //simpleModel ()
    constraintBuilderExample ()


    printfn "Press any key to close..."
    Console.ReadKey () |> ignore
    0 // return an integer exit code
