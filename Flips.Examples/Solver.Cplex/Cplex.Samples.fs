module Flips.Examples.Cplex.Samples

open ILOG.CPLEX
type ISolution = Flips.Solver.ISolution

type Objective<[<Measure>]'a> = Flips.UnitsOfMeasure.Types.Objective<'a>

open Flips.Solver.Cplex.Internals.CplexSolver
open Flips.Solver.Cplex.Internals.CplexSolver.Primitives
open Flips.Examples
open Flips.Solver.Cplex.Internals

type Flips.Types.Objective with
    static member evaluate (solution: ISolution) (objective: Flips.Types.Objective) =
        objective.Expression
        |> Flips.Types.LinearExpression.Evaluate solution.Values

type Flips.UnitsOfMeasure.Types.Objective<[<Measure>]'Measure> with
    static member evaluate (solution: ISolution) (Objective.Value objective: Objective<'Measure>) =
        objective.Expression
        |> Flips.Types.LinearExpression.Evaluate solution.Values
        |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>

module Solution =
    
    open Flips.UnitsOfMeasure

    let getValues (solution: ISolution) (decisions: System.Collections.Generic.IDictionary<_,Decision<'Measure>>) =
        let getWithDefault (Decision.Value d: Decision<'Measure>) =
            match solution.Values.TryGetValue d with
            | true, v -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> v
            | false, _ -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0

        seq { for kvp in decisions -> kvp.Key, getWithDefault kvp.Value}
        |> Map

let runModel model (options:ICPlexOption seq) =
    let cplex = new Cplex()
    let state = State.ofCplex cplex
    let state = options |> Seq.fold (fun state o -> ICplexSolverState.setOption o state) state
    
    runSolverWithCallbacks model state null

let inline runModelEvalObj sampleName (model, objective) evalObj =
    printfn $"-- sample: {sampleName} --"
    let lpFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__ ,@"..", "output", "cplex", $"{sampleName}.lp")
    let solutionFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__ ,@"..", "output", "cplex", $"{sampleName}.solution.tsv")
    let options = [(CplexSolver.WriteToFile (lpFile)) :> ICPlexOption]
    let result = runModel model options
    printfn "-- Result --"
    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Ok (solution,formulation) ->
        printfn "Objective Value: %f" (evalObj solution objective)
        for KeyValue(decision, value) in solution.Values do
            printfn "Decision: %A\tValue: %f" decision.Name value
        
        use writer = new System.IO.StreamWriter(solutionFile)
        writer.WriteLine "Name\tValue"
        for KeyValue(decision,value) in solution.Values do
            let (Flips.Types.DecisionName name) = decision.Name
            writer.WriteLine($"{name}\t{value}")
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase

let runModelMeasureAndPrintObjectiveThenDecisions sampleName (model, objective: Flips.UnitsOfMeasure.Types.Objective<'m>) =
    runModelEvalObj sampleName (model, objective) Flips.UnitsOfMeasure.Types.Objective<'m>.evaluate
    
let runModelAndPrintObjectiveThenDecisions sampleName (model, objective) =
    runModelEvalObj sampleName (model, objective) Flips.Types.Objective.evaluate
    
let runFoodTruckExample                  () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckExample)                  SampleProblems.FoodTruckExample.model
let runFoodTruckMapExample               () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckMapExample)               SampleProblems.FoodTruckMapExample.model
let runBinaryProgrammingExample          () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.BinaryProgrammingExample)          SampleProblems.BinaryProgrammingExample.model
let runFoodTruckConstraintBuilderExample () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckConstraintBuilderExample) SampleProblems.FoodTruckConstraintBuilderExample.model
let runFoodTruckDecisionBuilder          () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckDecisionBuilder)          SampleProblems.FoodTruckDecisionBuilder.model
let runMapSlicingExample                 () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.MapSlicingExample)                 SampleProblems.MapSlicingExample.model
let runFoodTruckUnitsOfMeasureExample    () = runModelMeasureAndPrintObjectiveThenDecisions (nameof SampleProblems.FoodTruckUnitsOfMeasureExample)    SampleProblems.FoodTruckUnitsOfMeasureExample.model

let runMultiobjectiveModel () =
    let model, revenueObjective, wasteObjective = SampleProblems.MultiObjectiveProblem.model
    
    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    printfn "-- Result --"
    match runModel model [] with
    | Ok (solution,formulation) ->

        printfn "Revenue: %f" (Flips.Types.Objective.evaluate solution revenueObjective)
        printfn "Waste: %f" (Flips.Types.Objective.evaluate solution wasteObjective)

        for KeyValue(decision, value) in solution.Values do
            printfn "Decision: %A\tValue: %f" decision.Name value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase

open SampleProblems.CoffeeRoastingExample
let runCoffeeRoastingModel () =
    let model, objective, buildRoaster, buildWarehouse, locations = SampleProblems.CoffeeRoastingExample.model
    match runModel model [] with
    | Ok (solution,formulation) ->
        
        printfn "Objective Value: %f" (Flips.UnitsOfMeasure.Types.Objective<USD>.evaluate solution objective)

        let roasterValues = Solution.getValues solution buildRoaster
        let warehouseValues = Solution.getValues solution buildWarehouse
        let result : float<USD> = Flips.UnitsOfMeasure.Types.Objective.evaluate solution objective

        printfn "Plan Cost: $%.2f" result

        printfn "Location\tRoaster\tWarehouse"

        for location in locations do
            let (SampleProblems.CoffeeRoastingExample.Location l) = location
            printfn "%-12s\t%12A\t%12A" l roasterValues.[location] warehouseValues.[location]        
    | errorCase ->
        printfn "Unable to solve. Error: %A" errorCase
    