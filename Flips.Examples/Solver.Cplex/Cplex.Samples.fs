module Flips.Examples.Cplex.Samples

open ILOG.CPLEX
type ISolution = Flips.Solver.ISolution

type Objective<[<Measure>]'a> = Flips.UnitsOfMeasure.Types.Objective<'a>

open Flips.Solver.Cplex.Internals.CplexSolver
open Flips.Solver.Cplex.Internals.CplexSolver.Primitives
open Flips.Examples
open Flips.Solver.Cplex.Internals

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
    let printName n = printfn $"==== cplex callback {n}"
    let callbacks =
      Callbacks.CallbacksOption.empty
      |> Callbacks.CallbacksOption.withBarrier             { new Cplex.BarrierCallback            () with member x.Main () = printName ((nameof Cplex.BarrierCallback            )) }
      |> Callbacks.CallbacksOption.withBranch              { new Cplex.BranchCallback             () with member x.Main () = printName ((nameof Cplex.BranchCallback             )) }
      |> Callbacks.CallbacksOption.withContinuous          { new Cplex.ContinuousCallback         () with member x.Main () = printName ((nameof Cplex.ContinuousCallback         )) }
      |> Callbacks.CallbacksOption.withControl             { new Cplex.ControlCallback            () with member x.Main () = printName ((nameof Cplex.ControlCallback            )) }
      |> Callbacks.CallbacksOption.withCrossover           { new Cplex.CrossoverCallback          () with member x.Main () = printName ((nameof Cplex.CrossoverCallback          )) }
      |> Callbacks.CallbacksOption.withDisjunctiveCut      { new Cplex.DisjunctiveCutCallback     () with member x.Main () = printName ((nameof Cplex.DisjunctiveCutCallback     )) }
      |> Callbacks.CallbacksOption.withDisjunctiveCutInfo  { new Cplex.DisjunctiveCutInfoCallback () with member x.Main () = printName ((nameof Cplex.DisjunctiveCutInfoCallback )) }
      |> Callbacks.CallbacksOption.withFlowMIRCut          { new Cplex.FlowMIRCutCallback         () with member x.Main () = printName ((nameof Cplex.FlowMIRCutCallback         )) }
      |> Callbacks.CallbacksOption.withFlowMIRCutInfo      { new Cplex.FlowMIRCutInfoCallback     () with member x.Main () = printName ((nameof Cplex.FlowMIRCutInfoCallback     )) }
      |> Callbacks.CallbacksOption.withFractionalCut       { new Cplex.FractionalCutCallback      () with member x.Main () = printName ((nameof Cplex.FractionalCutCallback      )) }
      |> Callbacks.CallbacksOption.withFractionalCutInfo   { new Cplex.FractionalCutInfoCallback  () with member x.Main () = printName ((nameof Cplex.FractionalCutInfoCallback  )) }
      |> Callbacks.CallbacksOption.withHeuristic           { new Cplex.HeuristicCallback          () with member x.Main () = printName ((nameof Cplex.HeuristicCallback          )) }
      |> Callbacks.CallbacksOption.withIncumbent           { new Cplex.IncumbentCallback          () with member x.Main () = printName ((nameof Cplex.IncumbentCallback          )) }
      |> Callbacks.CallbacksOption.withLazyConstraint      { new Cplex.LazyConstraintCallback     () with member x.Main () = printName ((nameof Cplex.LazyConstraintCallback     )) }
      |> Callbacks.CallbacksOption.withMip                 { new Cplex.MIPCallback                () with member x.Main () = printName ((nameof Cplex.MIPCallback                )) }
      |> Callbacks.CallbacksOption.withMipInfo             { new Cplex.MIPInfoCallback            () with member x.Main () = printName ((nameof Cplex.MIPInfoCallback            )) }
      |> Callbacks.CallbacksOption.withNetwork             { new Cplex.NetworkCallback            () with member x.Main () = printName ((nameof Cplex.NetworkCallback            )) }
      |> Callbacks.CallbacksOption.withNode                { new Cplex.NodeCallback               () with member x.Main () = printName ((nameof Cplex.NodeCallback               )) }
      |> Callbacks.CallbacksOption.withPresolve            { new Cplex.PresolveCallback           () with member x.Main () = printName ((nameof Cplex.PresolveCallback           )) }
      |> Callbacks.CallbacksOption.withProbing             { new Cplex.ProbingCallback            () with member x.Main () = printName ((nameof Cplex.ProbingCallback            )) }
      |> Callbacks.CallbacksOption.withProbingInfo         { new Cplex.ProbingInfoCallback        () with member x.Main () = printName ((nameof Cplex.ProbingInfoCallback        )) }
      |> Callbacks.CallbacksOption.withSimplex             { new Cplex.SimplexCallback            () with member x.Main () = printName ((nameof Cplex.SimplexCallback            )) }
      |> Callbacks.CallbacksOption.withSolve               { new Cplex.SolveCallback              () with member x.Main () = printName ((nameof Cplex.SolveCallback              )) }
      |> Callbacks.CallbacksOption.withTuning              { new Cplex.TuningCallback             () with member x.Main () = printName ((nameof Cplex.TuningCallback             )) }
      |> Callbacks.CallbacksOption.withUserCut             { new Cplex.UserCutCallback            () with member x.Main () = printName ((nameof Cplex.UserCutCallback            )) }

    runSolverWithCallbacks model state callbacks.AsCallbacks

open Flips.Types.TypeExtensions



let inline runModelEvalObj sampleName (model, objective) evalObj printObj =
    let prefixPrint t = printfn $"========== Flips Samples {sampleName} - %s{t}"
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
    if result.Solved then
        let solution = result :> ISolution
        prefixPrint $"Objective Value: %f{(evalObj solution objective)}"
        
        for KeyValue(decision, value) in solution.Values do
            printfn "Decision: %50s\tValue: %f" decision.Name.AsString value
        
        prefixPrint "Problem Definition"
        printfn "Objective:"
        printObj solution
        
        printfn "Constraints:"
        for c in model.Constraints do
          printfn $"{Printer.print c}"
        
        use writer = new System.IO.StreamWriter(solutionFile)
        writer.WriteLine "Name\tValue"
        for KeyValue(decision,value) in solution.Values do
            let (Flips.Types.DecisionName name) = decision.Name
            writer.WriteLine($"{name}\t{value}")
    else
        printfn "Unable to solve."

let runModelMeasureAndPrintObjectiveThenDecisions sampleName (model, objective: Flips.UnitsOfMeasure.Types.Objective<'m>) =
    runModelEvalObj 
        sampleName 
        (model, objective) 
        Flips.UnitsOfMeasure.Types.Objective<'m>.evaluate 
        (fun sln -> printfn $"{Printer.print objective.Objective}")
    
let runModelAndPrintObjectiveThenDecisions sampleName (model, objective) =
    runModelEvalObj
        sampleName 
        (model, objective) 
        Flips.Types.Objective.evaluate 
        (fun sln -> printfn $"{Printer.print objective}")
    
let runFoodTruckExample                     () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckExample                    ) SampleProblems.FoodTruckExample.model
let runFoodTruckMapExample                  () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckMapExample                 ) SampleProblems.FoodTruckMapExample.model
let runBinaryProgrammingExample             () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.BinaryProgrammingExample            ) SampleProblems.BinaryProgrammingExample.model
let runFoodTruckConstraintBuilderExample    () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckConstraintBuilderExample   ) SampleProblems.FoodTruckConstraintBuilderExample.model
let runFoodTruckDecisionBuilder             () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.FoodTruckDecisionBuilder            ) SampleProblems.FoodTruckDecisionBuilder.model
let runMapSlicingExample                    () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.MapSlicingExample                   ) SampleProblems.MapSlicingExample.model
let runStocksExample                        () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.StocksExample                       ) SampleProblems.StocksExample.model
let runFoodTruckUnitsOfMeasureExample       () = runModelMeasureAndPrintObjectiveThenDecisions (nameof SampleProblems.FoodTruckUnitsOfMeasureExample      ) SampleProblems.FoodTruckUnitsOfMeasureExample.model
let runSimpleExample                        () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.SimpleExample                       ) (SampleProblems.SimpleExample.model,SampleProblems.SimpleExample.model.Objectives.Head)
let runMultipleFoodTruckExample             () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.MultipleFoodTruckExample            ) SampleProblems.MultipleFoodTruckExample.model
let runMultipleFoodTruckWithSliceMapExample () = runModelAndPrintObjectiveThenDecisions        (nameof SampleProblems.MultipleFoodTruckWithSliceMapExample) SampleProblems.MultipleFoodTruckWithSliceMapExample.model


let runMultipleFoodTruckSliceMapWithUnitsOfMeasureExample () = 
  let model, objective, numberOfItem= SampleProblems.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample.model
  runModelEvalObj 
      (nameof SampleProblems.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample)
      (model, objective) 
      Flips.UnitsOfMeasure.Types.Objective<SampleProblems.MultipleFoodTruckSliceMapWithUnitsOfMeasureExample.USD>.evaluate 
      (fun solution -> 
          printfn $"{Printer.print objective.Objective}"
          let values = Solution.getValues solution numberOfItem
          for ((location, item), value) in values |> Map.toSeq do
              printfn "Item: %s\tLocation: %s\tValue: %f" item location value
      )

let runMultiobjectiveModel () =
    let model, revenueObjective, wasteObjective = SampleProblems.MultiObjectiveProblem.model
    
    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    printfn "-- Result --"
    let result = runModel model []
    if result.Solved then
        let solution = result :> ISolution
        
        printfn "Revenue: %f" (Flips.Types.Objective.evaluate solution revenueObjective)
        printfn "Waste: %f" (Flips.Types.Objective.evaluate solution wasteObjective)

        for KeyValue(decision, value) in solution.Values do
            printfn "Decision: %s\tValue: %f" decision.Name.AsString value
    else
        printfn "Unable to solve."

open SampleProblems.CoffeeRoastingExample

let runCoffeeRoastingModel () =
    let model, objective, buildRoaster, buildWarehouse, locations = SampleProblems.CoffeeRoastingExample.model
    let result = runModel model []
    if result.Solved then
        let solution = result :> ISolution
        printfn "Objective Value: %f" (Flips.UnitsOfMeasure.Types.Objective<USD>.evaluate solution objective)

        let roasterValues = Solution.getValues solution buildRoaster
        let warehouseValues = Solution.getValues solution buildWarehouse
        let result : float<USD> = Flips.UnitsOfMeasure.Types.Objective.evaluate solution objective

        printfn "Plan Cost: $%.2f" result

        printfn "Location\tRoaster\tWarehouse"

        for location in locations do
            let (SampleProblems.CoffeeRoastingExample.Location l) = location
            printfn "%-12s\t%12A\t%12A" l roasterValues.[location] warehouseValues.[location]        
    else
        printfn "Unable to solve."
    