namespace Flips.Solver.UnitsOfMeasure

open Flips
open Flips.UnitsOfMeasure.Types
open Flips.Solver

[<RequireQualifiedAccess>]
module Decision =

    /// <summary>A function for evaluating the resulting value of an Objective after solving</summary>
    /// <param name="solution">The solution used for looking up the results of Decisions</param>
    /// <param name="objective">The Objective to evaluate the resulting value for</param>
    /// <returns>A float<'Measure> which is the simplification of the LinearExpression</returns>
    let evaluate (solution: ISolution) (Objective.Value objective: Objective<'Measure>) =
        objective.Expression
        |> LinearExpression.Evaluate solution
        |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>


[<RequireQualifiedAccess>]
module Objective =

    /// <summary>A function for evaluating the resulting value of an Objective after solving</summary>
    /// <param name="solution">The solution used for looking up the results of Decisions</param>
    /// <param name="objective">The Objective to evaluate the resulting value for</param>
    /// <returns>A float<'Measure> which is the simplification of the LinearExpression</returns>
    let evaluate (solution: ISolution) (Objective.Value objective: Objective<'Measure>) =
        objective.Expression
        |> Flips.Types.LinearExpression.Evaluate solution.Values
        |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>


[<RequireQualifiedAccess>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision<'Measure>> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float<'Mesure>> where the values are the recommendations from the solver</returns>
    let getValues (solution: ISolution) (decisions: System.Collections.Generic.IDictionary<_,Decision<'Measure>>) =
        let getWithDefault (Decision.Value d: Decision<'Measure>) =
            match solution.Values.TryGetValue d with
            | true, v -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> v
            | false, _ -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0

        seq { for kvp in decisions -> kvp.Key, getWithDefault kvp.Value}
        |> Map