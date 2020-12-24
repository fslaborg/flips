namespace Flips.Solver

open Flips
open Flips.Solver


[<RequireQualifiedAccess>]
module LinearExpression =

    let evaluate (solution: #ISolution) (expr: #ILinearExpression) : float =

        let mutable acc = FSharp.Core.LanguagePrimitives.GenericZero

        for elem in expr.Terms do
            match elem with
            | Constant c -> 
                acc <- acc + c
            | LinearElement (c, d) -> 
                acc <- acc + (c * solution.Values.[d])

        acc


[<RequireQualifiedAccess>]
module Objective =

    /// <summary>A function for evaluating the resulting value of an Objective after solving</summary>
    /// <param name="solution">The solution used for looking up the results of Decisions</param>
    /// <param name="objective">The Objective to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: #ISolution) (objective: #IObjective) =
        LinearExpression.evaluate solution objective.Expression


[<RequireQualifiedAccess>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision<'Measure>> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float<'Mesure>> where the values are the recommendations from the solver</returns>
    let getValues (solution: #ISolution) (decisions: #System.Collections.Generic.IDictionary<_, #IDecision>) =
        let getWithDefault (d: #IDecision) =
            match solution.Values.TryGetValue d with
            | true, v -> v
            | false, _ -> 0.0

        seq { for KeyValue (key, value) in decisions -> key, getWithDefault value }
        |> Map