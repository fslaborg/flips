namespace Flips.Solver

open Flips
open Flips.Solver
open System.Collections.Generic


[<RequireQualifiedAccess>]
module LinearExpression =

    let evaluate (solution: ISolution) (expr: ILinearExpression) : float =

        //let decisions = solution.Values

        //let rec evaluateNode (multiplier: float, state: ResizeArray<float>) (node: LinearExpression) cont =
        //    match node with
        //    | Empty -> cont (multiplier, state)
        //    | AddFloat (f, nodeExpr) ->
        //        state.Add(multiplier * f)
        //        let newState = (multiplier, state) 
        //        evaluateNode newState nodeExpr cont
        //    | AddDecision ((nodeCoef, nodeDecision), nodeExpr) ->
        //        state.Add(multiplier * nodeCoef * decisions.[nodeDecision])
        //        let newState = (multiplier, state)
        //        evaluateNode newState nodeExpr cont
        //    | Multiply (nodeMultiplier, nodeExpr) ->
        //        let newState = (multiplier * nodeMultiplier, state)
        //        evaluateNode newState nodeExpr cont
        //    | AddLinearExpression (lExpr, rExpr) ->
        //        evaluateNode (multiplier, state) lExpr (fun l -> evaluateNode l rExpr cont)
          

        //let (_, reduceResult) = evaluateNode (1.0, ResizeArray()) expr (fun x -> x)

        //reduceResult.Sort(SignInsenstiveComparer())
        //let total = Seq.sum reduceResult
        //total

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
    let evaluate (solution: ISolution) (objective: Objective) =
        LinearExpression.evaluate solution objective.Expression