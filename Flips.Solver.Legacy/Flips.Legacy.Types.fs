namespace Flips.Legacy

open System
open System.Collections.Generic
open Flips.Types
open Flips.Solver

[<AutoOpen>]
module Types =

    //[<NoComparison>]
    //type internal ReduceAccumulator = {
    //    DecisionTypes : Dictionary<DecisionName, DecisionType>
    //    Coefficients : Dictionary<DecisionName, List<float>>
    //    Offsets : List<float>
    //}

    [<NoComparison; CustomEquality>] 
    type internal ReducedLinearExpression =
        {
            DecisionTypes : Dictionary<DecisionName, DecisionType>
            Coefficients : Dictionary<DecisionName, float>
            Offset : float
        } with
        static member private NearlyEquals (a:float) (b:float) : bool =
            let aValue = System.BitConverter.DoubleToInt64Bits a
            let bValue = System.BitConverter.DoubleToInt64Bits b
            if (aValue >>> 63) <> (bValue >>> 63) then
                a = b
            else
                System.Math.Abs(aValue - bValue) <= 10_000L

        override this.GetHashCode () =
                hash this

        override this.Equals(obj) =
            match obj with
            | :? ReducedLinearExpression as otherExpr ->
            let offsetSame = ReducedLinearExpression.NearlyEquals this.Offset otherExpr.Offset

            let asMap (d:Dictionary<_,_>) =
              d
              |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
              |> Map.ofSeq

            let thisCoefficients = asMap this.Coefficients
            let otherCoefficients = asMap otherExpr.Coefficients

            let leftMatchesRight =
                (true, thisCoefficients)
                ||> Map.fold (fun b k thisCoef -> 
                                match Map.tryFind k otherCoefficients with
                                | Some otherCoef -> b && (ReducedLinearExpression.NearlyEquals thisCoef otherCoef)
                                | None -> b && (ReducedLinearExpression.NearlyEquals thisCoef 0.0))

            let evaluateRightElement b n otherCoef =
                if this.Coefficients.ContainsKey(n) then
                    b
                else
                    let essentiallyZero = ReducedLinearExpression.NearlyEquals otherCoef 0.0
                    b && essentiallyZero

            let rightNonMatchesAreZero =
                (true, otherCoefficients)
                ||> Map.fold evaluateRightElement

            let allPassing = offsetSame && leftMatchesRight && rightNonMatchesAreZero
            allPassing
                    | _ -> false

        //static member internal OfReduceAccumulator (acc:ReduceAccumulator) =
        //    let offset = acc.Offsets |> Seq.sortBy Math.Abs |> Seq.sum

        //    let coefficients = Dictionary()
        //    for elem in acc.Coefficients do
        //        elem.Value.Sort(SignInsenstiveComparer())
        //        let coefficient = Seq.sum elem.Value
        //        coefficients.Add(elem.Key, coefficient)

        //    {
        //        DecisionTypes = acc.DecisionTypes
        //        Coefficients = coefficients
        //        Offset = offset
        //    }

    /// The type of underlying solver to use
    type SolverType = 
        | CBC
        | GLOP
        | Cplex128
        | Gurobi900

    /// Parameters for the solver
    type SolverSettings = {
        SolverType : SolverType
        MaxDuration : int64
        WriteLPFile : Option<string>
        WriteMPSFile : Option<string>
        // We want to enable this in the next major release
        //EnableOutput : bool
    }


    /// The results of the optimization if it was successful
    type Solution = {
        DecisionResults : Map<Decision,float>
        [<Obsolete("Please use the Objective.evaluate function instead")>]
        ObjectiveResult : float
    } with
        interface ISolution with
            member this.Values = 
                this.DecisionResults
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> k :> IDecision, v)
                |> readOnlyDict
            

    /// The result of calling the solve function. If the solve was successful, the Optimal
    /// case will hold a Solution type. If it was not successful, the Supoptimal case will
    /// be returned with a string reporting what the solver returned.
    type SolveResult =
        | Optimal of Solution
        | Infeasible of string
        | Unbounded of string
        | Unknown of string

#if USE_LEGACY_NAMESPACE
namespace Flips.Types


open System
[<AutoOpen>]
module Obsolete =
    [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
    type SolverType = Flips.Legacy.Types.SolverType
    [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
    type SolverSettings = Flips.Legacy.Types.SolverSettings
    [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
    type Solution = Flips.Legacy.Types.Solution
    [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
    type SolveResult = Flips.Legacy.Types.SolveResult
#endif
