namespace Flips.Solver

open System
open System.Collections.Generic
open Flips

/// Comparer used for the reduction of LinearExpression due to float addition
type internal SignInsenstiveComparer () =
    interface IComparer<float> with 
        member this.Compare (a: float, b: float) =
            Math.Abs(a).CompareTo(Math.Abs(b))

type ISolution = 
    abstract member Values : IReadOnlyDictionary<IDecision, float>

type ISolver<'TError> =
    abstract member Solve : model:Model -> Result<ISolution, 'TError>
