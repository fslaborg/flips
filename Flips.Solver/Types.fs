namespace Flips.Solver

open System
open System.Collections.Generic
open Flips


type ISolution = 
    abstract member Values : IReadOnlyDictionary<IDecision, float>
