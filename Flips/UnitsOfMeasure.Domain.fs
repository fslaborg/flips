namespace Flips.UnitsOfMeasure

open Flips
open Flips.UnitsOfMeasure.Types

[<RequireQualifiedAccess>]
module Decision =

    let create<[<Measure>] 'Measure> decisionName decisionType =
        let d = Decision.create decisionName decisionType
        Decision<'Measure>.Value d

    let createBoolean<[<Measure>] 'Measure> decisionName =
        let d = Decision.createBoolean decisionName
        Decision<'Measure>.Value d

    let createInteger<[<Measure>] 'Measure> decisionName (lowerBound:float<'Measure>) (upperBound:float<'Measure>) =
        let d = Decision.createInteger decisionName (float lowerBound) (float upperBound)
        Decision<'Measure>.Value d

    let createContinuous<[<Measure>] 'Measure> decisionName (lowerBound:float<'Measure>) (upperBound:float<'Measure>) =
        let d = Decision.createContinuous decisionName (float lowerBound) (float upperBound)
        Decision<'Measure>.Value d


[<RequireQualifiedAccess>]
module Objective =
    
    let create objectiveName sense (Value expr:LinearExpression<'Measure>) =
        Objective.create objectiveName sense expr


[<RequireQualifiedAccess>]
module Solution =

    let getValues (s:Types.Solution) (m:Map<_,Decision<'Measure>>) =
        let getWithDefault _ (Decision.Value d:Decision<'Measure>) =
            match Map.tryFind d s.DecisionResults with
            | Some v -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> v
            | None -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0

        m |> Map.map getWithDefault