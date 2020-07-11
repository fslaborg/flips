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