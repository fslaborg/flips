module Flips.Gens

open Flips.Domain
open FsCheck


let IntegerBoundsGen =
    gen {
        let! lb = Arb.generate<int>
        let! PositiveInt d = Arb.generate<PositiveInt>
        let lowerBound = int64 lb
        let delta = int64 d
        let upperBound = lowerBound + delta
        return Integer (lowerBound, upperBound)
    }
    
let ContinuousBoundsGen =
    gen {
        let! lowerBound = Arb.generate<decimal>
        let! PositiveInt d = Arb.generate<PositiveInt>
        let delta = decimal d
        let upperBound = lowerBound + delta
        return Continuous (lowerBound, upperBound)
    }

let DecisionTypeGen =
    gen {
        let! integerBounds = IntegerBoundsGen
        let! continuousBounds = ContinuousBoundsGen
        return! Gen.elements [integerBounds; continuousBounds; Boolean]
    }

let DecisionNameGen =
    gen {
        let! (NonEmptyString name) = Arb.generate<NonEmptyString>
        return DecisionName name
    }

let DecisionGen = 
    gen {
        let! name = DecisionNameGen
        let! decisionType = DecisionTypeGen
        let d = {
            Name = name
            Type = decisionType
        }
        return d
    }

let InequalityGen = 
    gen {
        return! Gen.elements [LessOrEqual; GreaterOrEqual]
    }

let ConstraintNameGen = 
    gen {
        let! NonEmptyString name = Arb.generate<NonEmptyString>
        return ConstraintName name
    }

let ObjectiveSenseGen =
    gen {
        return! Gen.elements [Minimize; Maximize]
    }

let ObjectiveNameGen =
    gen {
        let! NonEmptyString name = Arb.generate<NonEmptyString>
        return ObjectiveName name
    }

let ObjectiveGen =
    gen {
        let! name = ObjectiveNameGen
        let! sense = ObjectiveSenseGen
        // Need to create gen for LinearExpression
    }

type Domain () =
    static member ArbDecisionTypeGen () = Arb.fromGen DecisionTypeGen
    static member ArbDecisionNameGen () = Arb.fromGen DecisionNameGen
    static member ArbDecision () = Arb.fromGen DecisionGen
    static member ArbInequality () = Arb.fromGen InequalityGen
    static member ArbConstraintName () = Arb.fromGen ConstraintNameGen
    static member ArbObjectiveSense () = Arb.fromGen ObjectiveSenseGen
    static member ArbObjectiveName () = Arb.fromGen ObjectiveNameGen
    static member ArbObjective () = Arb.fromGen ObjectiveGen