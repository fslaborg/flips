module Gens

open Flips.Domain
open FsCheck

let IntegerBoundsGen () =
    gen {
        let! lb = Arb.generate<int>
        let! PositiveInt d = Arb.generate<PositiveInt>
        let lowerBound = int64 lb
        let delta = int64 d
        let upperBound = lowerBound + delta
        return Integer (lowerBound, upperBound)
    }
    
let ContinuousBoundsGen () =
    gen {
        let! lowerBound = Arb.generate<decimal>
        let! PositiveInt d = Arb.generate<PositiveInt>
        let delta = decimal d
        let upperBound = lowerBound + delta
        return Continuous (lowerBound, upperBound)
    }

let DecisionTypeGen () =
    gen {
        let! integerBounds = IntegerBoundsGen ()
        let! continuousBounds = ContinuousBoundsGen ()
        return! Gen.elements [integerBounds; continuousBounds; Boolean]
    }

let DecisionNameGen () =
    gen {
        let! (NonEmptyString name) = Arb.generate<NonEmptyString>
        return DecisionName name
    }

let DecisionGen () = 
    gen {
        let! name = DecisionNameGen ()
        let! decisionType = DecisionTypeGen ()
        let d = {
            Name = name
            Type = decisionType
        }
        return d
    }

type Domain () =
    static member ArbDecision () = Arb.fromGen (DecisionGen ())