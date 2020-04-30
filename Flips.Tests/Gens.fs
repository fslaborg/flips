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

let DecisionExpressionGen =
    gen {
        let! coefficient = Arb.generate<float>
        let! decision = Arb.generate<Decision>
        return coefficient * decision
    }

let LinearExpressionGen =
    gen {
        let! offsetValue = Arb.generate<float>
        let! offset = Gen.elements [None; Some offsetValue]
        let! decisionElements = DecisionExpressionGen.ListOf 100
        let decisionExpr = decisionElements |> Seq.sum
        let finalExpr = match offset with | Some x -> decisionExpr + x | None -> decisionExpr
        return finalExpr
    }

let InequalityGen = 
    gen {
        return! Gen.elements [LessOrEqual; GreaterOrEqual]
    }

let ConstraintExpressionGen = 
    gen {
        let! lhs = LinearExpressionGen
        let! rhs = LinearExpressionGen
        let! inequality = InequalityGen
        return! Gen.elements [Inequality (lhs, inequality, rhs); Equality (lhs, rhs)]
    }

let ConstraintNameGen = 
    gen {
        let! NonEmptyString name = Arb.generate<NonEmptyString>
        return ConstraintName name
    }

let ConstraintGen =
    gen {
        let! name = ConstraintNameGen
        let! constraintExpression = ConstraintExpressionGen
        let cnst = {
            Name = name
            Expression = constraintExpression
        }
        return cnst
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
        let! expr = LinearExpressionGen
        let objective = {
            Name = name
            Sense = sense
            Expression = expr
        }
        return objective
    }

let ModelGen =
    gen {
        let! objective = ObjectiveGen
        let! constraints = ConstraintGen.ListOf 100
        let model =
            Model.create objective
            |> Model.addConstraints constraints
        return model
    }

type Domain () =
    static member ArbDecisionTypeGen () = Arb.fromGen DecisionTypeGen
    static member ArbDecisionNameGen () = Arb.fromGen DecisionNameGen
    static member ArbDecision () = Arb.fromGen DecisionGen
    static member ArbLinearExpression () = Arb.fromGen LinearExpressionGen
    static member ArbInequality () = Arb.fromGen InequalityGen
    static member ArbConstraintName () = Arb.fromGen ConstraintNameGen
    static member ArbConstraintExpression () = Arb.fromGen ConstraintExpressionGen
    static member ArbConstraint () = Arb.fromGen ConstraintGen
    static member ArbObjectiveSense () = Arb.fromGen ObjectiveSenseGen
    static member ArbObjectiveName () = Arb.fromGen ObjectiveNameGen
    static member ArbObjective () = Arb.fromGen ObjectiveGen
    static member ArbModel () = Arb.fromGen ModelGen