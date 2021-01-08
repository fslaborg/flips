namespace Flips.Tests

module Gens =

    open Flips
    open FsCheck

    type SmallFloat = SmallFloat of float

    let MIN_FLOAT = -1e18
    let MAX_FLOAT = 1e18
    let MIN_DISTANCE_FROM_ZERO = 1e-9

    let FloatGen = Arb.generate<float>.Where(fun x -> x > MIN_FLOAT && x < MAX_FLOAT && (x > (MIN_DISTANCE_FROM_ZERO) || x < (-1.0 * MIN_DISTANCE_FROM_ZERO)))

    let PositiveFloatGen = Arb.generate<float>.Where(fun x -> x > 0.0 && x < MAX_FLOAT)

    let SmallFloatGen =
        gen {
            let! f = FloatGen
            return SmallFloat f
        }

    let ScalarGen =
        gen {
            let! f = FloatGen
            return Value f
        }

    let IntegerBoundsGen =
        gen {
            let! lb = FloatGen
            let! d = PositiveFloatGen
            let lowerBound = lb
            let delta = d
            let upperBound = lowerBound + delta
            return Integer (lowerBound, upperBound)
        }
    
    let ContinuousBoundsGen =
        gen {
            let! lb = FloatGen
            let! d = PositiveFloatGen
            let lowerBound = lb
            let delta = d
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
            let! (SmallFloat offsetValue) = SmallFloatGen
            let! offset = Gen.elements [None; Some offsetValue]
            let! decisionElements = DecisionExpressionGen.ListOf 100
            let decisionExpr = decisionElements |> Seq.sum
            let finalExpr = match offset with | Some x -> decisionExpr + x | None -> decisionExpr
            return finalExpr
        }

    let RelationshipGen = 
        gen {
            return! Gen.elements [Equal; LessOrEqual; GreaterOrEqual]
        }

    let ConstraintExpressionGen = 
        gen {
            let! lhs = LinearExpressionGen
            let! rhs = LinearExpressionGen
            let! relationship = RelationshipGen
            return! Gen.elements [ConstraintExpression (lhs, relationship, rhs)]
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
                ConstraintExpression = constraintExpression
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

    type Types () =
        static member ArbSmallFloatGen () = Arb.fromGen SmallFloatGen
        static member ArbScalarGen () = Arb.fromGen ScalarGen
        static member ArbDecisionTypeGen () = Arb.fromGen DecisionTypeGen
        static member ArbDecisionNameGen () = Arb.fromGen DecisionNameGen
        static member ArbDecision () = Arb.fromGen DecisionGen
        static member ArbLinearExpression () = Arb.fromGen LinearExpressionGen
        static member ArbRelationship () = Arb.fromGen RelationshipGen
        static member ArbConstraintName () = Arb.fromGen ConstraintNameGen
        static member ArbConstraintExpression () = Arb.fromGen ConstraintExpressionGen
        static member ArbConstraint () = Arb.fromGen ConstraintGen
        static member ArbObjectiveSense () = Arb.fromGen ObjectiveSenseGen
        static member ArbObjectiveName () = Arb.fromGen ObjectiveNameGen
        static member ArbObjective () = Arb.fromGen ObjectiveGen
        static member ArbModel () = Arb.fromGen ModelGen

    module UnitsOfMeasure =
        
        open Flips.UnitsOfMeasure
        open Flips.UnitsOfMeasure.Types

        type [<Measure>] Item
        type SmallFloatItem = SmallFloatItem of float<Item>

        let MIN_FLOAT_ITEM = MIN_FLOAT * 1.0<Item>
        let MAX_FLOAT_ITEM = MAX_FLOAT * 1.0<Item>
        let MIN_DISTANCE_FROM_ZERO_ITEM = MIN_DISTANCE_FROM_ZERO * 1.0<Item>

        let FloatItemGen = Arb.generate<float<Item>>.Where(fun x -> x > MIN_FLOAT_ITEM && x < MAX_FLOAT_ITEM && (x > (MIN_DISTANCE_FROM_ZERO_ITEM) || x < (-1.0 * MIN_DISTANCE_FROM_ZERO_ITEM)))

        let PositiveFloatItemGen = Arb.generate<float<Item>>.Where(fun x -> x > 0.0<Item> && x < MAX_FLOAT_ITEM)

        let SmallFloatItemGen =
            gen {
                let! f = FloatItemGen
                return SmallFloatItem f
            }   

        let DecisionItemGen = 
            gen {
                let! name = DecisionNameGen
                let! decisionType = DecisionTypeGen
                let d = {
                    Name = name
                    Type = decisionType
                }
                return Decision<Item>.Value d
            }

        let DecisionItemExpressionGen =
            gen {
                let! coefficient = Arb.generate<float>
                let! decision = Arb.generate<Decision<Item>>
                return coefficient * decision
            }

        let LinearExpressionItemGen =
            gen {
                let! offsetValue = Arb.generate<float>
                let! offset = Gen.elements [None; Some offsetValue]
                let! decisionElements = DecisionExpressionGen.ListOf 100
                let decisionExpr = decisionElements |> Seq.sum
                let finalExpr = match offset with | Some x -> decisionExpr + x | None -> decisionExpr
                return LinearExpression<Item>.Value finalExpr
            }

        type UnitOfMeasureTypes =
            static member ArbSmallFloatItem () = Arb.fromGen SmallFloatItemGen
            static member ArbDecisionItem () = Arb.fromGen DecisionItemGen
            static member ArbLinearExpressionItem () = Arb.fromGen LinearExpressionItemGen
            