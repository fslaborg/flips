module Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips.Domain
open Flips.Gens
open System
open Flips

let rng = System.Random()
let MIN_COEFFICIENT = -1_000_000_000.0
let MAX_COEFFICIENT = 1_000_000_000.0

let randomInRange lowerBound upperBound (rng:System.Random) =
    let range = upperBound - lowerBound
    lowerBound + (rng.NextDouble() * range)

let randomFloat (rng:System.Random) =
    randomInRange MIN_COEFFICIENT MAX_COEFFICIENT rng

let randomExpressionFromDecisions rng (decisions:seq<Decision>) =
    let decisionGen = Gen.elements decisions
    let numberOfElements = Gen.choose (1, 10) |> Gen.sample 0 1 |> Seq.exactlyOne
    let expr = 
        decisionGen 
        |> Gen.sample 0 numberOfElements 
        |> List.map (fun x -> (randomInRange MIN_COEFFICIENT MAX_COEFFICIENT rng) * x) 
        |> List.sum
    expr


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module Scalar =

    [<Property>]
    let ``Addition of Scalar is associative`` (a:Scalar) (b:Scalar) =
        let r1 = a + b
        let r2 = b + a
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Addition of Scalar is commutative`` (a:Scalar) (b:Scalar) (c:Scalar) =
        let r1 = (a + b) + c
        let r2 = a + (b + c)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Addition of negated Scalar yields Zero Scalar`` (a:Scalar) =
        let r = a + (-1.0 * a)
        Assert.StrictEqual(Scalar.Zero, r)

    [<Property>]
    let ``Addition of Zero Scalar yields same Scalar`` (a:Scalar) =
        let r = a + Scalar.Zero
        Assert.StrictEqual(a, r)

    [<Property>]
    let ``Multiplication of Scalar is associative`` (a:Scalar) (b:Scalar) =
        let r1 = a * b
        let r2 = b * a
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiplication of Scalar is commutative`` (a:Scalar) (b:Scalar) (c:Scalar) =
        let r1 = (a * b) * c
        let r2 = a * (b * c)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiplication of Zero Scalar yields Zero Scalar`` (a:Scalar) =
        let r = a * Scalar.Zero
        Assert.StrictEqual(Scalar.Zero, r)

    [<Property>]
    let ``Multiplication of Identity Scalar yields same Scalar`` (a:Scalar) =
        let r = a * (Scalar 1.0)
        Assert.StrictEqual(a, r)


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module Decision =

    [<Property>]
    let ``Addition of Decisions is associative`` (d1:Decision) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e1 = d1 + d2
        let e2 = d2 + d1
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of Decisions is commutative`` (d1:Decision) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let d3 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e1 = d1 + (d2 + d3)
        let e2 = (d1 + d2) + d3
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of Decisions and Scalar is associative`` (d:Decision) (s:Scalar) =
        let e1 = d + s
        let e2 = s + d
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of Decisions and Scalar is commutative`` (d1:Decision) (s1:Scalar) (s2:Scalar) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e1 = (d1 + s1) + (d2 + s2)
        let e2 = d1 + (s1 + d2) + s2
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of same Decisions is linear`` (d:Decision) =
        let x1 = randomFloat rng
        let x2 = randomFloat rng
        let r1 = (x1 * d) + (x2 * d)
        let r2 = (x1 + x2) * d
        Assert.Equal(r1, r2)

    [<Property>]
    let ``Multiplication of Decisions and Scalar is associative`` (d:Decision) (s:Scalar) =
        let e1 = d * s
        let e2 = s * d
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Multiplication of Decisions and Scalar is commutative`` (d1:Decision) (s1:Scalar) (s2:Scalar) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e1 = (d1 * s1) + (d2 * s2)
        let e2 = (d2 * s2) + (d1 * s1)
        Assert.Equal(e1, e2)


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module LinearExpression =

    [<Property>]
    let ``Mismatched DecisionType throws error in LinearExpression`` (decisionName:DecisionName) (type1:DecisionType) =
        let type2 = DecisionTypeGen.Where(fun x -> x <> type1) |> Gen.sample 0 1 |> Seq.exactlyOne
        let d1 = { Name = decisionName; Type = type1 }
        let d2 = { Name = decisionName; Type = type2 }
        Prop.throws<System.ArgumentException,_>(lazy (d1 + d2))

    [<Property>]
    let ``Addition of LinearExpression is associative`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr1 = randomExpressionFromDecisions rng decisions
        let expr2 = randomExpressionFromDecisions rng decisions
        let r1 = expr1 + expr2
        let r2 = expr2 + expr1
        Assert.Equal(r1, r2)

    [<Property>]
    let ``Addition of LinearExpression is commutative`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr1 = randomExpressionFromDecisions rng decisions
        let expr2 = randomExpressionFromDecisions rng decisions
        let expr3 = randomExpressionFromDecisions rng decisions
        let r1 = (expr1 + expr2) + expr3
        let r2 = expr1 + (expr2 + expr3)
        Assert.Equal(r1, r2)

    [<Property>]
    let ``Addition of Zero to LinearExpression yields same expression`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr = randomExpressionFromDecisions rng decisions
        let r = expr + LinearExpression.Zero
        Assert.Equal(expr, r)

    [<Property>]
    let ``Multiplication of LinearExpression by Scalar 1 yields same expression`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr = randomExpressionFromDecisions rng decisions
        let r = expr * 1.0
        Assert.Equal(expr, r)

    [<Property>]
    let ``Multiplication of LinearExpression by X then by 1/X yields same expression`` () =
        let x = randomFloat rng
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr = randomExpressionFromDecisions rng decisions
        let r1 = expr * x
        let r2 = r1 * (1.0 / x)
        Assert.Equal(expr, r2)

[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module ModelTests =

    [<Property>]
    let ``Adding Constraint to Model yields a new Model with Constraint`` (sense:ObjectiveSense) (inequality:Inequality) =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let objExpr = randomExpressionFromDecisions rng decisions
        let objective = Objective.create "Test" sense objExpr
        let lhs = (randomExpressionFromDecisions rng decisions)
        let rhs = (randomExpressionFromDecisions rng decisions)
        let cnst = Constraint.create "Test" (Inequality (lhs, inequality, rhs))
        let m = 
            Model.create objective
            |> Model.addConstraint cnst

        Assert.Contains(cnst, m.Constraints)

    [<Fact>]
    let ``Simple Model solves`` () =
        let x1 = Decision.createContinuous "x1" 0.0M Decimal.MaxValue
        let x2 = Decision.createContinuous "x2" 0M Decimal.MaxValue
        
        let objExpr = 2.0 * x1 + 3.0 * x2
        let objective = Objective.create "Get big" Maximize objExpr
        let model = 
            Model.create objective
            |> Model.addConstraint (Constraint.create "Max x1" (x1 <== 10.0))
            |> Model.addConstraint (Constraint.create "Max x2" (x2 <== 5.0))
            |> Model.addConstraint (Constraint.create "Max x1 and x2" (x1 + x2 <== 12.0))
        
        let settings = {
            SolverType = SolverType.CBC
            MaxDuration = 30_000L
            WriteLPFile = Some "Test.lp"
        }
        
        let result = Solve.solve settings model

        match result with
        | Optimal _ -> Assert.True(true)
        | Suboptimal _ -> Assert.True(false, "Simple model failed to solve")