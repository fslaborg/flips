module Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips.Domain
open Flips.Gens

let rng = System.Random()
let MIN_COEFFICIENT = -1_000_000_000.0
let MAX_COEFFICIENT = 1_000_000_000.0

let randomInRange lowerBound upperBound (rng:System.Random) =
    let range = upperBound - lowerBound
    lowerBound + (rng.NextDouble() * range)

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
    let ``Multiplication of Scalar is associative`` (a:Scalar) (b:Scalar) =
        let r1 = a * b
        let r2 = b * a
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiplication of Scalar is commutative`` (a:Scalar) (b:Scalar) (c:Scalar) =
        let r1 = (a * b) * c
        let r2 = a * (b * c)
        Assert.StrictEqual(r1, r2)


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module LinearExpression =

    [<Property>]
    let ``Addition of Decisions is associative`` (d1:Decision) (d2:Decision) =

        let e1 = d1 + d2
        let e2 = d2 + d1
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of Decisions is commutative`` (d1:Decision) (d2:Decision) (d3:Decision) =

        let e1 = d1 + (d2 + d3)
        let e2 = (d1 + d2) + d3
        Assert.Equal(e1, e2)

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