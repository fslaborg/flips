module Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips.Domain
open Flips.Gens
open System
open Flips

let rng = System.Random()
let MIN_COEFFICIENT = -1_000_000_000_000.0
let MAX_COEFFICIENT = 1_000_000_000_000.0

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
    let ``Addition then Subtraction of Scalar yields same Scalar`` (a:Scalar) (b:Scalar)  =
        let r = a + b - b
        Assert.StrictEqual(a, r)

    [<Property>]
    let ``Subtraction then Addition of Scalar yields same Scalar`` (a:Scalar) (b:Scalar)  =
        let r = a - b + b
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
        let d3 = DecisionGen.Where(fun x -> x.Name <> d1.Name && x.Name <> d2.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
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
    let ``Addition then Subtraction of Scalar returns Equivalent`` (d:Decision) (s:Scalar) =
        let e = LinearExpression.OfDecision d
        let r = d + s - s
        Assert.Equal(e, r)

    [<Property>]
    let ``Subtraction then Addition of Scalar returns Equivalent`` (d:Decision) (s:Scalar) =
        let e = LinearExpression.OfDecision d
        let r = d - s + s
        Assert.Equal(e, r)

    [<Property>]
    let ``Addition then Subtraction of Decision returns Equivalent`` (d1:Decision) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e = LinearExpression.OfDecision d1
        let r = d1 + d2 - d2
        Assert.Equal(e, r)

    [<Property>]
    let ``Subtraction then Addition of Decision returns Equivalent`` (d1:Decision) =
        let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        let e = LinearExpression.OfDecision d1
        let r = d1 - d2 + d2
        Assert.Equal(e, r)

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

    [<Property>]
    let ``Adding then Subtracting LinearExpression yields equivalent LinearExpression`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr1 = randomExpressionFromDecisions rng decisions
        let expr2 = randomExpressionFromDecisions rng decisions
        let r = expr1 + expr2 - expr2
        Assert.Equal(expr1, r)

    [<Property>]
    let ``Adding then Subtracting Scalar yields equivalent LinearExpression`` (s:Scalar) =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let expr = randomExpressionFromDecisions rng decisions
        let r = expr + s - s
        Assert.Equal(expr, r)

    [<Property>]
    let ``Adding then Subtracting Decision yields equivalent LinearExpression`` () =
        let numberOfDecisions = rng.Next(1, 100)
        let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
        let d = Seq.item (rng.Next(Seq.length decisions)) decisions
        let expr = randomExpressionFromDecisions rng decisions
        let r = expr + d - d
        Assert.Equal(expr, r)

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
        let x1 = Decision.createContinuous "x1" 0.0 infinity
        let x2 = Decision.createContinuous "x2" 0.0 infinity
        
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

[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module SliceMapTests =
    open SliceMap

    [<Property>]
    let ``SliceMap addition is commutative`` (v1:List<(NonEmptyString * Scalar)>) (v2:List<(NonEmptyString * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap
        let s2 = Map.ofList v2 |> SMap
        let r1 = s1 + s2
        let r2 = s2 + s1
        Assert.StrictEqual(r1, r2)
    
    [<Property>]
    let ``SliceMap addition is associative`` (v1:List<(NonEmptyString * Scalar)>) (v2:List<(NonEmptyString * Scalar)>) (v3:List<(NonEmptyString * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap
        let s2 = Map.ofList v2 |> SMap
        let s3 = Map.ofList v3 |> SMap
        let r1 = (s1 + s2) + s3
        let r2 = s1 + (s2 + s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SliceMap element-wise multiplication is commutative`` (v1:List<(NonEmptyString * Scalar)>) (v2:List<(NonEmptyString * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap
        let s2 = Map.ofList v2 |> SMap
        let r1 = s1 .* s2
        let r2 = s2 .* s1
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SliceMap element-wise multiplication is associative`` (v1:List<(NonEmptyString * Scalar)>) (v2:List<(NonEmptyString * Scalar)>) (v3:List<(NonEmptyString * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap
        let s2 = Map.ofList v2 |> SMap
        let s3 = Map.ofList v3 |> SMap
        let r1 = (s1 .* s2) .* s3
        let r2 = s1 .* (s2 .* s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiply SliceMap by 1 yields same SliceMap`` (v:List<(NonEmptyString * Scalar)>)=
        let s = Map.ofList v |> SMap
        let r = 1.0 * s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap by its inverse then itself yields original `` (v:List<(NonEmptyString * Scalar)>)=
        let s = Map.ofList v |> SMap
        let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap.ofList
        let r = s .* inverse .* s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Adding empty SliceMap yields same SliceMap`` (v:List<(NonEmptyString * Scalar)>)=
        let s = Map.ofList v |> SMap
        let empty = Map.empty |> SMap
        let r = s + empty
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SliceMap by X then 1/X yields same SliceMap`` (v:List<(NonEmptyString * Scalar)>)=
        let x = randomFloat rng
        let s = Map.ofList v |> SMap
        let r1 = x * s
        let r2 = (1.0 / x) * r1
        Assert.StrictEqual(s, r2)

[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module SliceMap2Tests =
    open SliceMap

    [<Property>]
    let ``SMap2 addition is commutative`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let s2 = Map.ofList v2 |> SMap2
        let r1 = s1 + s2
        let r2 = s2 + s1
        Assert.StrictEqual(r1, r2)
    
    [<Property>]
    let ``SMap2 addition is associative`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let s2 = Map.ofList v2 |> SMap2
        let s3 = Map.ofList v3 |> SMap2
        let r1 = (s1 + s2) + s3
        let r2 = s1 + (s2 + s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap2 element-wise multiplication is commutative`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let s2 = Map.ofList v2 |> SMap2
        let r1 = s1 .* s2
        let r2 = s2 .* s1
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap2 element-wise multiplication is associative`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let s2 = Map.ofList v2 |> SMap2
        let s3 = Map.ofList v3 |> SMap2
        let r1 = (s1 .* s2) .* s3
        let r2 = s1 .* (s2 .* s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiply SMap2 by 1 yields same SMap2`` (v:List<((NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap2
        let r = 1.0 * s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap2 by its inverse then itself yields original `` (v:List<((NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap2
        let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap2.ofList
        let r = s .* inverse .* s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Adding empty SMap2 yields same SMap2`` (v:List<((NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap2
        let empty = Map.empty |> SMap2
        let r = s + empty
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap2 by X then 1/X yields same SMap2`` (v:List<((NonEmptyString * NonEmptyString) * Scalar)>)=
        let x = randomFloat rng
        let s = Map.ofList v |> SMap2
        let r1 = x * s
        let r2 = (1.0 / x) * r1
        Assert.StrictEqual(s, r2)

    [<Property>]
    let ``Multiplication of SMap2 by SMap then 1/SMap yields initial`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let v2 = v1 |> List.map (fun ((k1, k2), v) -> k2, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap by 1/SMap then by SMap2 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap2
        let v2 = v1 |> List.map (fun ((k1, k2), v) -> k1, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module SliceMap3Tests =
    open SliceMap

    [<Property>]
    let ``SMap3 addition is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let s2 = Map.ofList v2 |> SMap3
        let r1 = s1 + s2
        let r2 = s2 + s1
        Assert.StrictEqual(r1, r2)
    
    [<Property>]
    let ``SMap3 addition is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let s2 = Map.ofList v2 |> SMap3
        let s3 = Map.ofList v3 |> SMap3
        let r1 = (s1 + s2) + s3
        let r2 = s1 + (s2 + s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap3 element-wise multiplication is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let s2 = Map.ofList v2 |> SMap3
        let r1 = s1 .* s2
        let r2 = s2 .* s1
        Assert.StrictEqual(r1, r2)
    
    [<Property>]
    let ``SMap3 element-wise multiplication is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let s2 = Map.ofList v2 |> SMap3
        let s3 = Map.ofList v3 |> SMap3
        let r1 = (s1 .* s2) .* s3
        let r2 = s1 .* (s2 .* s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiply SMap3 by 1 yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap3
        let r = 1.0 * s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap3 by its inverse then itself yields original `` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap3
        let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap3.ofList
        let r = s .* inverse .* s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Adding empty SMap3 yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap3
        let empty = Map.empty |> SMap3
        let r = s + empty
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap3 by X then 1/X yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let x = randomFloat rng
        let s = Map.ofList v |> SMap3
        let r1 = x * s
        let r2 = (1.0 / x) * r1
        Assert.StrictEqual(s, r2)

    [<Property>]
    let ``Multiplication of SMap3 by SMap then 1/SMap yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> k3, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Multiplication of SMap3 by SMap2 then 1/SMap2 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> (k2, k3), v) |> List.distinctBy fst
        let s2 = v2 |> SMap2.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap2.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap by 1/SMap then by SMap3 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> k1, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap2 by 1/SMap2 then by SMap3 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap3
        let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> (k1, k2), v) |> List.distinctBy fst
        let s2 = v2 |> SMap2.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap2.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)


[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module SliceMap4Tests =
    open SliceMap

    [<Property>]
    let ``SMap4 addition is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let s2 = Map.ofList v2 |> SMap4
        let r1 = s1 + s2
        let r2 = s2 + s1
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap4 addition is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let s2 = Map.ofList v2 |> SMap4
        let s3 = Map.ofList v3 |> SMap4
        let r1 = (s1 + s2) + s3
        let r2 = s1 + (s2 + s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap4 element-wise multiplication is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let s2 = Map.ofList v2 |> SMap4
        let r1 = s1 .* s2
        let r2 = s2 .* s1
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``SMap4 element-wise multiplication is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let s2 = Map.ofList v2 |> SMap4
        let s3 = Map.ofList v3 |> SMap4
        let r1 = (s1 .* s2) .* s3
        let r2 = s1 .* (s2 .* s3)
        Assert.StrictEqual(r1, r2)

    [<Property>]
    let ``Multiply SMap4 by 1 yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap4
        let r = 1.0 * s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap4 by its inverse then itself yields original `` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap4
        let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap4.ofList
        let r = s .* inverse .* s
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Adding empty SMap4 yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let s = Map.ofList v |> SMap4
        let empty = Map.empty |> SMap4
        let r = s + empty
        Assert.StrictEqual(s, r)

    [<Property>]
    let ``Multiply SMap4 by X then 1/X yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
        let x = randomFloat rng
        let s = Map.ofList v |> SMap4
        let r1 = x * s
        let r2 = (1.0 / x) * r1
        Assert.StrictEqual(s, r2)

    [<Property>]
    let ``Multiplication of SMap4 by SMap then 1/SMap yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> k4, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Multiplication of SMap4 by SMap2 then 1/SMap2 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) |> List.distinctBy fst
        let s2 = v2 |> SMap2.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap2.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Multiplication of SMap4 by SMap3 then 1/SMap3 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k2, k3, k4), v) |> List.distinctBy fst
        let s2 = v2 |> SMap3.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap3.ofList
        let r = s1 .* s2 .* s2Inverse
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap by 1/SMap then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> k1, v) |> List.distinctBy fst
        let s2 = v2 |> SMap.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap2 by 1/SMap2 then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k1, k2), v) |> List.distinctBy fst
        let s2 = v2 |> SMap2.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap2.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)

    [<Property>]
    let ``Elementwise-multiplication of SMap3 by 1/SMap3 then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
        let s1 = Map.ofList v1 |> SMap4
        let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k3), v) |> List.distinctBy fst
        let s2 = v2 |> SMap3.ofList
        let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Scalar 1.0) / x) |> SMap3.ofList
        let r = s2 .* (s2Inverse .* s1)
        Assert.StrictEqual(r, s1)