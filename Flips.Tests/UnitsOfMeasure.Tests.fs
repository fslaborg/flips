namespace Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips
open Flips.Types
open Flips.Tests.Gens
open Flips.Tests.Types


module UnitsOfMeasureTests =
    open Flips.UnitsOfMeasure
    open Flips.UnitsOfMeasure.Types
    open Flips.Tests.Gens.UnitsOfMeasure

    let randomItemExpressionFromDecisions rng (decisions:seq<Decision<Item>>) =
        let decisionGen = Gen.elements decisions
        let numberOfElements = Gen.choose (1, 10) |> Gen.sample 0 1 |> Seq.exactlyOne
        let expr = 
            decisionGen 
            |> Gen.sample 0 numberOfElements 
            |> List.map (fun x -> (randomInRange MIN_COEFFICIENT MAX_COEFFICIENT rng) * x) 
            |> List.sum
        expr

    [<Properties(Arbitrary = [| typeof<UnitsOfMeasure.UnitOfMeasureTypes>; typeof<Types> |] )>]
    module Decision =

        [<Property>]
        let ``Addition of Decisions is associative`` (d1:Decision<Item>) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = d1 + d2
            let e2 = d2 + d1
            Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of Decisions is commutative`` (d1:Decision<Item>) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let d3 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name && x.Name <> d2.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = d1 + (d2 + d3)
            let e2 = (d1 + d2) + d3
            Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of Decisions and float is associative`` (d:Decision<Item>) (SmallFloatItem f) =
            let e1 = d + f
            let e2 = f + d
            Assert.Equal(e1, e2)

        //[<Property>]
        //let ``Addition of Decisions and Scalar is associative`` (d:Decision<Item>) (s:Scalar) =
        //    let e1 = d + s
        //    let e2 = s + d
        //    Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of Decisions and float is commutative`` (d1:Decision<Item>) (SmallFloatItem f1) (SmallFloatItem f2) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 + f1) + (d2 + f2)
            let e2 = d1 + (f1 + d2) + f2
            Assert.Equal(e1, e2)

        //[<Property>]
        //let ``Addition of Decisions and Scalar is commutative`` (Decision.Value d1:Decision<Item>) (s1:Scalar) (s2:Scalar) =
        //    let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        //    let e1 = (d1 + s1) + (d2 + s2)
        //    let e2 = d1 + (s1 + d2) + s2
        //    Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of same Decisions is linear`` (d:Decision<Item>) =
            let x1 = randomFloat rng
            let x2 = randomFloat rng
            let r1 = (x1 * d) + (x2 * d)
            let r2 = (x1 + x2) * d
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Addition then Subtraction of float returns Equivalent`` (d:Decision<Item>) (SmallFloatItem f) =
            let e = 1.0 * d
            let r = d + f - f
            Assert.Equal(e, r)

        //[<Property>]
        //let ``Addition then Subtraction of Scalar returns Equivalent`` (d:Decision<Item>) (SmallFloatItem f) =
        //    let e = 1.0 * d
        //    let r = d + f - f
        //    Assert.Equal(e, r)

        [<Property>]
        let ``Subtraction then Addition of float returns Equivalent`` (d:Decision<Item>) (SmallFloatItem f) =
            let e = 1.0 * d
            let r = d - f + f
            Assert.Equal(e, r)

        //[<Property>]
        //let ``Subtraction then Addition of Scalar returns Equivalent`` (d:Decision<Item>) (SmallFloatItem f) =
        //    let e = 1.0 * d
        //    let r = d - f + f
        //    Assert.Equal(e, r)

        [<Property>]
        let ``Addition then Subtraction of Decision returns Equivalent`` (d1:Decision<Item>) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e = 1.0 * d1
            let r = d1 + d2 - d2
            Assert.Equal(e, r)

        [<Property>]
        let ``Subtraction then Addition of Decision returns Equivalent`` (d1:Decision<Item>) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e = 1.0 * d1
            let r = d1 - d2 + d2
            Assert.Equal(e, r)

        [<Property>]
        let ``Multiplication of Decisions and float is associative`` (d:Decision<Item>) (SmallFloatItem f) =
            let e1 = d * f
            let e2 = f * d
            Assert.Equal(e1, e2)

        //[<Property>]
        //let ``Multiplication of Decisions and Scalar is associative`` (Decision.Value d:Decision<Item>) (s:Scalar) =
        //    let e1 = d * s
        //    let e2 = s * d
        //    Assert.Equal(e1, e2)

        [<Property>]
        let ``Multiplication of Decisions and float is commutative`` (d1:Decision<Item>) (SmallFloatItem f1) (SmallFloatItem f2) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 * f1) + (d2 * f2)
            let e2 = (d2 * f2) + (d1 * f1)
            Assert.Equal(e1, e2)

        //[<Property>]
        //let ``Multiplication of Decisions and Scalar is commutative`` (d1:Decision<Item>) (s1:Scalar) (s2:Scalar) =
        //    let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
        //    let e1 = (Decision.Value d1 * s1) + (Decision.Value d2 * s2)
        //    let e2 = (Decision.Value d2 * s2) + (Decision.Value d1 * s1)
        //    Assert.Equal(e1, e2)

    [<Properties(Arbitrary = [| typeof<UnitsOfMeasure.UnitOfMeasureTypes>; typeof<Types> |] )>]
    module LinearExpression =

        [<Property>]
        let ``Mismatched DecisionType throws error in LinearExpression`` (DecisionName decisionName:DecisionName) (type1:DecisionType) =
            let type2 = DecisionTypeGen.Where(fun x -> x <> type1) |> Gen.sample 0 1 |> Seq.exactlyOne
            let d1 = Decision.create<Item> decisionName type1
            let d2 = Decision.create<Item> decisionName type2
            Prop.throws<System.ArgumentException,_>(lazy (d1 + d2))

        [<Property>]
        let ``Addition of LinearExpression is associative`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr1 = randomItemExpressionFromDecisions rng decisions
            let expr2 = randomItemExpressionFromDecisions rng decisions
            let r1 = expr1 + expr2
            let r2 = expr2 + expr1
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Addition of LinearExpression is commutative`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr1 = randomItemExpressionFromDecisions rng decisions
            let expr2 = randomItemExpressionFromDecisions rng decisions
            let expr3 = randomItemExpressionFromDecisions rng decisions
            let r1 = (expr1 + expr2) + expr3
            let r2 = expr1 + (expr2 + expr3)
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Addition of LinearExpression and float is commutative`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r1 = expr + 1.0<Item>
            let r2 = 1.0<Item> + expr
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Addition of Zero to LinearExpression yields same expression`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r = expr + LinearExpression<Item>.Zero
            Assert.Equal(expr, r)

        [<Property>]
        let ``Multiplication of LinearExpression by float 1 yields same expression`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r = expr * 1.0
            Assert.Equal(expr, r)

        [<Property>]
        let ``Multiplication of LinearExpression and float is commutative`` (SmallFloat f) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r1 = expr * f
            let r2 = f * expr
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Multiplication of LinearExpression and float<Item> is commutative`` (SmallFloatItem f) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r1 = expr * f
            let r2 = f * expr
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Multiplication of LinearExpression by X then by 1/X yields same expression`` () =
            let x = randomFloat rng
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r1 = expr * x
            let r2 = r1 * (1.0 / x)
            Assert.Equal(expr, r2)

        [<Property>]
        let ``Adding then Subtracting LinearExpression yields equivalent LinearExpression`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr1 = randomItemExpressionFromDecisions rng decisions
            let expr2 = randomItemExpressionFromDecisions rng decisions
            let r = expr1 + expr2 - expr2
            Assert.Equal(expr1, r)

        [<Property>]
        let ``Adding then Subtracting float yields equivalent LinearExpression`` (SmallFloatItem f) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomItemExpressionFromDecisions rng decisions
            let r = expr + f - f
            Assert.Equal(expr, r)

        [<Property>]
        let ``Adding then Subtracting Decision yields equivalent LinearExpression`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionItemGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let d = Seq.item (rng.Next(Seq.length decisions)) decisions
            let expr = randomItemExpressionFromDecisions rng decisions
            let r = expr + d - d
            Assert.Equal(expr, r)


