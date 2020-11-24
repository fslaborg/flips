namespace Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open SliceMap
open Flips
open Flips.Legacy
open Flips.Types
open Flips.Legacy.Types
open Flips.Tests.Gens

module Types =

    type Job = Job of int
    type Machine = Machine of int


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


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
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
        let ``Addition of Decisions and float is associative`` (d:Decision) (SmallFloat f) =
            let e1 = d + f
            let e2 = f + d
            Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of Decisions and float is commutative`` (d1:Decision) (SmallFloat f1) (SmallFloat f2) =
            let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 + f1) + (d2 + f2)
            let e2 = d1 + (f1 + d2) + f2
            Assert.Equal(e1, e2)

        [<Property>]
        let ``Addition of same Decisions is linear`` (d:Decision) =
            let x1 = randomFloat rng
            let x2 = randomFloat rng
            let r1 = (x1 * d) + (x2 * d)
            let r2 = (x1 + x2) * d
            Assert.Equal(r1, r2)

        [<Property>]
        let ``Addition then Subtraction of float returns Equivalent`` (d:Decision) (SmallFloat f) =
            let e = LinearExpression.OfDecision d
            let r = d + f - f
            Assert.Equal(e, r)

        [<Property>]
        let ``Subtraction then Addition of float returns Equivalent`` (d:Decision) (SmallFloat f) =
            let e = LinearExpression.OfDecision d
            let r = d - f + f
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
        let ``Multiplication of Decisions and float is associative`` (d:Decision) (SmallFloat f) =
            let e1 = d * f
            let e2 = f * d
            Assert.Equal(e1, e2)

        [<Property>]
        let ``Multiplication of Decisions and float is commutative`` (d1:Decision) (SmallFloat f1) (SmallFloat f2) =
            let d2 = DecisionGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 * f1) + (d2 * f2)
            let e2 = (d2 * f2) + (d1 * f1)
            Assert.Equal(e1, e2)


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module LinearExpression =

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
        let ``Addition of LinearExpression and float is commutative`` (SmallFloat f) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomExpressionFromDecisions rng decisions
            let r1 = expr + f
            let r2 = f + expr
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
        let ``Multiplication of LinearExpression and float is commutative`` (SmallFloat f) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let expr = randomExpressionFromDecisions rng decisions
            let r1 = expr * f
            let r2 = f * expr
            Assert.Equal(r1, r2)

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
        let ``Adding then Subtracting Decision yields equivalent LinearExpression`` () =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)
            let d = Seq.item (rng.Next(Seq.length decisions)) decisions
            let expr = randomExpressionFromDecisions rng decisions
            let r = expr + d - d
            Assert.Equal(expr, r)


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
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
                WriteMPSFile = Some "Test.mps"
            }
        
            let result = Solver.solve settings model

            match result with
            | Optimal _ -> Assert.True(true)
            | _ -> Assert.True(false, "Simple model failed to solve")

        [<Fact>]
        let ``Adding Empty Constraint Seq does not raise error`` () =
            let x1 = Decision.createContinuous "x1" 0.0 infinity
        
            let objExpr = 1.0 * x1
            let objective = Objective.create "Get big" Maximize objExpr

            let emptyConstraintList = []

            let model = 
                Model.create objective
                |> Model.addConstraints emptyConstraintList

            ()

        [<Property>]
        let ``MultiObjective does not violate worsen initial objective`` () =
            let maxAssignments = 5.0
            let maxMachineAssignments = 2.0
            let jobs = [1..9] |> List.map Job
            let machines = [1..5] |> List.map Machine
            let jobRevenue =
                jobs
                |> List.map (fun j -> j, float (rng.Next(5, 10)))
                |> SMap.ofList

            let jobMachineWaste =
                [for j in jobs do
                    for m in machines ->
                        (j, m), float (rng.Next(1, 5))
                ] |> SMap2.ofList

            let assignments =
                DecisionBuilder "Assignment" {
                    for j in jobs do
                        for m in machines ->
                            Boolean
                } |> SMap2.ofSeq

            // Sum the total revenue for the jobs selected
            let revenueExpr = sum (jobRevenue .* assignments)

            // Sum the waste for running a job on a particular machine
            let wasteExpr = sum (assignments .* jobMachineWaste)

            // Create an objective to maximize the revenue
            let revenueObjective = Objective.create "MaxRevenue" Maximize revenueExpr

            // Create an objective to minimize the waste
            let wasteObjective = Objective.create "MinWaste" Minimize wasteExpr

            // Create a constraint which limits the number of jobs that can be assigned
            let assignmentConstraint =
                Constraint.create "MaxNumberOfAssignments" (sum assignments <== maxAssignments)

            // Create constraints for a job only being assigned once
            let oneAssignmentConstraints =
                ConstraintBuilder "OneAssignment" {
                    for j in jobs ->
                        sum assignments.[j, All] <== 1.0
                }

            // Create constraints so that no machine can have more than max number of jobs
            let machineAssignmentConstraints =
                ConstraintBuilder "MaxMachineAssignement" {
                    for m in machines ->
                        sum assignments.[All, m] <== maxMachineAssignments
                }

            let initialModel =
                Model.create revenueObjective
                |> Model.addConstraint assignmentConstraint
                |> Model.addConstraints machineAssignmentConstraints
                |> Model.addConstraints oneAssignmentConstraints

            let postModel =
              Model.create revenueObjective
              |> Model.addObjective wasteObjective
              |> Model.addConstraint assignmentConstraint
              |> Model.addConstraints machineAssignmentConstraints
              |> Model.addConstraints oneAssignmentConstraints


            // Create a Settings type which tells the Solver which types of underlying solver to use,
            // the time alloted for solving, and whether to write an LP file to disk
            let settings = {
                SolverType = SolverType.CBC
                MaxDuration = 10_000L
                WriteLPFile = None
                WriteMPSFile = None
            }

            // Call the `solve` function in the Solve module to evaluate the model
            let initialResult = Solver.solve settings initialModel

            let postResult = Solver.solve settings postModel

            match initialResult, postResult with
            | Optimal sln1, Optimal sln2 ->
                let initialRevenue = Objective.evaluate sln1 revenueObjective
                let postSolutionRevenue = Objective.evaluate sln2 revenueObjective

                let revenueIsTheSame =
                    System.Math.Abs (initialRevenue - postSolutionRevenue) < 0.001
                
                Assert.True(revenueIsTheSame)
            | _, Optimal _ -> Assert.True(false, "initialModel failed to solve")
            | Optimal _, _ -> Assert.True(false, "postModel failed to solve")
            | _, _ -> Assert.True(false, "The initialModel and postModel failed to solve")

 
    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SumTests =
        
        open Flips.SliceMap

        [<Property>]
        let ``Sum of SMap works`` (indices:List<NonEmptyString>) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)

            let v = 
                indices
                |> List.distinct
                |> List.map (fun idx -> idx, randomExpressionFromDecisions rng decisions)
            let sm = SMap.ofList v

            let result = sum (sm)
            let expected = v |> List.sumBy snd

            Assert.StrictEqual(expected, result)

        [<Property>]
        let ``Sum of SMap2 works`` (indices:List<NonEmptyString * NonEmptyString>) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)

            let v = 
                indices.[..3]
                |> List.distinct
                |> List.map (fun idx -> idx, randomExpressionFromDecisions rng decisions)
            let sm = SMap2.ofList v

            let result = sum (sm)
            let expected = v |> List.sumBy snd

            Assert.StrictEqual(expected, result)

        [<Property>]
        let ``Sum of SMap3 works`` (indices:List<NonEmptyString * NonEmptyString * NonEmptyString>) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)

            let v = 
                indices.[..3]
                |> List.distinct
                |> List.map (fun idx -> idx, randomExpressionFromDecisions rng decisions)
            let sm = SMap3.ofList v

            let result = sum (sm)
            let expected = v |> List.sumBy snd

            Assert.StrictEqual(expected, result)

        [<Property>]
        let ``Sum of SMap4 works`` (indices:List<NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString>) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)

            let v = 
                indices.[..3]
                |> List.distinct
                |> List.map (fun idx -> idx, randomExpressionFromDecisions rng decisions)
            let sm = SMap4.ofList v

            let result = sum (sm)
            let expected = v |> List.sumBy snd

            Assert.StrictEqual(expected, result)

        [<Property>]
        let ``Sum of SMap5 works`` (indices:List<NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString>) =
            let numberOfDecisions = rng.Next(1, 100)
            let decisions = DecisionGen |> Gen.sample 0 numberOfDecisions |> Seq.distinctBy (fun x -> x.Name)

            let v = 
                indices.[..3]
                |> List.distinct
                |> List.map (fun idx -> idx, randomExpressionFromDecisions rng decisions)
            let sm = SMap5.ofList v

            let result = sum (sm)
            let expected = v |> List.sumBy snd

            Assert.StrictEqual(expected, result)


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module DecisionBuilderTests =
        open Flips

        type private Animal = 
            | Chicken of decimal
            | Donkey of string

        [<Fact>]
        let ``8D DecisionBuilder works`` () =
            let x = 
                DecisionBuilder "WaterSent" {
                    for i in ["a"; "b"] do
                        for j in 1..4 do
                            for k in [1M; 2M] do
                                for l in 1..3 do
                                    for m in [Chicken 100M; Donkey "WinkyWonky"] do
                                        for n in 1..2 do
                                            for p in 1..2 do
                                                for q in 1..2 -> 
                                                    Continuous (0.0, infinity)
                } |> Map.ofSeq

            Assert.True(true)