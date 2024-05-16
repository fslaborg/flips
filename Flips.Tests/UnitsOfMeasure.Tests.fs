namespace Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips
open Flips.Types
open Flips.Tests.Gens
open Flips.Tests.Types


[<Measure>] type cm

module UnitsOfMeasureTests =
    open Flips.SliceMap
    open Flips.UnitsOfMeasure
    open Flips.UnitsOfMeasure.Types
    open Flips.Tests.UnitsOfMeasure
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

        [<Property>]
        let ``Addition of Decisions and float is commutative`` (d1:Decision<Item>) (SmallFloatItem f1) (SmallFloatItem f2) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 + f1) + (d2 + f2)
            let e2 = d1 + (f1 + d2) + f2
            Assert.Equal(e1, e2)

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

        [<Property>]
        let ``Subtraction then Addition of float returns Equivalent`` (d:Decision<Item>) (SmallFloatItem f) =
            let e = 1.0 * d
            let r = d - f + f
            Assert.Equal(e, r)

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

        [<Property>]
        let ``Multiplication of Decisions and float is commutative`` (d1:Decision<Item>) (SmallFloatItem f1) (SmallFloatItem f2) =
            let d2 = DecisionItemGen.Where(fun x -> x.Name <> d1.Name) |> Gen.sample 0 1 |> Seq.exactlyOne
            let e1 = (d1 * f1) + (d2 * f2)
            let e2 = (d2 * f2) + (d1 * f1)
            Assert.Equal(e1, e2)


    [<Properties(Arbitrary = [| typeof<UnitsOfMeasure.UnitOfMeasureTypes>; typeof<Types> |] )>]
    module LinearExpression =

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

    [<Properties(Arbitrary = [| typeof<UnitsOfMeasure.UnitOfMeasureTypes>; typeof<Types> |] )>]
    module Model =

      type [<Measure>] job

      [<Property>]
      let ``MultiObjective does not violate worsen initial objective`` () =
          let maxAssignments = 5.0<job>
          let maxMachineAssignments = 2.0<job>
          let jobs = [1..9] |> List.map Job
          let machines = [1..5] |> List.map Machine
          let jobRevenue =
              jobs
              |> List.map (fun j -> j, float (rng.Next(5, 10)))
              |> SMap.ofList

          let jobMachineWaste =
              [for j in jobs do
                  for m in machines ->
                      (j, m), 1.0<cm> * float (rng.Next(1, 5))
              ] |> SMap2.ofList

          let assignments =
              DecisionBuilder<job> "Assignment" {
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
                      sum assignments.[j, All] <== 1.0<job>
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
                  System.Math.Abs ((float initialRevenue) - (float postSolutionRevenue)) < 0.001
              
              Assert.True(revenueIsTheSame)
          | _, Optimal _ -> Assert.True(false, "initialModel failed to solve")
          | Optimal _, _ -> Assert.True(false, "postModel failed to solve")
          | _, _ -> Assert.True(false, "The initialModel and postModel failed to solve")

    [<Properties(Arbitrary = [| typeof<UnitsOfMeasure.UnitOfMeasureTypes>; typeof<Types> |] )>]
    module SMaps =
        // just make sure it compiles (was buggy giving SMap<NonEmptyString, Scalar<Item ^ 2>> due to implementation issue, same for SMap2, SMap3, SMap4 and SMap5)
        [<Property>]
        let ``SMap pairwise addition doesn't affect measure type `` (a: List<NonEmptyString*Scalar<Item>>) (b: List<NonEmptyString*Scalar<Item>>) =
          let a = SMap.ofList a
          let b = SMap.ofList b
          let c : SMap<NonEmptyString, Scalar<Item>> = a + b
          ()
        [<Property>]
        let ``SMap2 pairwise addition doesn't affect measure type `` (a: List<(NonEmptyString*NonEmptyString)*Scalar<Item>>) (b: List<(NonEmptyString*NonEmptyString)*Scalar<Item>>) =
          let a = SMap2.ofList a
          let b = SMap2.ofList b
          let c : SMap2<NonEmptyString, NonEmptyString, Scalar<Item>> = a + b
          ()
        [<Property>]
        let ``SMap3 pairwise addition doesn't affect measure type `` (a: List<(NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) (b: List<(NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) =
          let a = SMap3.ofList a
          let b = SMap3.ofList b
          let c : SMap3<NonEmptyString,NonEmptyString,NonEmptyString, Scalar<Item>> = a + b
          ()
        [<Property>]
        let ``SMap4 pairwise addition doesn't affect measure type `` (a: List<(NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) (b: List<(NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) =
          let a = SMap4.ofList a
          let b = SMap4.ofList b
          let c : SMap4<NonEmptyString,NonEmptyString,NonEmptyString,NonEmptyString, Scalar<Item>> = a + b
          ()
        [<Property>]
        let ``SMap5 pairwise addition doesn't affect measure type `` (a: List<(NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) (b: List<(NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString*NonEmptyString)*Scalar<Item>>) =
          let a = SMap5.ofList a
          let b = SMap5.ofList b
          let c : SMap5<NonEmptyString,NonEmptyString,NonEmptyString,NonEmptyString,NonEmptyString, Scalar<Item>> = a + b
          ()

        [<Property>]
        let ``SMap works with Solution.getValues`` (d:List<(NonEmptyString * Scalar<cm>)>) =
            let uniqueValues = d |> List.distinctBy fst
            let expectedResult = uniqueValues |> Map.ofList
            let names = uniqueValues |> List.map fst
            let decisions = DecisionBuilder<cm> "Test" { for n in names -> Boolean } |> SMap
            let decisionResults = uniqueValues |> List.map (fun (n, (Scalar.Value s)) -> let (Decision.Value d) = decisions.[n]
                                                                                         d, float s) |> Map.ofList
            let solution = {
                ObjectiveValue = 0.0
                DecisionResultsMap = decisionResults
            }

            let resultValues = 
                Solution.getValues solution decisions
                |> Map.map (fun k v -> Scalar<cm>.Value v)

            Assert.StrictEqual(expectedResult, resultValues)

        [<Property>]
        let ``SMap2 works with Solution.getValues`` (d:List<((NonEmptyString * NonEmptyString) * Scalar<cm>)>) =
            let uniqueValues = d |> List.distinctBy fst
            let expectedResult = uniqueValues |> Map.ofList
            let names = uniqueValues |> List.map fst
            let decisions = DecisionBuilder<cm> "Test" { for n in names -> Boolean } |> SMap2
            let decisionResults = uniqueValues |> List.map (fun (n, (Scalar.Value s)) -> let (Decision.Value d) = decisions.[n]
                                                                                         d, float s) |> Map.ofList
            let solution = {
                ObjectiveValue = 0.0
                DecisionResultsMap = decisionResults
            }

            let resultValues = 
                Solution.getValues solution decisions
                |> Map.map (fun k v -> Scalar<cm>.Value v)

            Assert.StrictEqual(expectedResult, resultValues)

        [<Property>]
        let ``SMap3 works with Solution.getValues`` (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar<cm>)>) =
            let uniqueValues = d |> List.distinctBy fst
            let expectedResult = uniqueValues |> Map.ofList
            let names = uniqueValues |> List.map fst
            let decisions = DecisionBuilder<cm> "Test" { for n in names -> Boolean } |> SMap3
            let decisionResults = uniqueValues |> List.map (fun (n, (Scalar.Value s)) -> let (Decision.Value d) = decisions.[n]
                                                                                         d, float s) |> Map.ofList
            let solution = {
                ObjectiveValue = 0.0
                DecisionResultsMap = decisionResults
            }

            let resultValues = 
                Solution.getValues solution decisions
                |> Map.map (fun k v -> Scalar<cm>.Value v)

            Assert.StrictEqual(expectedResult, resultValues)

        [<Property>]
        let ``SMap4 works with Solution.getValues`` (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar<cm>)>) =
            let uniqueValues = d |> List.distinctBy fst |> fun x -> x.[..6]
            let expectedResult = uniqueValues |> Map.ofList
            let names = uniqueValues |> List.map fst
            let decisions = DecisionBuilder<cm> "Test" { for n in names -> Boolean } |> SMap4
            let decisionResults = uniqueValues |> List.map (fun (n, (Scalar.Value s)) -> let (Decision.Value d) = decisions.[n]
                                                                                         d, float s) |> Map.ofList
            let solution = {
                ObjectiveValue = 0.0
                DecisionResultsMap = decisionResults
            }

            let resultValues = 
                Solution.getValues solution decisions
                |> Map.map (fun k v -> Scalar<cm>.Value v)

            Assert.StrictEqual(expectedResult, resultValues)

        [<Property>]
        let ``SMap5 works with Solution.getValues`` (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar<cm>)>) =
            let uniqueValues = d |> List.distinctBy fst |> fun x -> x.[..6]
            let expectedResult = uniqueValues |> Map.ofList
            let names = uniqueValues |> List.map fst
            let decisions = DecisionBuilder<cm> "Test" { for n in names -> Boolean } |> SMap5
            let decisionResults = uniqueValues |> List.map (fun (n, (Scalar.Value s)) -> let (Decision.Value d) = decisions.[n]
                                                                                         d, float s) |> Map.ofList
            let solution = {
                ObjectiveValue = 0.0
                DecisionResultsMap = decisionResults
            }

            let resultValues = 
                Solution.getValues solution decisions
                |> Map.map (fun k v -> Scalar<cm>.Value v)

            Assert.StrictEqual(expectedResult, resultValues)
          
