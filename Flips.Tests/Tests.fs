namespace Flips.Tests

module Types =

    type Job = Job of int
    type Machine = Machine of int

    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Flips
    open Flips.Types
    open Flips.SliceMap
    open Flips.Tests.Gens

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
            }

            // Call the `solve` function in the Solve module to evaluate the model
            let initialResult = Solver.solve settings initialModel

            let postResult = Solver.solve settings postModel

            match initialResult, postResult with
            | Optimal sln1, Optimal sln2 ->
                let initialRevenue = sln1.ObjectiveResult
                let postSolutionRevenue = Solution.evaluate sln2 revenueExpr

                let revenueIsTheSame =
                    System.Math.Abs (initialRevenue - postSolutionRevenue) < 0.001
                
                Assert.True(revenueIsTheSame)
            | _, Optimal _ -> Assert.True(false, "initialModel failed to solve")
            | Optimal _, _ -> Assert.True(false, "postModel failed to solve")
            | _, _ -> Assert.True(false, "The initialModel and postModel failed to solve")



    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SlicetSetTests =
        open Flips.SliceMap


        [<Property>]
        let ``SliceSet has only distinct values`` (v:List<NonEmptyString>) =
            let s = SliceSet v
            let distinctCount = s |> SliceSet.toList |> List.distinct

            Assert.True(s.Count = distinctCount.Length)


        [<Property>]
        let ``SliceSet GreaterThan includes matches and excludes non-matches`` (v:Set<NonEmptyString>) (a:NonEmptyString) =
            let testSet = Set.add a v
            let s = SliceSet testSet
            let result = s.GreaterThan a

            let matches = testSet |> Set.filter (fun x -> x > a)
            let nonMatches = testSet - matches

            for m in matches do
                Assert.True(result.Contains m)

            for x in nonMatches do
                Assert.True(not (result.Contains x))


        [<Property>]
        let ``SliceSet GreaterOrEqual includes matches and excludes non-matches`` (v:Set<NonEmptyString>) (a:NonEmptyString) =
            let testSet = Set.add a v
            let s = SliceSet testSet
            let result = s.GreaterOrEqual a

            let matches = testSet |> Set.filter (fun x -> x >= a)
            let nonMatches = testSet - matches

            for m in matches do
                Assert.True(result.Contains m)

            for x in nonMatches do
                Assert.True(not (result.Contains x))


        [<Property>]
        let ``SliceSet LessThan includes matches and excludes non-matches`` (v:Set<NonEmptyString>) (a:NonEmptyString) =
            let testSet = Set.add a v
            let s = SliceSet testSet
            let result = s.LessThan a

            let matches = testSet |> Set.filter (fun x -> x < a)
            let nonMatches = testSet - matches

            for m in matches do
                Assert.True(result.Contains m)

            for x in nonMatches do
                Assert.True(not (result.Contains x))


        [<Property>]
        let ``SliceSet LessOrEqual includes matches and excludes non-matches`` (v:Set<NonEmptyString>) (a:NonEmptyString) =
            let testSet = Set.add a v
            let s = SliceSet testSet
            let result = s.LessOrEqual a

            let matches = testSet |> Set.filter (fun x -> x <= a)
            let nonMatches = testSet - matches

            for m in matches do
                Assert.True(result.Contains m)

            for x in nonMatches do
                Assert.True(not (result.Contains x))


        [<Property>]
        let ``SliceSet Between includes matches and excludes non-matches`` (v:List<NonEmptyString>) (a:NonEmptyString) (b:NonEmptyString) =
            let (a, b) = if a < b then (a, b) else (b, a)
            let testSet = [a; b] @ v |> Set.ofList
            let s = SliceSet testSet
            let result = s.Between a b

            let matches = testSet |> Set.filter (fun x -> x >= a && x <= b)
            let nonMatches = testSet - matches

            for m in matches do
                Assert.True(result.Contains m)

            for x in nonMatches do
                Assert.True(not (result.Contains x))

        [<Property>]
        let ``SliceSet Intersect only includes overlap`` (v1:Set<NonEmptyString>) (v2:Set<NonEmptyString>) (a:NonEmptyString) =
            let v1 = Set.add a v1
            let v2 = Set.add a v2
            let s1 = SliceSet v1
            let s2 = SliceSet v2
            let result = s1.Intersect s2

            let intersectValues = Set.intersect v1 v2

            // All expected values are in result
            for x in intersectValues do
                Assert.True(result.Contains x)

            // No unexpected values are in result
            for x in result do
                Assert.True(Set.contains x intersectValues)

        [<Property>]
        let ``SliceSet Union includes all values and no others`` (v1:Set<NonEmptyString>) (v2:Set<NonEmptyString>) =
            let s1 = SliceSet v1
            let s2 = SliceSet v2
            let result = s1.Union s2

            let allValues = v1 + v2

            // All expected values are in result
            for x in allValues do
                Assert.True(result.Contains x)

            // No unexpected values are in result
            for x in result do
                Assert.True(Set.contains x allValues)

        [<Property>]
        let ``SliceSet Contains returns true for values it contains`` (v:Set<NonEmptyString>) =
            let s = SliceSet v

            // All expected values are in result
            for x in v do
                Assert.True(s.Contains x)

        [<Property>]
        let ``SliceSet Contains returns false for values it does not contain`` (v1:Set<NonEmptyString>) (v2:Set<NonEmptyString>) =
            let v1 = v1 - v2
            let s = SliceSet v1 // Have the SliceSet populated with values sometimes

            // All expected values are in result
            for x in v2 do
                Assert.True(not (s.Contains x))

        [<Property>]
        let ``SliceSet Filter returns matching results`` (v:Set<NonEmptyString>) (a:NonEmptyString) =
            let v = Set.add a v
            let s = SliceSet v
            let f = fun x -> x = a
            let result = s.Filter f
            
            let nonMatches = v |> Set.filter (f >> not)

            // All expected values are in result
            for x in result do
                Assert.True(f x)

            // All non-matches are not in result
            for x in nonMatches do
                Assert.True(not (result.Contains x))

        [<Property>]
        let ``SliceSet Minus removes correct elements`` (v1:Set<NonEmptyString>) (v2:Set<NonEmptyString>) =
            let v = v1 + v2
            let minusS = SliceSet v1
            let s = SliceSet v 
            let result = s.Minus minusS

            let expected = v - v1

            // All expected values are in result
            for x in expected do
                Assert.True(s.Contains x)

            // Non unexpected values are in result
            for x in result do
                Assert.True(Set.contains x expected)



    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SliceMapTests =
        open Flips.SliceMap

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
            let distinctV = v |> List.distinctBy fst
            let s = Map.ofList distinctV |> SMap
            let inverse = distinctV |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap.ofList
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

        [<Property>]
        let ``SMap 1 dimension filters work`` 
            (d:List<NonEmptyString * Scalar>)
            (x1:NonEmptyString) =

            let sm = d |> SMap
            let r = sm.[GreaterOrEqual x1]

            for (k1) in r.Keys do
                Assert.True(k1 >= x1)


        [<Property>]
        let ``SMap contains keys`` 
            (d:List<(NonEmptyString * Scalar)>) =

            let keys = d |> List.map fst
            let sm = d |> SMap

            for k in keys do
                Assert.True(sm.ContainsKey k)


        [<Property>]
        let ``SMap contains correct values`` 
            (d:List<(NonEmptyString * Scalar)>) =

            let valueMap = d |> Map.ofList
            let sm = d |> SMap
            
            for (k, v) in Map.toSeq valueMap do
                Assert.StrictEqual (v, sm.[k])


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SliceMap2Tests =
        open Flips.SliceMap

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
            let distinctV = v |> List.distinctBy fst
            let s = Map.ofList distinctV |> SMap2
            let inverse = distinctV |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap2.ofList
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
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap by 1/SMap then by SMap2 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString) * Scalar)>) =
            let s1 = Map.ofList v1 |> SMap2
            let v2 = v1 |> List.map (fun ((k1, k2), v) -> k1, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``SMap2 1, 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString ) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString) =

            let sm = d |> SMap2
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2]

            for (k1, k2) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap2 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString ) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString) =

            let sm = d |> SMap2
            let r = sm.[x1, GreaterOrEqual x2]

            for (k2) in r.Keys do
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap2 1 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString ) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString) =

            let sm = d |> SMap2
            let r = sm.[GreaterOrEqual x1, x2]

            for (k1) in r.Keys do
                Assert.True(k1 >= x1)


        [<Property>]
        let ``SMap2 contains keys`` 
            (d:List<((NonEmptyString * NonEmptyString) * Scalar)>) =

            let keys = d |> List.map fst
            let sm = d |> SMap2

            for k in keys do
                Assert.True(sm.ContainsKey k)


        [<Property>]
        let ``SMap2 contains correct values`` 
            (d:List<((NonEmptyString * NonEmptyString) * Scalar)>) =

            let valueMap = d |> Map.ofList
            let sm = d |> SMap2
            
            for (k, v) in Map.toSeq valueMap do
                Assert.StrictEqual (v, sm.[k])


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SliceMap3Tests =
        open Flips.SliceMap

        [<Property>]
        let ``SMap3 addition is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let s2 = Map.ofList v2 |> SMap3
            let r1 = s1 + s2
            let r2 = s2 + s1
            Assert.StrictEqual(r1, r2)
    
        [<Property>]
        let ``SMap3 addition is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let v3 = v3.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let s2 = Map.ofList v2 |> SMap3
            let s3 = Map.ofList v3 |> SMap3
            let r1 = (s1 + s2) + s3
            let r2 = s1 + (s2 + s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap3 element-wise multiplication is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let s2 = Map.ofList v2 |> SMap3
            let r1 = s1 .* s2
            let r2 = s2 .* s1
            Assert.StrictEqual(r1, r2)
    
        [<Property>]
        let ``SMap3 element-wise multiplication is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let v3 = v3.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let s2 = Map.ofList v2 |> SMap3
            let s3 = Map.ofList v3 |> SMap3
            let r1 = (s1 .* s2) .* s3
            let r2 = s1 .* (s2 .* s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``Multiply SMap3 by 1 yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap3
            let r = 1.0 * s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap3 by its inverse then itself yields original `` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap3
            let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap3.ofList
            let r = s .* inverse .* s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Adding empty SMap3 yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap3
            let empty = Map.empty |> SMap3
            let r = s + empty
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap3 by X then 1/X yields same SMap3`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let x = randomFloat rng
            let s = Map.ofList v |> SMap3
            let r1 = x * s
            let r2 = (1.0 / x) * r1
            Assert.StrictEqual(s, r2)

        [<Property>]
        let ``Multiplication of SMap3 by SMap then 1/SMap yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> k3, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Multiplication of SMap3 by SMap2 then 1/SMap2 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> (k2, k3), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap by 1/SMap then by SMap3 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> k1, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap2 by 1/SMap2 then by SMap3 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap3
            let v2 = v1 |> List.map (fun ((k1, k2, k3), v) -> (k1, k2), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)


        [<Property>]
        let ``SMap3 1, 2, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3]

            for (k1, k2, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap3 2, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3]

            for (k2, k3) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)

        [<Property>]
        let ``SMap3 1, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3]

            for (k1, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap3 1, 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3]

            for (k1, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap3 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[x1, x2, GreaterOrEqual x3]

            for (k3) in r.Keys do
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap3 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[x1, GreaterOrEqual x2, x3]

            for (k2) in r.Keys do
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap3 1 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString) =

            let sm = d |> SMap3
            let r = sm.[GreaterOrEqual x1, x2, x3]

            for (k1) in r.Keys do
                Assert.True(k1 >= x1)


        [<Property>]
        let ``SMap3 contains keys`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let keys = d |> List.map fst
            let sm = d |> SMap3

            for k in keys do
                Assert.True(sm.ContainsKey k)


        [<Property>]
        let ``SMap3 contains correct values`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let valueMap = d |> Map.ofList
            let sm = d |> SMap3
            
            for (k, v) in Map.toSeq valueMap do
                Assert.StrictEqual (v, sm.[k])


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SliceMap4Tests =
        open Flips.SliceMap

        [<Property>]
        let ``SMap4 addition is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let s2 = Map.ofList v2 |> SMap4
            let r1 = s1 + s2
            let r2 = s2 + s1
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap4 addition is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..3]
            let v2 = v2.[..3]
            let v3 = v3.[..3]
            let s1 = Map.ofList v1 |> SMap4
            let s2 = Map.ofList v2 |> SMap4
            let s3 = Map.ofList v3 |> SMap4
            let r1 = (s1 + s2) + s3
            let r2 = s1 + (s2 + s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap4 element-wise multiplication is commutative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let s2 = Map.ofList v2 |> SMap4
            let r1 = s1 .* s2
            let r2 = s2 .* s1
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap4 element-wise multiplication is associative`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let v2 = v2.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let s2 = Map.ofList v2 |> SMap4
            let s3 = Map.ofList v3 |> SMap4
            let r1 = (s1 .* s2) .* s3
            let r2 = s1 .* (s2 .* s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``Multiply SMap4 by 1 yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap4
            let r = 1.0 * s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap4 by its inverse then itself yields original `` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap4
            let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap4.ofList
            let r = s .* inverse .* s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Adding empty SMap4 yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let s = Map.ofList v |> SMap4
            let empty = Map.empty |> SMap4
            let r = s + empty
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap4 by X then 1/X yields same SMap4`` (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..9]
            let x = randomFloat rng
            let s = Map.ofList v |> SMap4
            let r1 = x * s
            let r2 = (1.0 / x) * r1
            Assert.StrictEqual(s, r2)

        [<Property>]
        let ``Multiplication of SMap4 by SMap then 1/SMap yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> k4, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Multiplication of SMap4 by SMap2 then 1/SMap2 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Multiplication of SMap4 by SMap3 then 1/SMap3 yields initial`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k2, k3, k4), v) |> List.distinctBy fst
            let s2 = v2 |> SMap3.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap3.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap by 1/SMap then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> k1, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap2 by 1/SMap2 then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k1, k2), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap3 by 1/SMap3 then by SMap4 yields initial SMap2`` (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..9]
            let s1 = Map.ofList v1 |> SMap4
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k3), v) |> List.distinctBy fst
            let s2 = v2 |> SMap3.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap3.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``SMap4 1, 2, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4]

            for (k1, k2, k3, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 2, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4]

            for (k2, k3, k4) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 1, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3, GreaterOrEqual x4]

            for (k1, k3, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 1, 2, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, GreaterOrEqual x4]

            for (k1, k2, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 1, 2, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, x4]

            for (k1, k2, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap4 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, x2, GreaterOrEqual x3, GreaterOrEqual x4]

            for (k3, k4) in r.Keys do
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 2, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, GreaterOrEqual x2, x3, GreaterOrEqual x4]

            for (k2, k4) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 2, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3, x4]

            for (k2, k3) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap4 1, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3, x4]

            for (k1, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap4 1, 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, x4]

            for (k1, k2) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap4 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, x2, x3, GreaterOrEqual x4]

            for (k4) in r.Keys do
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap4 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, x2, GreaterOrEqual x3, x4]

            for (k3) in r.Keys do
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap4 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[x1, GreaterOrEqual x2, x3, x4]

            for (k2) in r.Keys do
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap4 1 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString) =

            let sm = d |> SMap4
            let r = sm.[GreaterOrEqual x1, x2, x3, x4]

            for (k1) in r.Keys do
                Assert.True(k1 >= x1)


        [<Property>]
        let ``SMap4 contains keys`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let keys = d |> List.map fst
            let sm = d |> SMap4

            for k in keys do
                Assert.True(sm.ContainsKey k)


        [<Property>]
        let ``SMap4 contains correct values`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let valueMap = d |> Map.ofList
            let sm = d |> SMap4
            
            for (k, v) in Map.toSeq valueMap do
                Assert.StrictEqual (v, sm.[k])


    [<Properties(Arbitrary = [| typeof<Types> |] )>]
    module SliceMap5Tests =
        open Flips.SliceMap

        [<Property>]
        let ``SMap5 addition is commutative`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..3]
            let v2 = v2.[..3]
            let s1 = Map.ofList v1 |> SMap5
            let s2 = Map.ofList v2 |> SMap5
            let r1 = s1 + s2
            let r2 = s2 + s1
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap5 addition is associative`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..2]
            let v2 = v2.[..2]
            let v3 = v3.[..2]
            let s1 = Map.ofList v1 |> SMap5
            let s2 = Map.ofList v2 |> SMap5
            let s3 = Map.ofList v3 |> SMap5
            let r1 = (s1 + s2) + s3
            let r2 = s1 + (s2 + s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap5 element-wise multiplication is commutative`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let v2 = v2.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let s2 = Map.ofList v2 |> SMap5
            let r1 = s1 .* s2
            let r2 = s2 .* s1
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``SMap5 element-wise multiplication is associative`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v2:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) 
            (v3:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let v2 = v2.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let s2 = Map.ofList v2 |> SMap5
            let s3 = Map.ofList v3 |> SMap5
            let r1 = (s1 .* s2) .* s3
            let r2 = s1 .* (s2 .* s3)
            Assert.StrictEqual(r1, r2)

        [<Property>]
        let ``Multiply SMap5 by 1 yields same SMap5`` 
            (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..5]
            let s = Map.ofList v |> SMap5
            let r = 1.0 * s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap5 by its inverse then itself yields original `` 
            (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..5]
            let s = Map.ofList v |> SMap5
            let inverse = v |> List.map (fun (k, v) -> k, 1.0 / v) |> SMap5.ofList
            let r = s .* inverse .* s
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Adding empty SMap4 yields same SMap5`` 
            (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..5]
            let s = Map.ofList v |> SMap5
            let empty = Map.empty |> SMap5
            let r = s + empty
            Assert.StrictEqual(s, r)

        [<Property>]
        let ``Multiply SMap5 by X then 1/X yields same SMap5`` 
            (v:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)=
            let v = v.[..5]
            let x = randomFloat rng
            let s = Map.ofList v |> SMap5
            let r1 = x * s
            let r2 = (1.0 / x) * r1
            Assert.StrictEqual(s, r2)

        [<Property>]
        let ``Hadamard-Product of SMap5 by SMap then 1/SMap yields initial`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> k5, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap5 by SMap2 then 1/SMap2 yields initial`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k4, k5), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap5 by SMap3 then 1/SMap3 yields initial`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k4, k5), v) |> List.distinctBy fst
            let s2 = v2 |> SMap3.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap3.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap5 by SMap4 then 1/SMap4 yields initial`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k4, k5), v) |> List.distinctBy fst
            let s2 = v2 |> SMap4.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap4.ofList
            let r = s1 .* s2 .* s2Inverse
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap by 1/SMap then by SMap5 yields initial SMap5`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> k1, v) |> List.distinctBy fst
            let s2 = v2 |> SMap.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap2 by 1/SMap2 then by SMap5 yields initial SMap5`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2), v) |> List.distinctBy fst
            let s2 = v2 |> SMap2.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap2.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap3 by 1/SMap3 then by SMap5 yields initial SMap5`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3), v) |> List.distinctBy fst
            let s2 = v2 |> SMap3.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap3.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``Hadamard-Product of SMap4 by 1/SMap4 then by SMap5 yields initial SMap5`` 
            (v1:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =
            let v1 = v1.[..5]
            let s1 = Map.ofList v1 |> SMap5
            let v2 = v1 |> List.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k4), v) |> List.distinctBy fst
            let s2 = v2 |> SMap4.ofList
            let s2Inverse = v2 |> List.map (fun (idx, x) -> idx, (Value 1.0) / x) |> SMap4.ofList
            let r = s2 .* (s2Inverse .* s1)
            Assert.StrictEqual(r, s1)

        [<Property>]
        let ``SMap5 1, 2, 3, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k1, k2, k3, k4, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        
        [<Property>]
        let ``SMap5 2, 3, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k2, k3, k4, k5) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        
        [<Property>]
        let ``SMap5 1, 3, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k1, k3, k4, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        [<Property>]
        let ``SMap5 1, 2, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k1, k2, k4, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 1, 2, 3, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, x4, GreaterOrEqual x5]

            for (k1, k2, k3, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k5 >= x5)

        
        [<Property>]
        let ``SMap5 1, 2, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4, x5]

            for (k1, k2, k3, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap5 3, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, GreaterOrEqual x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k3, k4, k5) in r.Keys do
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        [<Property>]
        let ``SMap5 2, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, GreaterOrEqual x2, x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k2, k4, k5) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 2, 3, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3, x4, GreaterOrEqual x5]

            for (k2, k3, k5) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 2, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, GreaterOrEqual x2, GreaterOrEqual x3, GreaterOrEqual x4, x5]

            for (k2, k3, k4) in r.Keys do
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)

        [<Property>]
        let ``SMap5 1, 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k1, k4, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        [<Property>]
        let ``SMap5 1, 3, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3, x4, GreaterOrEqual x5]

            for (k1, k3, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 1, 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, GreaterOrEqual x3, GreaterOrEqual x4, x5]

            for (k1, k3, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap5 1, 2, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, x4, GreaterOrEqual x5]

            for (k1, k2, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 1, 2, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, GreaterOrEqual x4, x5]

            for (k1, k2, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap5 1, 2, 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, GreaterOrEqual x3, x4, x5]

            for (k1, k2, k3) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)
                Assert.True(k3 >= x3)


        [<Property>]
        let ``SMap5 4, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, x3, GreaterOrEqual x4, GreaterOrEqual x5]

            for (k4, k5) in r.Keys do
                Assert.True(k4 >= x4)
                Assert.True(k5 >= x5)

        [<Property>]
        let ``SMap5 3, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, GreaterOrEqual x3, x4, GreaterOrEqual x5]

            for (k3, k5) in r.Keys do
                Assert.True(k3 >= x3)
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 3, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, GreaterOrEqual x3, GreaterOrEqual x4, x5]

            for (k3, k4) in r.Keys do
                Assert.True(k3 >= x3)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap5 1, 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, x3, x4, GreaterOrEqual x5]

            for (k1, k5) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k5 >= x5)

        [<Property>]
        let ``SMap5 1, 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, x3, GreaterOrEqual x4, x5]

            for (k1, k4) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k4 >= x4)


        [<Property>]
        let ``SMap5 1, 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, GreaterOrEqual x2, x3, x4, x5]

            for (k1, k2) in r.Keys do
                Assert.True(k1 >= x1)
                Assert.True(k2 >= x2)

        [<Property>]
        let ``SMap5 5 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, x3, x4, GreaterOrEqual x5]

            for (k5) in r.Keys do
                Assert.True(k5 >= x5)


        [<Property>]
        let ``SMap5 4 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, x3, GreaterOrEqual x4, x5]

            for (k4) in r.Keys do
                Assert.True(k4 >= x4)

        [<Property>]
        let ``SMap5 3 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, x2, GreaterOrEqual x3, x4, x5]

            for (k3) in r.Keys do
                Assert.True(k3 >= x3)

        [<Property>]
        let ``SMap5 2 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[x1, GreaterOrEqual x2, x3, x4, x5]

            for (k2) in r.Keys do
                Assert.True(k2 >= x2)


        [<Property>]
        let ``SMap5 1 dimension filters work`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>)
            (x1:NonEmptyString)
            (x2:NonEmptyString)
            (x3:NonEmptyString)
            (x4:NonEmptyString)
            (x5:NonEmptyString) =

            let sm = d |> SMap5
            let r = sm.[GreaterOrEqual x1, x2, x3, x4, x5]

            for (k1) in r.Keys do
                Assert.True(k1 >= x1)


        [<Property>]
        let ``SMap5 contains keys`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let keys = d |> List.map fst
            let sm = d |> SMap5

            for k in keys do
                Assert.True(sm.ContainsKey k)


        [<Property>]
        let ``SMap5 contains correct values`` 
            (d:List<((NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString * NonEmptyString) * Scalar)>) =

            let valueMap = d |> Map.ofList
            let sm = d |> SMap5
            
            for (k, v) in Map.toSeq valueMap do
                Assert.StrictEqual (v, sm.[k])



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