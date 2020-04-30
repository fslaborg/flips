module Flips.Domain

[<CustomEquality; CustomComparison>]
type Scalar = Scalar of float with

    static member private nearlyEquals (Scalar x:Scalar) (Scalar y:Scalar) =
        let epsilon = 0.00001
        let absX = System.Math.Abs x
        let absY = System.Math.Abs y
        let diff = System.Math.Abs (absX - absY)

        if x = y then
            true
        elif x = 0.0 || y = 0.0 || (absX + absY < System.Double.MinValue) then
            diff < (epsilon * System.Double.MinValue)
        else
            diff / (System.Math.Min((absX + absY), System.Double.MaxValue)) < epsilon

    static member (+) (Scalar lhs:Scalar, Scalar rhs:Scalar) =
        Scalar (lhs + rhs)

    static member (*) (Scalar lhs:Scalar, Scalar rhs:Scalar) =
        Scalar (lhs * rhs)

    static member Zero = Scalar 0.0

    override this.GetHashCode () =
        let (Scalar v) = this
        hash v

    override this.Equals(obj) =
        match obj with
        | :? Scalar as s -> Scalar.nearlyEquals this s 
        | _ -> false

    interface System.IComparable with
        member this.CompareTo yObj =
            match yObj with
            | :? Scalar as s -> compare this s
            | _ -> invalidArg "yObj" "Cannot compare values of different types"

type DecisionType =
    | Boolean
    | Integer of LowerBound:int64 * UpperBound:int64
    | Continuous of LowerBound:decimal * UpperBound:decimal

type DecisionName = DecisionName of string

type Decision = {
    Name : DecisionName
    Type : DecisionType
}
with
    static member (*) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision + scalar
    
    static member (*) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfFloat scalar + decision

    static member (+) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfFloat scalar + decision

    static member (+) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision + scalar

    static member (+) (decision:Decision, rightDecision:Decision) =
        LinearExpression.OfDecision decision + rightDecision

    static member (+) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision + expr

    static member (<==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision <== scalar

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (<==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision <== expr

    static member (==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision == scalar

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision == expr

    static member (>==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision >== scalar

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision >== rhsDecision

    static member (>==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision >== expr

and LinearExpression = 
    | LinearExpression of names:Set<DecisionName> * coefs:Map<DecisionName, Scalar> * decs:Map<DecisionName, Decision> * offset:Scalar
with

    static member OfFloat (f:Scalar) =
        LinearExpression (Set.empty, Map.empty, Map.empty, f)

    static member OfDecision (d:Decision) =
        let names = Set.ofList [d.Name]
        let coefs = Map.ofList [d.Name, Scalar 1.0]
        let decs = Map.ofList [d.Name, d]
        LinearExpression (names, coefs, decs, Scalar 0.0)

    static member GetDecisions (LinearExpression (names, coefs, decs, offset):LinearExpression) =
        decs
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    static member Zero =
        LinearExpression (Set.empty, Map.empty, Map.empty, Scalar.Zero)

    static member (+) (LinearExpression (names, coefs, decs, offset):LinearExpression, scalar:Scalar) =
        LinearExpression (names, coefs, decs, offset + scalar)

    static member (+) (scalar:Scalar, expr:LinearExpression) =
        expr + scalar

    static member (+) (LinearExpression (names, coefs, decs, offset):LinearExpression, decision:Decision) =
        if Set.contains decision.Name names then
            if decs.[decision.Name].Type <> decision.Type then
                let (DecisionName name) = decision.Name
                invalidArg name "Mistmatched DecisionType"

            let newCoefs = Map.add decision.Name (coefs.[decision.Name] + (Scalar 1.0)) coefs
            LinearExpression (names, newCoefs, decs, offset)
        else
            let newNames = Set.add decision.Name names
            let newCoefs = Map.add decision.Name (Scalar 1.0) coefs
            let newDecs = Map.add decision.Name decision decs
            LinearExpression (newNames, newCoefs, newDecs, offset)

    static member private Merge (LinearExpression (lNames, lCoefs, lDecs, lOffset):LinearExpression, LinearExpression (rNames, rCoefs, rDecs, rOffset):LinearExpression) =
        // Assume the Left LinearExpression is larget than the right
        let nameOverlap = Set.intersect lNames rNames
        
        for n in nameOverlap do
            if lDecs.[n].Type <> rDecs.[n].Type then
                let (DecisionName name) = n
                invalidArg name "Cannot have mismatched DecisionTypes for same DecisionName"

        let newNames = lNames + rNames

        let newDecs = (lDecs, (rNames - lNames)) ||> Set.fold (fun m k -> Map.add k rDecs.[k] m)

        let newCoefs =
            (lCoefs, nameOverlap)
            ||> Set.fold (fun m k -> Map.add k (lCoefs.[k] + rCoefs.[k]) m)
            |> fun updatedCoefs -> Set.fold (fun m n -> Map.add n rCoefs.[n] m) updatedCoefs (rNames - lNames)

        LinearExpression (newNames, newCoefs, newDecs, lOffset + rOffset)

    static member (+) (lExpr:LinearExpression, rExpr:LinearExpression) =
        let (LinearExpression (lNames, _, _, _)) = lExpr
        let (LinearExpression (rNames, _, _, _)) = rExpr
        let lSize = Set.count lNames
        let rSize = Set.count rNames

        if lSize > rSize then
            LinearExpression.Merge (lExpr, rExpr)
        else
            LinearExpression.Merge (rExpr, lExpr)

    static member (*) (LinearExpression (names, coefs, decs, offset):LinearExpression, scalar:Scalar) =
        let newCoefs = Map.map (fun k v -> v * scalar) coefs
        LinearExpression (names, newCoefs, decs, offset * scalar)

    static member (*) (scalar:Scalar, expr:LinearExpression) =
        expr * scalar

    static member (<==) (lhs:LinearExpression, rhs:Scalar) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

    static member (<==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        Inequality (lhs, LessOrEqual, rhs)

    static member (==) (lhs:LinearExpression, rhs:Scalar) =
        Equality (lhs, LinearExpression.OfFloat rhs)

    static member (==) (lhs:LinearExpression, rhs:Decision) =
        Equality (lhs, LinearExpression.OfDecision rhs)

    static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        Equality (lhs, rhs)

    static member (>==) (lhs:LinearExpression, rhs:Scalar) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)

    static member (>==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

    static member (>==) (lhs:LinearExpression, rhs:LinearExpression) =
        Inequality (lhs, GreaterOrEqual, rhs)


and Inequality =
    | LessOrEqual
    | GreaterOrEqual

and ConstraintExpression = 
    | Inequality of LHS:LinearExpression * Inequality * RHS:LinearExpression
    | Equality of LHS:LinearExpression * RHS:LinearExpression

type ConstraintName = ConstraintName of string

type Constraint = {
    Name : ConstraintName
    Expression : ConstraintExpression
}

type ObjectiveSense =
    | Minimize
    | Maximize

type ObjectiveName = ObjectiveName of string

type Objective = {
    Name : ObjectiveName
    Sense : ObjectiveSense
    Expression : LinearExpression
}


module Decision =

    let createBoolean decisionName =
        if System.String.IsNullOrEmpty(decisionName) then
            invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Boolean
        }

    let createInteger decisionName lowerBound upperBound =
        if System.String.IsNullOrEmpty(decisionName) then
                invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        if lowerBound > upperBound then
            invalidArg "LowerBound" "Cannot create Decision where LowerBound is greater than UpperBound"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Integer (lowerBound, upperBound)
        }

    let createContinuous decisionName lowerBound upperBound =
        if System.String.IsNullOrEmpty(decisionName) then
                invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        if lowerBound > upperBound then
            invalidArg "LowerBound" "Cannot create Decision where LowerBound is greater than UpperBound"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Continuous (lowerBound, upperBound)
        }    

module Constraint =

    let getDecisions (c:Constraint) =
        let (lhs, rhs) =
            match c.Expression with
            | Inequality (lhs, c, rhs) -> lhs, rhs
            | Equality (lhs, rhs) -> lhs, rhs
        let lhsDecisions = LinearExpression.GetDecisions lhs
        let rhsDecisions = LinearExpression.GetDecisions rhs
        lhsDecisions + rhsDecisions

    let create (constraintName:string) (cExpr:ConstraintExpression) =
        if System.String.IsNullOrEmpty(constraintName) then
            invalidArg "ConstraintName" "Cannot have Name of Constraint that is null or empty"
        {
            Name = ConstraintName constraintName
            Expression = cExpr
        }


module Objective =

    let create objectiveName sense expression =
        if System.String.IsNullOrEmpty(objectiveName) then
            invalidArg "ObjectiveName" "Cannot have Name of Decision that is null or empty"
        {
            Name = ObjectiveName objectiveName
            Sense = sense
            Expression = expression
        }


module Model =

    type Model = private {
        _Objective : Objective
        _Constraints : List<Constraint>
        _Decisions : Map<DecisionName, Decision>
    } 
    with
        member public this.Objective = this._Objective
        member public this.Constraints = this._Constraints
        member public this.Decisions = this._Decisions

    let private getMismatchedDecisionTypesInSet(decisions:Set<Decision>) =
        decisions
        |> Set.toList
        |> List.map (fun x -> x.Name, x.Type)
        |> List.groupBy fst
        |> List.map (fun (k, g) -> k, g |> Seq.map snd |> Set.ofSeq |> Set.count)
        |> List.filter (fun (k, c) -> c > 1)

    let private existingDecisions (decisionMap:Map<DecisionName,Decision>) (decisions:Set<Decision>) =
        decisions
        |> Set.filter (fun n -> Map.containsKey n.Name decisionMap)

    let private newDecisions (decisionMap:Map<DecisionName,Decision>) (decisions:Set<Decision>) =
        decisions - (existingDecisions decisionMap decisions)
        |> Set.toList

    let private getMismatchedDecisionTypes (decisionMap:Map<DecisionName,Decision>) (decisions:Set<Decision>) =
        existingDecisions decisionMap decisions
        |> Set.filter (fun x -> decisionMap.[x.Name].Type <> x.Type)

    let private addToDecisionMap (decision:Decision) (decisionMap:Map<DecisionName, Decision>) =
        Map.add decision.Name decision decisionMap

    let create objective =
        let objectiveDecisions = LinearExpression.GetDecisions objective.Expression
        let mismatchedDecisionsInObjective = getMismatchedDecisionTypesInSet objectiveDecisions

        if not (List.isEmpty mismatchedDecisionsInObjective) then
            failwith "Cannot have mismatched DecisionTypes for same DecisionName"

        let decisions = 
            objectiveDecisions 
            |> Set.toList 
            |> List.map (fun x -> x.Name, x) 
            |> Map.ofList

        {
            _Objective = objective
            _Constraints = []
            _Decisions = decisions
        }

    let addConstraint c (model:Model) =
        let decisions = Constraint.getDecisions c

        let mismatchedDecisions = getMismatchedDecisionTypes model.Decisions decisions
        if not (Set.isEmpty mismatchedDecisions) then
            // TODO Make this error more informative
            failwith "Cannot have mismatched DecisionTypes for same DecisionName"

        let newDecisions = newDecisions model.Decisions decisions
        let newDecisionMap = (model.Decisions, newDecisions) ||> List.fold (fun m d -> Map.add d.Name d m )

        { model with _Constraints = [c] @ model.Constraints; _Decisions = newDecisionMap }

    let addConstraints constraints model =
        (constraints |> Seq.map addConstraint |> Seq.reduce (>>)) model


type Solution = {
    DecisionResults : Map<DecisionName,float>
    ObjectiveResult : float
}

type SolverType = | CBC


type SolverSettings = {
    SolverType : SolverType
    MaxDuration : int64
    WriteLPFile : Option<string>
}

type SolveResult =
    | Optimal of Solution
    | Suboptimal of string


type ConstraintBuilder (constraintSetPrefix:string) =

    let isTuple t = t.GetType() |> Reflection.FSharpType.IsTuple

    let getFields (t:obj) = t |> Reflection.FSharpValue.GetTupleFields |> Array.toList

    let rec flattenFields f =
        f
        |> List.collect(
            fun t ->
                if isTuple t then
                    flattenFields (getFields t)
                else
                    [t]
        )

    let tupleToObjectList (t:obj) : List<obj> =
        if isTuple t then
            t |> getFields |> flattenFields
        else
            [t]

    let constraintNamer (indices:obj) : string =
        tupleToObjectList indices
        |> List.map (sprintf "%O")
        |> String.concat "_"
        |> (sprintf "%s|%s" constraintSetPrefix)

    member this.Yield (cExpr:ConstraintExpression) =
        cExpr

    member this.For(source:seq<'a>, body:'a -> seq<'b * ConstraintExpression>) =
        source
        |> Seq.collect (fun x -> body x |> Seq.map (fun (idx, expr) -> (x, idx), expr))

    member this.For(source:seq<'a>, body:'a -> ConstraintExpression) =
        source |> Seq.map (fun x -> x, body x)

    member this.Run(source:seq<'a * ConstraintExpression>) =
        source |> Seq.map (fun (n, c) -> Constraint.create (constraintNamer n) c)