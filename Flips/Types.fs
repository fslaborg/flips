namespace Flips.Types


type DecisionType =
    | Boolean
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float

type DecisionName = DecisionName of string

type Decision = {
    Name : DecisionName
    Type : DecisionType
}
with

    static member (*) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision * f

    static member (*) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision * f

    static member (+) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + rhsDecision

    static member (+) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision + f

    static member (+) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision + f

    static member (-) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + (-1.0 * rhsDecision)

    static member (-) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision - f

    static member (-) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision - f

    static member (<==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision <== f

    static member (<==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f <== decision

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision == f

    static member (==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f  == decision

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (>==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision >== f

    static member (>==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f >== decision

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision >== rhsDecision


and LinearExpression (names:Set<DecisionName>, coefficients : Map<DecisionName, float>, decisions : Map<DecisionName, Decision>, offset:float) =
    member this.Names = names
    member this.Coefficients = coefficients
    member this.Decisions = decisions
    member this.Offset = offset

    static member private Equivalent (lExpr:LinearExpression) (rExpr:LinearExpression) =
        let isEqualOffset = (lExpr.Offset = rExpr.Offset)
        let leftOnlyNames = lExpr.Names - rExpr.Names
        let rightOnlyNames = rExpr.Names - lExpr.Names
        let overlapNames = Set.intersect lExpr.Names rExpr.Names

        let leftOnlyNamesAreZero = 
            leftOnlyNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = 0.0)

        let rightOnlyNamesAreZero =
            rightOnlyNames
            |> Set.forall (fun n -> rExpr.Coefficients.[n] = 0.0)

        let overlapNamesMatch =
            overlapNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = rExpr.Coefficients.[n])

        isEqualOffset && leftOnlyNamesAreZero && rightOnlyNamesAreZero && overlapNamesMatch

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as expr -> LinearExpression.Equivalent this expr
        | _ -> false

    static member OfFloat (f:float) =
        LinearExpression (Set.empty, Map.empty, Map.empty, 0.0)

    static member OfDecision (d:Decision) =
        let names = Set.ofList [d.Name]
        let coefs = Map.ofList [d.Name, 1.0]
        let decs = Map.ofList [d.Name, d]
        LinearExpression (names, coefs, decs, 0.0)

    static member internal GetDecisions (expr:LinearExpression) =
        expr.Decisions
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    static member Zero =
        LinearExpression (Set.empty, Map.empty, Map.empty, 0.0)

    static member private Merge (l:LinearExpression, r:LinearExpression) =
        // Assume the Left LinearExpression is larget than the right
        let nameOverlap = Set.intersect l.Names r.Names
    
        for n in nameOverlap do
            if l.Decisions.[n].Type <> r.Decisions.[n].Type then
                let (DecisionName name) = n
                invalidArg name "Cannot have mismatched DecisionTypes for same DecisionName"

        let newNames = l.Names + r.Names

        let newDecs = (l.Decisions, (r.Names - l.Names)) ||> Set.fold (fun m k -> Map.add k r.Decisions.[k] m)

        let newCoefs =
            (l.Coefficients, nameOverlap)
            ||> Set.fold (fun m k -> Map.add k (l.Coefficients.[k] + r.Coefficients.[k]) m)
            |> fun updatedCoefs -> Set.fold (fun m n -> Map.add n r.Coefficients.[n] m) updatedCoefs (r.Names - l.Names)

        LinearExpression (newNames, newCoefs, newDecs, l.Offset + r.Offset)

    static member (+) (l:LinearExpression, r:LinearExpression) =
        let lSize = Set.count l.Names
        let rSize = Set.count r.Names

        if lSize > rSize then
            LinearExpression.Merge (l, r)
        else
            LinearExpression.Merge (r, l)

    static member (+) (expr:LinearExpression, f:float) =
        expr + (LinearExpression.OfFloat f)

    static member (+) (f:float, expr:LinearExpression) =
        (LinearExpression.OfFloat f) + expr

    static member (+) (expr:LinearExpression, decision:Decision) =
        expr + (LinearExpression.OfDecision decision)

    static member (+) (decision:Decision, expr:LinearExpression) =
        (LinearExpression.OfDecision decision) + expr

    static member (*) (expr:LinearExpression, f:float) =
        let newCoefs = Map.map (fun k v -> v * f) expr.Coefficients
        LinearExpression (expr.Names, newCoefs, expr.Decisions, expr.Offset * f)

    static member (*) (f:float, expr:LinearExpression) =
        expr * f

    static member (-) (lExpr:LinearExpression, rExpr:LinearExpression) =
        lExpr + (-1.0 * rExpr)

    static member (-) (expr:LinearExpression, f:float) =
        expr - (LinearExpression.OfFloat f)

    static member (-) (f:float, expr:LinearExpression) =
        (LinearExpression.OfFloat f) - expr

    static member (-) (expr:LinearExpression, d:Decision) =
        expr - (LinearExpression.OfDecision d)

    static member (-) (d:Decision, expr:LinearExpression) =
        (LinearExpression.OfDecision d) - expr

    static member (<==) (lhs:LinearExpression, rhs:float) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

    static member (<==) (lhs:float, rhs:LinearExpression) =
        Inequality (LinearExpression.OfFloat lhs, LessOrEqual, rhs)

    static member (<==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

    static member (<==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision <== expr

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        Inequality (lhs, LessOrEqual, rhs)

    static member (==) (lhs:LinearExpression, rhs:float) =
        Equality (lhs, LinearExpression.OfFloat rhs)

    static member (==) (lhs:float, rhs:LinearExpression) =
        Equality (LinearExpression.OfFloat lhs, rhs)

    static member (==) (lhs:LinearExpression, rhs:Decision) =
        Equality (lhs, LinearExpression.OfDecision rhs)

    static member (==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision == expr

    static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        Equality (lhs, rhs)

    static member (>==) (lhs:LinearExpression, rhs:float) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)

    static member (>==) (lhs:float, rhs:LinearExpression) =
        Inequality (LinearExpression.OfFloat lhs, GreaterOrEqual, rhs)

    static member (>==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

    static member (>==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision >== expr

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

type Solution = {
    DecisionResults : Map<Decision,float>
    ObjectiveResult : float
}

type SolverType = 
    | CBC
    | GLOP
    | Cplex128
    | Gurobi900


type SolverSettings = {
    SolverType : SolverType
    MaxDuration : int64
    WriteLPFile : Option<string>
}

type SolveResult =
    | Optimal of Solution
    | Suboptimal of string
