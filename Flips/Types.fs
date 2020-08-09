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

    static member (+) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision + f

    static member (+) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision + f

    static member (+) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + rhsDecision

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


and [<NoComparison>][<CustomEquality>] 
    ReducedLinearExpression =
    {
        DecisionTypes : Map<DecisionName, DecisionType>
        Coefficients : Map<DecisionName, float>
        Offset : float
    } with
    static member private NearlyEquals (a:float) (b:float) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        let result = 
            if (aValue >>> 63) <> (bValue >>> 63) then
                a = b
            else
                System.Math.Abs(aValue - bValue) <= 10_000L
        result

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? ReducedLinearExpression as otherExpr ->
            let offsetSame = ReducedLinearExpression.NearlyEquals this.Offset otherExpr.Offset

            let leftMatchesRight =
                (true, this.Coefficients)
                ||> Map.fold (fun b k thisCoef -> 
                                match Map.tryFind k otherExpr.Coefficients with
                                | Some otherCoef -> b && (ReducedLinearExpression.NearlyEquals thisCoef otherCoef)
                                | None -> b && (ReducedLinearExpression.NearlyEquals thisCoef 0.0))

            let evaluateRightElement b n otherCoef =
                if this.Coefficients.ContainsKey(n) then
                    b
                else
                    let essentiallyZero = ReducedLinearExpression.NearlyEquals otherCoef 0.0
                    b && essentiallyZero

            let rightNonMatchesAreZero =
                (true, otherExpr.Coefficients)
                ||> Map.fold evaluateRightElement

            let allPassing = offsetSame && leftMatchesRight && rightNonMatchesAreZero
            allPassing
        | _ -> false

and [<NoComparison>][<CustomEquality>] LinearExpression =
    | Empty
    | AddFloat of float * LinearExpression
    | AddDecision of (float * Decision) * LinearExpression
    | Multiply of float * LinearExpression
    | AddLinearExpression of LinearExpression * LinearExpression

    static member Reduce (expr:LinearExpression) : ReducedLinearExpression =
        let initialState = {
            DecisionTypes = Map.empty
            Coefficients = Map.empty
            Offset = 0.0
        }

        let rec evaluateNode (multiplier:float, state:ReducedLinearExpression) (node:LinearExpression) : float * ReducedLinearExpression =
            match node with
            | Empty -> multiplier, state
            | AddFloat (addToOffset, nodeExpr) -> 
                let newState = {state with Offset = multiplier * addToOffset + state.Offset}
                evaluateNode (multiplier, newState) nodeExpr
            | AddDecision ((nodeCoef , nodeDecision), nodeExpr) ->
                match Map.tryFind nodeDecision.Name state.DecisionTypes with
                | Some existingType ->
                    if existingType <> nodeDecision.Type then
                        invalidArg "DecisionType" "Cannot have different DecisionType for same DecisionName"
                    else
                        let newCoefficients = Map.add nodeDecision.Name (state.Coefficients.[nodeDecision.Name] + nodeCoef * multiplier) state.Coefficients
                        let newState = {state with Coefficients = newCoefficients}
                        evaluateNode (multiplier, newState) nodeExpr
                | None ->
                    let newDecisionTypes = Map.add nodeDecision.Name nodeDecision.Type state.DecisionTypes
                    let newCoefficient = Map.add nodeDecision.Name (multiplier * nodeCoef) state.Coefficients
                    let newState = {state with DecisionTypes = newDecisionTypes; Coefficients = newCoefficient}
                    evaluateNode (multiplier, newState) nodeExpr
            | Multiply (nodeMultiplier, nodeExpr) ->
                let newMultiplier = multiplier * nodeMultiplier
                evaluateNode (newMultiplier, state) nodeExpr
            | AddLinearExpression (lExpr, rExpr) ->
                let (_, leftState) = evaluateNode (multiplier, state) lExpr
                let (_,rightState) = evaluateNode (multiplier, leftState) rExpr
                multiplier, rightState

        let (_,reducedExpr) = evaluateNode (1.0, initialState) expr

        reducedExpr

    static member internal GetDecisions (expr:LinearExpression) : Set<Decision> =

        let rec evaluateNode (decisions:Set<Decision>) (node:LinearExpression) : Set<Decision> =
            match node with
            | Empty -> decisions
            | AddFloat (_, nodeExpr) -> evaluateNode decisions nodeExpr
            | Multiply (_, nodeExpr) -> evaluateNode decisions nodeExpr
            | AddDecision ((_, nodeDecision), nodeExpr) ->
                let newDecisions = decisions.Add nodeDecision
                evaluateNode newDecisions nodeExpr
            | AddLinearExpression (lExpr, rExpr) ->
                let leftDecisions = evaluateNode decisions lExpr
                let rightDecisions = evaluateNode leftDecisions rExpr
                rightDecisions

        evaluateNode (Set.empty) expr


    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as otherExpr -> 
            let thisReduced = LinearExpression.Reduce this
            let otherReduced = LinearExpression.Reduce otherExpr
            thisReduced = otherReduced
        | _ -> false

    static member Zero =
        LinearExpression.Empty

    static member (+) (l:LinearExpression, r:LinearExpression) =
        LinearExpression.AddLinearExpression (l, r)

    static member (+) (expr:LinearExpression, f:float) =
        LinearExpression.AddFloat (f, expr)

    static member (+) (f:float, expr:LinearExpression) =
        expr + f

    static member (+) (expr:LinearExpression, decision:Decision) =
        LinearExpression.AddDecision ((1.0, decision), expr)
    
    static member (+) (decision:Decision, expr:LinearExpression) =
        expr + decision

    static member (*) (expr:LinearExpression, f:float) =
        LinearExpression.Multiply (f, expr)

    static member (*) (f:float, expr:LinearExpression) =
        expr * f

    static member (-) (expr:LinearExpression, f:float) =
        expr + (-1.0 * f)

    static member (-) (f:float, expr:LinearExpression) =
        f + (-1.0 * expr)

    static member (-) (expr:LinearExpression, d:Decision) =
        expr + (-1.0 * d)

    static member (-) (d:Decision, expr:LinearExpression) =
        d + (-1.0 * expr)

    static member (-) (lExpr:LinearExpression, rExpr:LinearExpression) =
        lExpr + (-1.0 * rExpr)

    static member OfFloat (f:float) =
        LinearExpression.AddFloat(f, LinearExpression.Zero)

    static member OfDecision (d:Decision) =
        LinearExpression.Zero + d

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