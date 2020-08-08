namespace Flips.Types


[<CustomEquality; CustomComparison>]
type Scalar = Value of float with

    static member private NearlyEquals (Value a:Scalar) (Value b:Scalar) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        if (aValue >>> 63) <> (bValue >>> 63) then
            a = b
        else
            System.Math.Abs(aValue - bValue) <= 10_000L

    static member (+) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs + rhs)

    static member (+) (Value s:Scalar, f:float) =
        Value (s + f)

    static member (+) (f:float, Value s:Scalar) =
        Value (s + f)

    static member (*) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs * rhs)

    static member (*) (Value s:Scalar, f:float) =
        Value (s * f)

    static member (*) (f:float, Value s:Scalar) =
        Value (s * f)

    static member (-) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs - rhs)

    static member (-) (Value s:Scalar, f:float) =
        Value (s - f)

    static member (-) (f:float, Value s:Scalar) =
        Value (f - s)

    static member (/) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs / rhs)

    static member (/) (f:float, Value s:Scalar) =
        Value (f / s)

    static member (/) (Value s:Scalar, f:float) =
        Value (s / f)

    static member Zero = Value 0.0

    override this.GetHashCode () =
        let (Value v) = this
        hash v

    override this.Equals(obj) =
        match obj with
        | :? Scalar as s -> Scalar.NearlyEquals this s 
        | _ -> false

    interface System.IComparable with
        member this.CompareTo yObj =
            match yObj with
            | :? Scalar as s -> compare this s
            | _ -> invalidArg "yObj" "Cannot compare values of different types"

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

    static member (*) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision * scalar
    
    static member (*) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfDecision decision * scalar

    static member (+) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision + f

    static member (+) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision + f

    static member (+) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision + scalar

    static member (+) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfScalar scalar + decision

    static member (+) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + rhsDecision

    static member (-) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + (-1.0 * rhsDecision)

    static member (-) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision - f

    static member (-) (f:float, decision:Decision) =
        LinearExpression.OfDecision decision - f

    static member (-) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfScalar scalar + (-1.0 * decision)

    static member (-) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision + (-1.0 * scalar)

    static member (<==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision <== f

    static member (<==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f <== decision

    static member (<==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision <== scalar

    static member (<==) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfScalar scalar <== decision

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision == f

    static member (==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f  == decision

    static member (==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision == scalar

    static member (==) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfScalar scalar == decision

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (>==) (decision:Decision, f:float) =
        LinearExpression.OfDecision decision >== f

    static member (>==) (f:float, decision:Decision) =
        LinearExpression.OfFloat f >== decision

    static member (>==) (decision:Decision, scalar:Scalar) =
        LinearExpression.OfDecision decision >== scalar

    static member (>==) (scalar:Scalar, decision:Decision) =
        LinearExpression.OfScalar scalar >== decision

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision >== rhsDecision


and LinearExpression (elements : Map<DecisionName, Scalar * Decision>, offset:Scalar) =
    member internal this.Elements = elements
    member internal this.Offset = offset

    static member private Equivalent (lExpr:LinearExpression) (rExpr:LinearExpression) =
        let isEqualOffset = (lExpr.Offset = rExpr.Offset)

        let leftOnlyNamesAreZero = 
            (true, lExpr.Elements)
            ||> Map.fold (fun a k (lCoef, _) -> 
                            match Map.tryFind k rExpr.Elements with
                            | Some (rCoef, _) -> lCoef = rCoef
                            | None -> lCoef = Scalar.Zero)

        let rightOnlyNamesAreZero =
            (true, rExpr.Elements)
            ||> Map.fold (fun a k (rCoef, _) -> 
                            match Map.tryFind k lExpr.Elements with
                            | Some (lCoef, _) -> lCoef = rCoef
                            | None -> rCoef = Scalar.Zero)

        isEqualOffset && leftOnlyNamesAreZero && rightOnlyNamesAreZero

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as expr -> LinearExpression.Equivalent this expr
        | _ -> false

    static member OfFloat (f:float) =
        LinearExpression (Map.empty, Value f)

    static member OfScalar (s:Scalar) =
        LinearExpression (Map.empty, s)

    static member OfDecision (d:Decision) =
        let elements = Map.ofList [d.Name, (Value 1.0 , d)]
        LinearExpression (elements, Value 0.0)

    static member GetDecisions (expr:LinearExpression) =
        expr.Elements
        |> Map.toList
        |> List.map (snd >> snd)
        |> Set.ofList

    static member Zero =
        LinearExpression (Map.empty, Scalar.Zero)

    static member private Merge (lExpr:LinearExpression, rightExpr:LinearExpression) =
        // Assume the Left LinearExpression is larger than the right

        let newElements = 
            (lExpr.Elements, rightExpr.Elements) 
            ||> Map.fold (fun m k (rCoef, rDec) -> 
                            match Map.tryFind k m with
                            | Some (lCoef, lDec) ->
                                if lDec.Type = rDec.Type then
                                    Map.add k (lCoef + rCoef, lDec) m
                                else
                                    invalidArg "DecisionType" "Cannot have different DecisionType for same DecisionName"
                            | None ->
                                Map.add k (rCoef, rDec) m)

        let newOffset = lExpr.Offset + rightExpr.Offset

        LinearExpression (newElements, newOffset)

    static member (+) (l:LinearExpression, r:LinearExpression) =
        let lSize = l.Elements.Count
        let rSize = l.Elements.Count

        if lSize > rSize then
            LinearExpression.Merge (l, r)
        else
            LinearExpression.Merge (r, l)

    static member (+) (expr:LinearExpression, f:float) =
        expr + (LinearExpression.OfFloat f)

    static member (+) (f:float, expr:LinearExpression) =
        (LinearExpression.OfFloat f) + expr

    static member (+) (expr:LinearExpression, scalar:Scalar) =
        expr + (LinearExpression.OfScalar scalar)

    static member (+) (scalar:Scalar, expr:LinearExpression) =
        (LinearExpression.OfScalar scalar) + expr

    static member (+) (expr:LinearExpression, decision:Decision) =
        expr + (LinearExpression.OfDecision decision)
    
    static member (+) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision + expr

    static member (*) (expr:LinearExpression, scalar:Scalar) =
        let newElements = Map.map (fun k (c, d) -> (c * scalar, d)) expr.Elements
        LinearExpression (newElements, expr.Offset * scalar)

    static member (*) (scalar:Scalar, expr:LinearExpression) =
        expr * scalar

    static member (*) (expr:LinearExpression, f:float) =
        expr * (Scalar.Value f)

    static member (*) (f:float, expr:LinearExpression) =
        expr * f

    static member (-) (expr:LinearExpression, f:float) =
        expr + (-1.0 * f)

    static member (-) (f:float, expr:LinearExpression) =
        f + (-1.0 * expr)

    static member (-) (expr:LinearExpression, s:Scalar) =
        expr + (-1.0 * s)

    static member (-) (s:Scalar, expr:LinearExpression) =
        s + (-1.0 * expr)

    static member (-) (expr:LinearExpression, d:Decision) =
        expr + (-1.0 * d)

    static member (-) (d:Decision, expr:LinearExpression) =
        d + (-1.0 * expr)

    static member (-) (lExpr:LinearExpression, rExpr:LinearExpression) =
        lExpr + (-1.0 * rExpr)

    static member (<==) (lhs:LinearExpression, rhs:float) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

    static member (<==) (lhs:float, rhs:LinearExpression) =
        Inequality (LinearExpression.OfFloat lhs, LessOrEqual, rhs)

    static member (<==) (lhs:LinearExpression, rhs:Scalar) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfScalar rhs)

    static member (<==) (lhs:Scalar, rhs:LinearExpression) =
        Inequality (LinearExpression.OfScalar lhs, LessOrEqual, rhs)

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

    static member (==) (lhs:LinearExpression, rhs:Scalar) =
        Equality (lhs, LinearExpression.OfScalar rhs)

    static member (==) (lhs:Scalar, rhs:LinearExpression) =
        Equality (LinearExpression.OfScalar lhs, rhs)

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

    static member (>==) (lhs:LinearExpression, rhs:Scalar) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfScalar rhs)

    static member (>==) (lhs:Scalar, rhs:LinearExpression) =
        Inequality (LinearExpression.OfScalar lhs, GreaterOrEqual, rhs)

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