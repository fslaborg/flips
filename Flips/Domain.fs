module Flips.Domain


type DecisionType =
    | Boolean
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float


type Decision = {
    Name : string
    DecisionType : DecisionType
}
with
    static member (*) (decision:Decision, scalar:float) =
        LinearExpression.OfDecision decision + scalar
    
    static member (*) (scalar:float, decision:Decision) =
        LinearExpression.OfFloat scalar + decision

    static member (+) (scalar:float, decision:Decision) =
        LinearExpression.OfFloat scalar + decision

    static member (+) (decision:Decision, scalar:float) =
        LinearExpression.OfDecision decision + scalar

    static member (+) (decision:Decision, rightDecision:Decision) =
        LinearExpression.OfDecision decision + rightDecision

    static member (+) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision + expr

    static member (<==) (decision:Decision, scalar:float) =
        LinearExpression.OfDecision decision <== scalar

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (<==) (decision:Decision, element:LinearElement) =
        LinearExpression.OfDecision decision <== element

    static member (<==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision <== expr

    static member (==) (decision:Decision, scalar:float) =
        LinearExpression.OfDecision decision == scalar

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (==) (decision:Decision, element:LinearElement) =
        LinearExpression.OfDecision decision == element

    static member (==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision == expr

    static member (>==) (decision:Decision, scalar:float) =
        LinearExpression.OfDecision decision >== scalar

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision >== rhsDecision

    static member (>==) (decision:Decision, element:LinearElement) =
        LinearExpression.OfDecision decision >== element

    static member (>==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision >== expr


and LinearElement =
    | Zero
    | Scalar of float
    | Variable of Coefficent:float * Decision
with
    static member (*) (elem:LinearElement, scalar:float) =
        match elem with
        | Zero -> Zero
        | Scalar c -> Scalar (c * scalar)
        | Variable (c, d) -> Variable (c * scalar, d)

    static member (*) (scalar:float, elem:LinearElement) =
        elem * scalar

    static member (+) (elem:LinearElement, scalar:float) =
        LinearExpression.OfLinearElement elem + scalar

    static member (+) (scalar:float, elem:LinearElement) =
        elem + scalar

    static member (+) (elem:LinearElement, decision:Decision) =
        LinearExpression.OfLinearElement elem + decision

    static member (+) (elem:LinearElement, rElem:LinearElement) =
        LinearExpression.OfLinearElement elem + rElem

    static member (+) (elem:LinearElement, expr:LinearExpression) =
        LinearExpression.OfLinearElement elem + expr

    static member (<==) (lhs:LinearElement, rhs:float) =
        LinearExpression.OfLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.OfLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.OfLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.OfLinearElement lhs <== rhs

    static member (==) (lhs:LinearElement, rhs:float) =
        LinearExpression.OfLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.OfLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.OfLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.OfLinearElement lhs == rhs

    static member (>==) (lhs:LinearElement, rhs:float) =
        LinearExpression.OfLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.OfLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.OfLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.OfLinearElement lhs >== rhs


and LinearExpression = LinearExpression of List<LinearElement>
with
    static member OfFloat (scalar:float) =
        LinearExpression [Scalar scalar]

    static member OfDecision (decision:Decision) =
        LinearExpression [Variable (1.0, decision)]

    static member OfLinearElement (elem:LinearElement) =
        LinearExpression [elem]

    static member (*) (LinearExpression expr:LinearExpression, scalar:float) =
        LinearExpression (expr |> List.map ((*) scalar))

    static member (*) (scalar:float, LinearExpression expr:LinearExpression) =
        LinearExpression (expr |> List.map ((*) scalar))

    static member (+) (LinearExpression expr:LinearExpression, scalar:float) =
        [Scalar scalar] @ expr
        |> LinearExpression

    static member (+) (scalar:float, expr:LinearExpression) =
        expr + scalar

    static member (+) (LinearExpression expr:LinearExpression, decision:Decision) =
        [Variable (1.0, decision)] @ expr
        |> LinearExpression

    static member (+) (LinearExpression expr:LinearExpression, elem:LinearElement) =
        [elem] @ expr 
        |> LinearExpression

    static member (+) (LinearExpression expr:LinearExpression, LinearExpression rExpr:LinearExpression) =
        match expr.Length > rExpr.Length with
        | true -> rExpr @ expr
        | false -> expr @ rExpr

    static member (<==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

    static member (<==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

    static member (<==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, LessOrEqual, LinearExpression.OfLinearElement rhs)

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        Constraint (lhs, LessOrEqual, rhs)

    static member (==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, Equal, LinearExpression.OfFloat rhs)

    static member (==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, Equal, LinearExpression.OfDecision rhs)

    static member (==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, Equal, LinearExpression.OfLinearElement rhs)

    static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        Constraint (lhs, Equal, rhs)

    static member (>==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)

    static member (>==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

    static member (>==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.OfLinearElement rhs)

    static member (>==) (lhs:LinearExpression, rhs:LinearExpression) =
        Constraint (lhs, GreaterOrEqual, rhs)


and ExpressionComparison =
    | Equal
    | LessOrEqual
    | GreaterOrEqual


and Constraint = Constraint of LHS:LinearExpression * ExpressionComparison * RHS:LinearExpression


type ObjectiveSense =
    | Minimize
    | Maximize


type Objective = {
    Expression : LinearExpression
    Sense : ObjectiveSense
    Priority : float
    Weighting : float
}


type Model = {
    Objectives : List<Objective>
    Constraints : List<Constraint>
}


module Decision =

    let create name decisionType =
        {
            Name = name
            DecisionType = decisionType
        }


module Objective =

    let create expression sense priority weighting =
        {
            Expression = expression
            Sense = sense
            Priority = priority
            Weighting = weighting
        }


module Model =

    let create objectives constraints =
        {
            Objectives = objectives
            Constraints = constraints
        }

    let empty =
        {
            Objectives = []
            Constraints = []
        }

    let addObjective objective model =
        { model with Objectives = [objective] @ model.Objectives}

    let addConstraint c model =
        { model with Constraints = [c] @ model.Constraints}


let inline (.*) (lhs, rhs) =
    lhs
    |> Map.filter (fun k _ -> Map.containsKey k rhs)
    |> Map.map (fun k v -> v * rhs.[k])


let inline sum m =
    m
    |> Map.toSeq
    |> Seq.sumBy snd


let x = 1.0 * (Decision.create  "CHicken" DecisionType.Boolean)