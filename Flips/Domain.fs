module Flips.Domain

open System


type DecisionType =
    | Boolean
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float


type Decision = {
    Id : Guid
    Description : string
    DecisionType : DecisionType
}
with
    static member (*) (decision:Decision, scalar:float) =
        LinearExpression.ofDecision decision + scalar
    
    static member (*) (scalar:float, decision:Decision) =
        LinearExpression.ofFloat scalar + decision

    static member (+) (scalar:float, decision:Decision) =
        LinearExpression.ofFloat scalar + decision

    static member (+) (decision:Decision, scalar:float) =
        LinearExpression.ofDecision decision + scalar

    static member (+) (decision:Decision, rightDecision:Decision) =
        LinearExpression.ofDecision decision + rightDecision

    static member (+) (decision:Decision, expr:LinearExpression) =
        LinearExpression.ofDecision decision + expr

    static member (<==) (decision:Decision, scalar:float) =
        LinearExpression.ofDecision decision <== scalar

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.ofDecision decision <== rhsDecision

    static member (<==) (decision:Decision, element:LinearElement) =
        LinearExpression.ofDecision decision <== element

    static member (<==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.ofDecision decision <== expr

    static member (==) (decision:Decision, scalar:float) =
        LinearExpression.ofDecision decision == scalar

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.ofDecision decision == rhsDecision

    static member (==) (decision:Decision, element:LinearElement) =
        LinearExpression.ofDecision decision == element

    static member (==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.ofDecision decision == expr

    static member (>==) (decision:Decision, scalar:float) =
        LinearExpression.ofDecision decision >== scalar

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.ofDecision decision >== rhsDecision

    static member (>==) (decision:Decision, element:LinearElement) =
        LinearExpression.ofDecision decision >== element

    static member (>==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.ofDecision decision >== expr


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
        LinearExpression.ofLinearElement elem + scalar

    static member (+) (scalar:float, elem:LinearElement) =
        elem + scalar

    static member (+) (elem:LinearElement, decision:Decision) =
        LinearExpression.ofLinearElement elem + decision

    static member (+) (elem:LinearElement, rElem:LinearElement) =
        LinearExpression.ofLinearElement elem + rElem

    static member (+) (elem:LinearElement, expr:LinearExpression) =
        LinearExpression.ofLinearElement elem + expr

    static member (<==) (lhs:LinearElement, rhs:float) =
        LinearExpression.ofLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.ofLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.ofLinearElement lhs <== rhs

    static member (<==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.ofLinearElement lhs <== rhs

    static member (==) (lhs:LinearElement, rhs:float) =
        LinearExpression.ofLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.ofLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.ofLinearElement lhs == rhs

    static member (==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.ofLinearElement lhs == rhs

    static member (>==) (lhs:LinearElement, rhs:float) =
        LinearExpression.ofLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:Decision) =
        LinearExpression.ofLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:LinearElement) =
        LinearExpression.ofLinearElement lhs >== rhs

    static member (>==) (lhs:LinearElement, rhs:LinearExpression) =
        LinearExpression.ofLinearElement lhs >== rhs


and LinearExpression = LinearExpression of List<LinearElement>
with
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

    static member ofFloat (scalar:float) =
        LinearExpression [Scalar scalar]

    static member ofDecision (decision:Decision) =
        LinearExpression [Variable (1.0, decision)]

    static member ofLinearElement (elem:LinearElement) =
        LinearExpression [elem]

    static member (<==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, LessOrEqual, LinearExpression.ofFloat rhs)

    static member (<==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, LessOrEqual, LinearExpression.ofDecision rhs)

    static member (<==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, LessOrEqual, LinearExpression.ofLinearElement rhs)

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        Constraint (lhs, LessOrEqual, rhs)

    static member (==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, Equal, LinearExpression.ofFloat rhs)

    static member (==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, Equal, LinearExpression.ofDecision rhs)

    static member (==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, Equal, LinearExpression.ofLinearElement rhs)

    static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        Constraint (lhs, Equal, rhs)

    static member (>==) (lhs:LinearExpression, rhs:float) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.ofFloat rhs)

    static member (>==) (lhs:LinearExpression, rhs:Decision) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.ofDecision rhs)

    static member (>==) (lhs:LinearExpression, rhs:LinearElement) =
        Constraint (lhs, GreaterOrEqual, LinearExpression.ofLinearElement rhs)

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

    let create id description decisionType =
        {
            Id = id
            Description = description
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



