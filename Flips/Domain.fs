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
    static member (*) (coefficient:float, decision:Decision) =
        Element (Variable (coefficient, decision))

    static member (*) (decision:Decision, coefficient:float) =
        coefficient * decision

    static member (+) (constant:float, decision:Decision) =
        Elements [Constant constant; Variable (1.0, decision)]

    static member (+) (decision:Decision, constant:float) =
        constant + decision

    static member (+) (leftDecision:Decision, rightDecision:Decision) =
        Elements [Variable (1.0, leftDecision); Variable (1.0, rightDecision)]

    static member (+) (decision:Decision, expr:LinearExpression) =
        match expr with
        | Empty -> Element (Variable (1.0, decision))
        | Element e -> Elements [e; Variable (1.0, decision)]
        | Elements es -> Elements ([Variable (1.0, decision)] @ es)

    static member (+) (expr:LinearExpression, decision:Decision) =
        decision + expr

    static member (<==) (decision:Decision, constant:float) =
        let lhs = Element (Variable (1.0, decision))
        let rhs = Element (Constant constant)
        Constraint (lhs, LessOrEqual, rhs)

    static member (<==) (lhsDecision:Decision, rhsDecision:Decision) =
        let lhs = Element (Variable (1.0, lhsDecision))
        let rhs = Element (Variable (1.0, rhsDecision))
        Constraint (lhs, LessOrEqual, rhs)




and LinearElement =
    | Zero
    | Constant of float
    | Variable of Coefficent:float * Decision


and LinearExpression = 
    | Empty
    | Element of LinearElement
    | Elements of List<LinearElement>


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



