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
    static member (*) (decision:Decision, coefficient:float) =
        Element (Variable (coefficient, decision))
    
    static member (*) (coefficient:float, decision:Decision) =
        decision * coefficient

    static member (+) (constant:float, decision:Decision) =
        constant + decision

    static member (+) (decision:Decision, constant:float) =
        Elements [Constant constant; Variable (1.0, decision)]

    static member (+) (decision:Decision, rightDecision:Decision) =
        Elements [Variable (1.0, decision); Variable (1.0, rightDecision)]

    static member (+) (decision:Decision, expr:LinearExpression) =
        match expr with
        | Empty -> Element (Variable (1.0, decision))
        | Element e -> Elements [e; Variable (1.0, decision)]
        | Elements es -> Elements ([Variable (1.0, decision)] @ es)

    static member private decisionConstantComparison (decision:Decision) (constant:float) (comparison:ExpressionComparison) =
        let lhs = Element (Variable (1.0, decision))
        let rhs = Element (Constant constant)
        Constraint (lhs, comparison, rhs)

    static member (<==) (decision:Decision, constant:float) =
        Decision.decisionConstantComparison decision constant LessOrEqual

    static member (>==) (decision:Decision, constant:float) =
        Decision.decisionConstantComparison decision constant GreaterOrEqual

    static member (==) (decision:Decision, constant:float) =
        Decision.decisionConstantComparison decision constant Equal

    static member private decisionDecisionComparison (decision:Decision) (rhsDecision:Decision) (comparison:ExpressionComparison) =
        let lhs = Element (Variable (1.0, decision))
        let rhs = Element (Variable (1.0, rhsDecision))
        Constraint (lhs, comparison, rhs)

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        Decision.decisionDecisionComparison decision rhsDecision LessOrEqual

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        Decision.decisionDecisionComparison decision rhsDecision GreaterOrEqual

    static member (==) (decision:Decision, rhsDecision:Decision) =
        Decision.decisionDecisionComparison decision rhsDecision Equal

    static member private decisionLinearElementComparison (decision:Decision) (element:LinearElement) (comparison:ExpressionComparison) =
        let lhs = Element (Variable (1.0, decision))
        let rhs = Element (element)
        Constraint (lhs, comparison, rhs)

    static member (<==) (decision:Decision, element:LinearElement) =
        Decision.decisionLinearElementComparison decision element LessOrEqual

    static member (>==) (decision:Decision, element:LinearElement) =
        Decision.decisionLinearElementComparison decision element GreaterOrEqual

    static member (==) (decision:Decision, element:LinearElement) =
        Decision.decisionLinearElementComparison decision element Equal

    static member private decisionLinearExpressionComparison (decision:Decision) (expr:LinearExpression) (comparison:ExpressionComparison) =
        let lhs = Element (Variable (1.0, decision))
        Constraint (lhs, comparison, expr)

    static member (<==) (decision:Decision, expr:LinearExpression) =
        Decision.decisionLinearExpressionComparison decision expr LessOrEqual

    static member (>==) (decision:Decision, expr:LinearExpression) =
        Decision.decisionLinearExpressionComparison decision expr GreaterOrEqual

    static member (==) (decision:Decision, expr:LinearExpression) =
        Decision.decisionLinearExpressionComparison decision expr Equal





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



