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

type LinearElement =
    | Zero
    | Constant of float
    | Decision of Coefficent:float * Decision

type LinearExpression = LinearExpression of List<LinearElement>

type ExpressionComparison =
    | Equal
    | LessOrEqual
    | GreaterOrEqual

type Constraint = Constraint of LHS:LinearExpression * ExpressionComparison * RHS:LinearExpression

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



