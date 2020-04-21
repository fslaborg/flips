module Flips.Domain


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
    | Empty
    | Scalar of float
    | Variable of Coefficent:float * Decision
with
    static member getDecision (elem:LinearElement) =
        match elem with
        | Empty -> None
        | Scalar s -> None
        | Variable (c, d) -> Some d

    static member Zero = Empty

    static member (*) (elem:LinearElement, scalar:float) =
        match elem with
        | Empty -> Empty
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

    static member getDecisions (LinearExpression expr:LinearExpression) =
        expr
        |> List.choose LinearElement.getDecision
        |> Set.ofList

    static member Zero =
        LinearExpression []

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

    static member (*) (LinearExpression expr:LinearExpression, scalar:float) =
        LinearExpression (expr |> List.map ((*) scalar))

    static member (*) (scalar:float, LinearExpression expr:LinearExpression) =
        LinearExpression (expr |> List.map ((*) scalar))

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
    Name : string
    Expression : LinearExpression
    Sense : ObjectiveSense
    Priority : int
}


module Constraint =

    let getDecisions (Constraint (lhs, _, rhs):Constraint) =
        let lhsDecisions = LinearExpression.getDecisions lhs
        let rhsDecisions = LinearExpression.getDecisions rhs
        lhsDecisions + rhsDecisions


module Decision =

    let create name decisionType =
        {
            Name = name
            Type = decisionType
        }


module Objective =

    let create name expression sense priority weighting =
        {
            Name = name
            Expression = expression
            Sense = sense
            Priority = priority
        }


module Model =

    type Model = private {
        _Objective : List<Objective>
        _Constraints : List<Constraint>
        _Decisions : Map<DecisionName, Decision>
    } 
    with
        member public this.Objectives = this._Objective
        member public this.Constraints = this._Constraints
        member public this.Decisions = this._Decisions

    let empty =
        {
            _Objective = []
            _Constraints = []
            _Decisions = Map.empty
        }

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


    let addObjective (objective:Objective) (model:Model) =
        // TODO Check that objective priority is not repeated
        let objectiveDecisions = LinearExpression.getDecisions objective.Expression
        let mismatchedDecisions = getMismatchedDecisionTypes model.Decisions objectiveDecisions
        
        if not (Set.isEmpty mismatchedDecisions) then
            // TODO Make this error more informative
            failwith "Cannot have Decisions with the same name of differnt type"

        let newDecisions = newDecisions model.Decisions objectiveDecisions
        let newDecisionMap = (newDecisions |> List.map addToDecisionMap |> List.reduce (>>)) model.Decisions

        { model with _Objective = [objective] @ model.Objectives; _Decisions = newDecisionMap }

    let addObjectives objectives model =
        (objectives |> List.map addObjective |> List.reduce (>>)) model


    let addConstraint c (model:Model) =
        let decisions = Constraint.getDecisions c
        let mismatchedDecisions = getMismatchedDecisionTypes model.Decisions decisions
        
        if not (Set.isEmpty mismatchedDecisions) then
            // TODO Make this error more informative
            failwith "Cannot have Decisions with the same name of differnt type"

        let newDecisions = newDecisions model.Decisions decisions
        let newDecisionMap = (newDecisions |> List.map addToDecisionMap |> List.reduce (>>)) model.Decisions

        { model with _Constraints = [c] @ model.Constraints; _Decisions = newDecisionMap }

    let addConstraints constraints model =
        (constraints |> List.map addConstraint |> List.reduce (>>)) model


let inline (.*) (lhs, rhs) =
    lhs
    |> Map.filter (fun k _ -> Map.containsKey k rhs)
    |> Map.map (fun k v -> v * rhs.[k])


let inline sum m =
    m
    |> Map.toSeq
    |> Seq.sumBy snd
