[<AutoOpen>]
module rec Flips.Types

open System.Collections.Generic
open System

/// Comparer used for the reduction of LinearExpression due to float addition
type SignInsenstiveComparer internal () =
    static let instance = SignInsenstiveComparer ()
    static member Instance = instance
    interface IComparer<float> with 
        member _.Compare (a:float, b:float) =
            Math.Abs(a).CompareTo(Math.Abs(b))

/// Represents the types of Decisions that can be made
/// A Boolean maps to a 0 or 1 value
/// An Integer type can take on any discrete value between the Upper and Lower Bound (inclusive)
/// A Continuous type can take on any value between the Upper and Lower bound (inclusive)
type DecisionType =
    | Boolean
    | Continuous of LowerBound: float * UpperBound: float
    | Integer of LowerBound: float * UpperBound: float

type IDecision =
    inherit IComparable
    abstract member Name : string
    abstract member Type : DecisionType

type LinearTerm =
    | Constant of float
    | LinearElement of coefficient:float * decision:IDecision

type ILinearExpression = 
    abstract member Terms : seq<LinearTerm>

type Relationship =
    | Equal
    | LessOrEqual
    | GreaterOrEqual

type IConstraint =
    abstract member Name : string
    abstract member LHSExpression : ILinearExpression
    abstract member RHSExpression : ILinearExpression
    abstract member Relationship : Relationship
    
/// The goal of the optimization. Minimize will try to minimize the Objective Function
/// Maximize will try to maximize the Objective Function
type ObjectiveSense =
    | Minimize
    | Maximize

type IObjective =
    abstract member Expression : ILinearExpression
    abstract member Sense : ObjectiveSense
    abstract member Name : string

type IModel =
    abstract member Constraints : seq<IConstraint>
    abstract member Objectives : IObjective list // TODO: This is wrong. Objectives should be in the order of priority

/// A Name which uniquely identifies the Decision
type DecisionName = DecisionName of string

/// Represents a decision that must be made
type Decision =  {
    Name : DecisionName
    Type : DecisionType
}
with

    interface IDecision with
        member this.Name =
            let (DecisionName name) = this.Name
            name

        member this.Type = this.Type

    static member (*) (decision: Decision, f: float) =
        LinearExpression.AddDecision ((f, decision), LinearExpression.Zero)

    static member (*) (f: float, decision: Decision) =
        decision * f

    static member (+) (decision: Decision, f: float) =
        LinearExpression.AddDecision ((1.0, decision), LinearExpression.OfFloat f)

    static member (+) (f: float, decision: Decision) =
        decision + f

    static member (+) (decision: Decision, rhsDecision: Decision) =
        (1.0 * decision) + (1.0 * rhsDecision)

    static member (-) (decision: Decision, rhsDecision: Decision) =
        decision + (-1.0 * rhsDecision)

    static member (-) (decision: Decision, f: float) =
        decision + (-1.0 * f)

    static member (-) (f: float, decision: Decision) =
        f + (-1.0 * decision)

    static member (<==) (decision: Decision, f: float) =
        LinearExpression.OfDecision decision <== f

    static member (<==) (f: float, decision: Decision) =
        LinearExpression.OfFloat f <== decision

    static member (<==) (decision: Decision, rhsDecision: Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (==) (decision: Decision, f: float) =
        LinearExpression.OfDecision decision == f

    static member (==) (f: float, decision: Decision) =
        LinearExpression.OfFloat f  == decision

    static member (==) (decision: Decision, rhsDecision: Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (>==) (decision: Decision, f: float) =
        LinearExpression.OfDecision decision >== f

    static member (>==) (f: float, decision: Decision) =
        LinearExpression.OfFloat f >== decision

    static member (>==) (decision: Decision, rhsDecision: Decision) =
        LinearExpression.OfDecision decision >== rhsDecision


[<NoComparison>]
type private ReduceAccumulator = {
    Terms : Dictionary<Decision, float list>
    mutable Offsets : float list
}

  /// A type used for mapping a LinearExpression to a form which a Solver can use
[<NoComparison; CustomEquality>] 
type private ReducedLinearExpression =
    {
        //DecisionTypes : Dictionary<DecisionName, DecisionType>
        Terms : Dictionary<Decision, float>
        Offset : float
    } with
    static member private NearlyEquals (a: float) (b: float) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        if (aValue >>> 63) <> (bValue >>> 63) then
            a = b
        else
            System.Math.Abs(aValue - bValue) <= 10_000L

    override this.Equals(obj) =
        match obj with
        | :? ReducedLinearExpression as that ->
            let offsetSame = ReducedLinearExpression.NearlyEquals this.Offset that.Offset

            let thatValuesMatchThis =
                this.Terms
                |> Seq.forall (fun (KeyValue(decision, thisCoefficient)) ->
                                match that.Terms.TryGetValue decision with
                                | true, thatCoefficient -> ReducedLinearExpression.NearlyEquals thisCoefficient thatCoefficient
                                | false, _ -> false
                )

            let thatHasNoAdditionalTerms =
                that.Terms
                |> Seq.forall (fun (KeyValue(decision, _)) -> this.Terms.ContainsKey decision)

            let allPassing = offsetSame && thatValuesMatchThis && thatHasNoAdditionalTerms
            allPassing
        | _ -> false

    static member internal OfReduceAccumulator (acc: ReduceAccumulator) =
        let offset = Math.kahanSum acc.Offsets
  
        let terms = Dictionary()
        for KeyValue (decision, coefficients) in acc.Terms do
            let coefficient = Math.kahanSum coefficients
            if coefficient <> 0.0 then // NOTE: Note sure if we need to test for -0.0
                terms.Add(decision, coefficient)

        {
            Terms = terms
            Offset = offset
        }
    

/// Represents a linear collection of Decisions, Coefficients, and an Offset
[<NoComparison; CustomEquality>] 
type LinearExpression =
    internal
    | Empty
    | AddFloat of float:float * linearExpression:LinearExpression
    | AddDecision of (float * Decision) * linearExpression:LinearExpression
    | Multiply of coefficient:float * linearExpression:LinearExpression
    | AddLinearExpression of lhsExpression:LinearExpression * rhsExpression:LinearExpression


    static member private Reduce (expr: LinearExpression) : ReducedLinearExpression =
        let accumulator = {
            Terms = Dictionary()
            Offsets = []
        }

        let rec evaluateNode (multiplier: float) (node: LinearExpression) cont =
            match node with
            | Empty -> cont (multiplier)
            | AddFloat (addToOffset, nodeExpr) -> 
                accumulator.Offsets <- (multiplier * addToOffset)::accumulator.Offsets
                evaluateNode multiplier nodeExpr cont
            | AddDecision ((nodeCoef , nodeDecision), nodeExpr) ->
                match Dictionary.tryFind nodeDecision accumulator.Terms with
                | Some existingCoefficients ->
                    accumulator.Terms.[nodeDecision] <- (nodeCoef * multiplier)::existingCoefficients
                    evaluateNode multiplier nodeExpr cont
                | None ->
                    accumulator.Terms.Add(nodeDecision, [multiplier * nodeCoef])
                    evaluateNode multiplier nodeExpr cont
            | Multiply (nodeMultiplier, nodeExpr) ->
                let newMultiplier = multiplier * nodeMultiplier
                evaluateNode newMultiplier nodeExpr cont
            | AddLinearExpression (lExpr, rExpr) ->
                evaluateNode multiplier lExpr (fun _ -> evaluateNode multiplier rExpr cont)

        let _ = evaluateNode 1.0 expr id

        ReducedLinearExpression.OfReduceAccumulator accumulator


    static member internal Evaluate (getDecisionCoef: #IReadOnlyDictionary<_,_>) (expr:LinearExpression) : float =

        let rec evaluateNode (multiplier:float, state:float list) (node:LinearExpression) cont =
            match node with
            | Empty -> cont (multiplier, state)
            | AddFloat (f, nodeExpr) ->
                let newState = (multiplier, (multiplier * f)::state) 
                evaluateNode newState nodeExpr cont
            | AddDecision ((nodeCoef, nodeDecision), nodeExpr) ->
                let newState = (multiplier, (multiplier * nodeCoef * getDecisionCoef.[nodeDecision])::state)
                evaluateNode newState nodeExpr cont
            | Multiply (nodeMultiplier, nodeExpr) ->
                let newState = (multiplier * nodeMultiplier, state)
                evaluateNode newState nodeExpr cont
            | AddLinearExpression (lExpr, rExpr) ->
                evaluateNode (multiplier, state) lExpr (fun (_, lState) -> evaluateNode (multiplier, lState) rExpr cont)

        let (_,reduceResult) = evaluateNode (1.0, []) expr id
        Math.kahanSum reduceResult

    
    interface ILinearExpression with
        member this.Terms =
            let reducedForm = LinearExpression.Reduce this
            seq { 
                for KeyValue (decision, coefficient) in reducedForm.Terms do
                    yield LinearTerm.LinearElement (coefficient, decision)
                yield LinearTerm.Constant reducedForm.Offset
            }
            
    
    override this.GetHashCode () =
        hash (LinearExpression.Reduce this)

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as otherExpr -> 
            let thisReduced = LinearExpression.Reduce this
            let otherReduced = LinearExpression.Reduce otherExpr
            thisReduced = otherReduced
        | _ -> false

    static member Zero =
        LinearExpression.Empty
    
    static member (+) (l: LinearExpression, r: LinearExpression) =
        LinearExpression.AddLinearExpression (l, r)
    
    static member (+) (expr: LinearExpression, f: float) =
        LinearExpression.AddFloat (f, expr)
    
    static member (+) (f: float, expr: LinearExpression) =
        expr + f
    
    static member (+) (expr: LinearExpression, decision: Decision) =
        LinearExpression.AddDecision ((1.0, decision), expr)
    
    static member (+) (decision: Decision, expr: LinearExpression) =
        expr + decision
    
    static member (*) (expr: LinearExpression, f: float) =
        LinearExpression.Multiply (f, expr)
    
    static member (*) (f: float, expr: LinearExpression) =
        LinearExpression.Multiply (f, expr)
    
    static member (-) (expr: LinearExpression, f: float) =
        expr + (-1.0 * f)
    
    static member (-) (f: float, expr: LinearExpression) =
        f + (-1.0 * expr)
    
    static member (-) (expr: LinearExpression, d: Decision) =
        expr + (-1.0 * d)
    
    static member (-) (d: Decision, expr: LinearExpression) =
        d + (-1.0 * expr)
    
    static member (-) (lExpr: LinearExpression, rExpr: LinearExpression) =
        lExpr + (-1.0 * rExpr)
    
    static member OfFloat (f: float) =
        LinearExpression.AddFloat(f, LinearExpression.Zero)
    
    static member OfDecision (d: Decision) =
        LinearExpression.AddDecision((1.0, d), LinearExpression.Zero)
    
    static member (<==) (lhs: LinearExpression, rhs: float) =
        ConstraintExpression (lhs, LessOrEqual, LinearExpression.OfFloat rhs)
    
    static member (<==) (lhs: float, rhs: LinearExpression) =
        ConstraintExpression (LinearExpression.OfFloat lhs, LessOrEqual, rhs)
    
    static member (<==) (lhs: LinearExpression, rhs: Decision) =
        ConstraintExpression (lhs, LessOrEqual, LinearExpression.OfDecision rhs)
    
    static member (<==) (decision: Decision, expr: LinearExpression) =
        LinearExpression.OfDecision decision <== expr
    
    static member (<==) (lhs: LinearExpression, rhs: LinearExpression) =
        ConstraintExpression (lhs, LessOrEqual, rhs)
    
    static member (==) (lhs: LinearExpression, rhs: float) =
        ConstraintExpression (lhs, Equal, LinearExpression.OfFloat rhs)
    
    static member (==) (lhs: float, rhs: LinearExpression) =
        ConstraintExpression (LinearExpression.OfFloat lhs, Equal, rhs)
    
    static member (==) (lhs: LinearExpression, rhs: Decision) =
        ConstraintExpression (lhs, Equal, LinearExpression.OfDecision rhs)
    
    static member (==) (decision: Decision, expr: LinearExpression) =
        LinearExpression.OfDecision decision == expr
    
    static member (==) (lhs: LinearExpression, rhs: LinearExpression) =
        ConstraintExpression (lhs, Equal, rhs)
    
    static member (>==) (lhs: LinearExpression, rhs: float) =
        ConstraintExpression (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)
    
    static member (>==) (lhs: float, rhs: LinearExpression) =
        ConstraintExpression (LinearExpression.OfFloat lhs, GreaterOrEqual, rhs)
    
    static member (>==) (lhs: LinearExpression, rhs: Decision) =
        ConstraintExpression (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)
    
    static member (>==) (decision: Decision, expr: LinearExpression) =
        LinearExpression.OfDecision decision >== expr
    
    static member (>==) (lhs: LinearExpression, rhs: LinearExpression) =
        ConstraintExpression (lhs, GreaterOrEqual, rhs)


/// The representation of how two LinearExpressions must relate to one another
[<NoComparison>]
type ConstraintExpression = 
    ConstraintExpression of lhs:LinearExpression * relationship:Relationship * rhs:LinearExpression

/// A unique identified for a Constraint
type ConstraintName = ConstraintName of string

/// Represents a constraint for the model
[<NoComparison>]
type Constraint = 
    internal {
        Name : ConstraintName
        ConstraintExpression : ConstraintExpression
    }

    interface IConstraint with
        member this.Name =
            let (ConstraintName name) = this.Name
            name
        member this.LHSExpression: ILinearExpression = 
            let (ConstraintExpression (lhs, _, _)) = this.ConstraintExpression
            lhs :> ILinearExpression
        member this.RHSExpression: ILinearExpression = 
            let (ConstraintExpression (_, _, rhs)) = this.ConstraintExpression
            rhs :> ILinearExpression
        member this.Relationship: Relationship = 
            let (ConstraintExpression (_, relationship, _)) = this.ConstraintExpression
            relationship


/// A name for the Objective to document what the function is meant to represent
type ObjectiveName = internal ObjectiveName of string

/// The goal of the optimization model
[<NoComparison>]
type Objective = 
    internal {
        Name : ObjectiveName
        Sense : ObjectiveSense
        Expression : ILinearExpression
    }

    interface IObjective with
        member this.Expression = this.Expression
        member this.Sense = this.Sense
        member this.Name =
            let (ObjectiveName name) = this.Name
            name

/// A type which represents the optimization model. It contains an Objective which represents the
/// goal of the model and a collection of Constraints which the model must obey.
[<NoComparison>]
type Model = 
    internal {
        Constraints : IConstraint list
        Objectives : IObjective list
    }

    interface IModel with
        member this.Constraints = this.Constraints :> seq<IConstraint>
        member this.Objectives = this.Objectives // TODO: This is wrong
