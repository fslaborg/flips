﻿namespace Flips.Types

open System.Collections.Generic
open System

/// Comparer used for the reduction of LinearExpression due to float addition
type SignInsenstiveComparer private () =
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
    | Integer of LowerBound:float * UpperBound:float
    | Continuous of LowerBound:float * UpperBound:float

/// A Name which uniquely identifies the Decision
type DecisionName = DecisionName of string

/// Used for the reduction of a LinearExpression to a form used for mapping to 
/// underlying solvers.
[<NoComparison>]
type internal ReduceAccumulator = {
    DecisionTypes : Dictionary<DecisionName, DecisionType>
    Coefficients : Dictionary<DecisionName, List<float>>
    Offsets : List<float>
}

/// Represents the type of comparison between two LinearExpression
type Inequality =
    | LessOrEqual
    | GreaterOrEqual

/// Represents a decision that must be made
type Decision = {
    Name : DecisionName
    Type : DecisionType
}
with

    static member (*) (decision:Decision, f:float) =
        LinearExpression.AddDecision ((f, decision), LinearExpression.Zero)

    static member (*) (f:float, decision:Decision) =
        decision * f

    static member (+) (decision:Decision, f:float) =
        LinearExpression.AddDecision ((1.0, decision), LinearExpression.OfFloat f)

    static member (+) (f:float, decision:Decision) =
        decision + f

    static member (+) (decision:Decision, rhsDecision:Decision) =
        (1.0 * decision) + (1.0 * rhsDecision)

    static member (-) (decision:Decision, rhsDecision:Decision) =
        decision + (-1.0 * rhsDecision)

    static member (-) (decision:Decision, f:float) =
        decision + (-1.0 * f)

    static member (-) (f:float, decision:Decision) =
        f + (-1.0 * decision)

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


and 
    /// A type used for mapping a LinearExpression to a form which a Solver can use
    [<NoComparison>][<CustomEquality>] 
    internal ReducedLinearExpression =
    {
        DecisionTypes : Dictionary<DecisionName, DecisionType>
        Coefficients : Dictionary<DecisionName, float>
        Offset : float
    } with
    static member private NearlyEquals (a:float) (b:float) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        if (aValue >>> 63) <> (bValue >>> 63) then
            a = b
        else
            System.Math.Abs(aValue - bValue) <= 10_000L

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? ReducedLinearExpression as that ->
            let offsetSame = ReducedLinearExpression.NearlyEquals this.Offset that.Offset

            let thisCoefficients = Map.ofDictionary this.Coefficients
            let thatCoefficients = Map.ofDictionary that.Coefficients

            let leftMatchesRight =
                thisCoefficients
                |> Map.toSeq
                |> Seq.forall (fun (k, thisCoef) -> 
                                match Map.tryFind k thatCoefficients with
                                | Some thatCoef -> ReducedLinearExpression.NearlyEquals thisCoef thatCoef
                                | None -> ReducedLinearExpression.NearlyEquals thisCoef 0.0)

            let rightNonMatchesAreZero =
                thatCoefficients
                |> Map.toSeq
                |> Seq.forall (fun (n, thatCoef) ->
                                this.Coefficients.ContainsKey n
                                || ReducedLinearExpression.NearlyEquals thatCoef 0.0)

            let allPassing = offsetSame && leftMatchesRight && rightNonMatchesAreZero
            allPassing
        | _ -> false

    static member internal OfReduceAccumulator (acc:ReduceAccumulator) =
        let offset = acc.Offsets |> Seq.sortBy Math.Abs |> Seq.sum
        
        let coefficients = Dictionary()
        for elem in acc.Coefficients do
            elem.Value.Sort SignInsenstiveComparer.Instance
            let coefficient = Seq.sum elem.Value
            coefficients.Add(elem.Key, coefficient)

        {
            DecisionTypes = acc.DecisionTypes
            Coefficients = coefficients
            Offset = offset
        }
        

and 
    /// Represents a linear collection of Decisions, Coefficients, and an Offset
    [<NoComparison>][<CustomEquality>] LinearExpression =
    | Empty
    | AddFloat of float * LinearExpression
    | AddDecision of (float * Decision) * LinearExpression
    | Multiply of float * LinearExpression
    | AddLinearExpression of LinearExpression * LinearExpression



    static member internal Reduce (expr:LinearExpression) : ReducedLinearExpression =
        let initialState = {
            DecisionTypes = Dictionary()
            Coefficients = Dictionary()
            Offsets = ResizeArray()
        }

        let rec evaluateNode (multiplier:float, state:ReduceAccumulator) (node:LinearExpression) cont =
            match node with
            | Empty -> cont (multiplier, state)
            | AddFloat (addToOffset, nodeExpr) -> 
                state.Offsets.Add(multiplier * addToOffset)
                evaluateNode (multiplier, state) nodeExpr cont
            | AddDecision ((nodeCoef , nodeDecision), nodeExpr) ->
                match Dictionary.tryFind nodeDecision.Name state.DecisionTypes with
                | Some existingType ->
                    if existingType <> nodeDecision.Type then
                        invalidArg "DecisionType" "Cannot have different DecisionType for same DecisionName"
                    else
                        state.Coefficients.[nodeDecision.Name].Add(nodeCoef * multiplier)
                        evaluateNode (multiplier, state) nodeExpr cont
                | None ->
                    let newCoefArray = ResizeArray()
                    newCoefArray.Add(multiplier * nodeCoef)
                    state.DecisionTypes.Add(nodeDecision.Name, nodeDecision.Type)
                    state.Coefficients.Add(nodeDecision.Name, newCoefArray)
                    evaluateNode (multiplier, state) nodeExpr cont
            | Multiply (nodeMultiplier, nodeExpr) ->
                let newMultiplier = multiplier * nodeMultiplier
                evaluateNode (newMultiplier, state) nodeExpr cont
            | AddLinearExpression (lExpr, rExpr) ->
                evaluateNode (multiplier, state) lExpr (fun (_, lState) -> evaluateNode (multiplier, lState) rExpr cont)

        let (_,reduceResult) = evaluateNode (1.0, initialState) expr id

        ReducedLinearExpression.OfReduceAccumulator reduceResult

    static member internal GetDecisions (expr:LinearExpression) : Set<Decision> =
        let rec getRec cont (decisions: Set<Decision>) = function
            | Empty -> cont decisions
            | AddFloat (_, expr) -> getRec cont decisions expr
            | Multiply (_, expr) -> getRec cont decisions expr
            | AddDecision ((_, decision), expr) -> getRec cont (decisions.Add decision) expr
            | AddLinearExpression (lExpr, rExpr) -> getRec (fun l -> getRec cont l rExpr) decisions lExpr
        getRec id Set.empty expr

    static member internal Evaluate (getDecisionCoef: Decision -> float) (expr:LinearExpression) : float =

        let rec evaluateNode (multiplier:float, state:ResizeArray<float>) (node:LinearExpression) cont =
            match node with
            | Empty -> cont (multiplier, state)
            | AddFloat (f, nodeExpr) ->
                state.Add(multiplier * f)
                let newState = (multiplier, state) 
                evaluateNode newState nodeExpr cont
            | AddDecision ((nodeCoef, nodeDecision), nodeExpr) ->
                state.Add(multiplier * nodeCoef * getDecisionCoef nodeDecision)
                let newState = (multiplier, state)
                evaluateNode newState nodeExpr cont
            | Multiply (nodeMultiplier, nodeExpr) ->
                let newState = (multiplier * nodeMultiplier, state)
                evaluateNode newState nodeExpr cont
            | AddLinearExpression (lExpr, rExpr) ->
                evaluateNode (multiplier, state) lExpr (fun (_, lState) -> evaluateNode (multiplier, lState) rExpr cont)
            

        let (_,reduceResult) = evaluateNode (1.0, ResizeArray()) expr id

        reduceResult.Sort SignInsenstiveComparer.Instance
        let total = Seq.sum reduceResult
        total


    override this.GetHashCode () =
        match this with
        | Empty -> 0
        | AddFloat (f, le) -> hash (f, le)
        | AddDecision (f, d) -> hash (f, d)
        | Multiply (f, le) -> hash (f, le)
        | AddLinearExpression (le1, le2) -> hash (le1, le2)

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as that -> 
            let thisReduced = LinearExpression.Reduce this
            let thatReduced = LinearExpression.Reduce that
            thisReduced = thatReduced
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
        LinearExpression.Multiply (f, expr)

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
        LinearExpression.AddDecision((1.0, d), LinearExpression.Zero)

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

and 
    /// The representation of how two LinearExpressions must relate to one another
    [<NoComparison>]
    ConstraintExpression = 
    | Inequality of LHS:LinearExpression * Inequality * RHS:LinearExpression
    | Equality of LHS:LinearExpression * RHS:LinearExpression

/// A unique identified for a Constraint
type ConstraintName = ConstraintName of string

/// Represents a constraint for the model
[<NoComparison>]
type Constraint = {
    Name : ConstraintName
    Expression : ConstraintExpression
}

/// The goal of the optimization. Minimize will try to minimize the Objective Function
/// Maximize will try to maximize the Objective Function
type ObjectiveSense =
    | Minimize
    | Maximize

/// A name for the Objective to document what the function is meant to represent
type ObjectiveName = ObjectiveName of string

/// The goal of the optimization model
[<NoComparison>]
type Objective = {
    Name : ObjectiveName
    Sense : ObjectiveSense
    Expression : LinearExpression
}

/// The results of the optimization if it was successful
type Solution = {
    DecisionResults : Map<Decision,float>
    [<Obsolete("Please use the Objective.evaluate function instead")>]
    ObjectiveResult : float
}

/// The type of underlying solver to use
type SolverType = 
    | CBC
    | GLOP
    | Cplex128
    | Gurobi900


/// Parameters for the solver
type SolverSettings = {
    SolverType : SolverType
    MaxDuration : int64
    WriteLPFile : Option<string>
    WriteMPSFile : Option<string>
    // We want to enable this in the next major release
    //EnableOutput : bool
}

/// The result of calling the solve function. If the solve was successful, the Optimal
/// case will hold a Solution type. If it was not successful, the Supoptimal case will
/// be returned with a string reporting what the solver returned.
type SolveResult =
    | Optimal of Solution
    | Infeasible of string
    | Unbounded of string
    | Unknown of string