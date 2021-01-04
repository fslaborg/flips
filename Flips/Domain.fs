namespace Flips

open Flips.Types


[<RequireQualifiedAccess>]
module Decision =

    /// <summary>Create a Decision with a given Name and Type</summary>
    /// <remarks>This function is here for completeness. It is recommend to use the functions for the specific decision types.</remarks>
    /// <param name="decisionName">The unique identifier for the Decision</param>
    /// <param name="decitionType">The type of the decision</param>
    /// <returns>A new Decision with the given DecisionType</returns>
    let create decisionName decisionType =
        if System.String.IsNullOrEmpty(decisionName) then
            invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        {
            Name = DecisionName decisionName
            Type = decisionType
        }

    /// <summary>Create a Boolean type of decision</summary>
    /// <remarks>These types of decisions are meant to represent True/False, Yes/No types of decisions. They map to 0.0 and 1.0 in the mathematical representation.</remarks>
    /// <param name="decisionName">The unique identifier for the decision</param>
    /// <returns>A new Decision with a type of Boolean</returns>
    let createBoolean decisionName =
        if System.String.IsNullOrEmpty(decisionName) then
            invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Boolean
        }

    /// <summary>Create an Integer type of decision</summary>
    /// <remarks>These types of decisions will take on whole values and are bounded by the Lower and Upper bounds, inclusive.</remarks>
    /// <param name="decisionName">The unique identifier for the decision</param>
    /// <param name="lowerBound">The smallest value the decision is allowed to take on</param>
    /// <param name="upperBound">The largest value the decision is allowed to take on</param>
    /// <returns>A new Decision with a type of Integer</returns>
    let createInteger decisionName lowerBound upperBound =
        if System.String.IsNullOrEmpty(decisionName) then
                invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        if lowerBound > upperBound then
            invalidArg "LowerBound" "Cannot create Decision where LowerBound is greater than UpperBound"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Integer (lowerBound, upperBound)
        }

    /// <summary>Create an Continuous type of decision</summary>
    /// <remarks>These types of decisions will take on any value within the Lower and Upper bounds, inclusive.</remarks>
    /// <param name="decisionName">The unique identifier for the decision</param>
    /// <param name="lowerBound">The smallest value the decision is allowed to take on</param>
    /// <param name="upperBound">The largest value the decision is allowed to take on</param>
    /// <returns>A new Decision with a type of Continuous</returns>
    let createContinuous decisionName lowerBound upperBound =
        if System.String.IsNullOrEmpty(decisionName) then
                invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        if lowerBound > upperBound then
            invalidArg "LowerBound" "Cannot create Decision where LowerBound is greater than UpperBound"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Continuous (lowerBound, upperBound)
        }


[<RequireQualifiedAccess>]
module Constraint =

    //let internal getDecisions (c: IConstraint) =
    //    match c.Expression with
    //    | Inequality (lhs, _, rhs) | Equality (lhs, rhs) ->
    //        let lhsDecisions = LinearExpression.GetDecisions lhs
    //        let rhsDecisions = LinearExpression.GetDecisions rhs
    //        lhsDecisions + rhsDecisions

    /// <summary>Create a Constraint</summary>
    /// <param name="constraintName">The unique identifier for the Constraint</param>
    /// <param name="constraintExpr">The Constraint Expression for the Constraint</param>
    /// <returns>A new Constraint</returns>
    let create (constraintName: string) (constraintExpr: ConstraintExpression) =
        if System.String.IsNullOrEmpty(constraintName) then
            invalidArg "ConstraintName" "Cannot have Name of Constraint that is null or empty"
        {
            Name = ConstraintName constraintName
            Expression = constraintExpr
        }


[<RequireQualifiedAccess>]
module Objective =

    let internal getDecisions (objective: IObjective) =
        //LinearExpression.GetDecisions objective.Expression
        objective.Expression.Terms
        |> Seq.choose (fun x -> match x with | LinearTerm.Constant _ -> None | LinearTerm.LinearElement (_, d) -> Some d)
        |> Set

    /// <summary>Create an Objective for an optimization model</summary>
    /// <param name="objectiveName">The name which describes the goal of the objective function</param>
    /// <param name="objectiveSense">The goal of the objective: Maximize or Minimize</param>
    /// <param name="objectiveExpression">The Linear Expression which describes the goal of the model</param>
    /// <returns>A new Objective</returns>
    let create objectiveName objectiveSense objectiveExpression =
        if System.String.IsNullOrEmpty(objectiveName) then
            invalidArg "ObjectiveName" "Cannot have Name of Decision that is null or empty"
        {
            Name = ObjectiveName objectiveName
            Sense = objectiveSense
            Expression = objectiveExpression
        }
#if HAS_SOLUTION_TYPE
    /// <summary>A function for evaluating the resulting value of an Objective after solving</summary>
    /// <param name="solution">The solution used for looking up the results of Decisions</param>
    /// <param name="objective">The Objective to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: Solution) (objective: Objective) =
        LinearExpression.Evaluate solution.DecisionResults objective.Expression
#endif

[<RequireQualifiedAccess>]
module Model =

    //let internal getDecisions (m: Model) =
    //    let objectiveDecisions =
    //        (Set.empty, m.Objectives)
    //        ||> List.fold (fun decs objective -> decs + (Objective.getDecisions objective))

    //    (objectiveDecisions, m.Constraints)
    //    ||> List.fold (fun decs c -> decs + (Constraint.getDecisions c))

    /// <summary>Create a Model with the given objective</summary>
    /// <param name="objective">The objective for the model</param>
    /// <returns>A new Model with an Objective but no constraints</returns>
    let create objective =

        {
            Objectives = [objective]
            Constraints = []
        }

    /// <summary>Add an Objective to a Model</summary>
    /// <param name="objective">The objective to be added to the model</param>
    /// <returns>A new Model with the Objective added</returns>
    let addObjective objective model =

        { model with Objectives = [objective] @ model.Objectives }

    /// <summary>Adds a Constraint to a Model and returns a new Model</summary>
    /// <param name="c">The constraint to be added to the model</param>
    /// <param name="model">The model to add the constraint to</param>
    /// <returns>A new Model with the constraint added</returns>
    let addConstraint c model =

        { model with Constraints = [c] @ model.Constraints }

    /// <summary>Adds a sequence of Constraints to a Model and returns a new Model</summary>
    /// <param name="constraints">The constraints to be added to the model</param>
    /// <param name="model">The model to add the constraint to</param>
    /// <returns>A new Model with the constraints added</returns>
    let addConstraints constraints model =
        (model, constraints) ||> Seq.fold (fun model c -> addConstraint c model)

[<AutoOpen>]
module Builders =

    let internal isTuple t = t.GetType() |> Reflection.FSharpType.IsTuple

    let internal getFields (t: obj) = t |> Reflection.FSharpValue.GetTupleFields |> Array.toList

    let rec internal flattenFields f =
        f
        |> List.collect(
            fun t ->
                if isTuple t then
                    flattenFields (getFields t)
                else
                    [t]
        )

    let internal tupleToObjectList (t: obj) : List<obj> =
        if isTuple t then
            t |> getFields |> flattenFields
        else
            [t]

    let internal namer (prefix: string) (indices: obj) : string =
        tupleToObjectList indices
        |> List.map (sprintf "%O")
        |> String.concat "_"
        |> (sprintf "%s_%s" prefix)


    /// <summary>A Computation Expression for creating constraints with a predefined naming convention</summary>
    /// <param name="constraintSetPrefix">The string which will be the prefix for all of the constraints</param>
    /// <returns>A sequence of Constraints whith the given prefix and a unique name for each constraint</returns>
    type ConstraintBuilder (constraintSetPrefix: string) =

        member this.Yield (cExpr: ConstraintExpression) =
            cExpr

        member this.For(source: seq<'a>, body:'a -> seq<'b * ConstraintExpression>) =
            source
            |> Seq.collect (fun x -> body x |> Seq.map (fun (idx, expr) -> (x, idx), expr))

        member this.For(source: seq<'a>, body:'a -> ConstraintExpression) =
            source |> Seq.map (fun x -> x, body x)

        member this.Run(source: seq<'a * ConstraintExpression>) =
            source |> Seq.map (fun (n, c) -> Constraint.create (namer constraintSetPrefix n) c)


    /// <summary>A Computation Expression for creating tuples of type ('Key * Decision)</summary>
    /// <param name="decisionSetPrefix">The prefix used for naming the Decisions</param>
    /// <returns>A seq of type ('Key * Decision). The result is typically used to create a Map or SliceMap</returns>
    type DecisionBuilder (decisionSetPrefix: string) =

        let createDecision indices decisionType =
            let name = namer decisionSetPrefix indices
            let decision = Decision.create name decisionType
            indices, decision

        member this.Yield (decisionType: DecisionType) =
            decisionType

        member this.For(source: seq<'a>, body:'a -> 'b) =
            source |> Seq.map (fun x -> x, body x)

        member this.Run(a: seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.collect (fun (g, h) ->
            h |> Seq.map (fun (h, i) -> createDecision (a,b,c,d,e,f,g,h) i))))))))

        member this.Run(a: seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.map (fun (g, h) -> createDecision (a,b,c,d,e,f,g) h)))))))

        member this.Run(a: seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.map (fun (f, g) -> createDecision (a,b,c,d,e,f) g))))))

        member this.Run(a: seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.map (fun (e, f) -> createDecision (a,b,c,d,e) f)))))

        member this.Run(a: seq<_*seq<_*seq<_*seq<_*DecisionType>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.map (fun (d, e) -> createDecision (a,b,c,d) e))))

        member this.Run(a: seq<_*seq<_*seq<_*DecisionType>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.map (fun (c, d) -> createDecision (a,b,c) d)))

        member this.Run(a: seq<_*seq<_*DecisionType>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.map (fun (b, c) -> createDecision (a, b) c))

        member this.Run(a: seq<_*DecisionType>) =
            a |> Seq.map (fun (a, b) -> createDecision a b)

