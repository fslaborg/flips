namespace Flips

open Flips.Types


[<RequireQualifiedAccess>]
module Decision =

    let create decisionName decisionType =
        if System.String.IsNullOrEmpty(decisionName) then
            invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        {
            Name = DecisionName decisionName
            Type = decisionType
        }

    let createBoolean decisionName =
        if System.String.IsNullOrEmpty(decisionName) then
            invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Boolean
        }

    let createInteger decisionName lowerBound upperBound =
        if System.String.IsNullOrEmpty(decisionName) then
                invalidArg "decisionName" "Cannot have Name of Decision that is null or empty"
        if lowerBound > upperBound then
            invalidArg "LowerBound" "Cannot create Decision where LowerBound is greater than UpperBound"
        {
            Name = DecisionName decisionName
            Type = DecisionType.Integer (lowerBound, upperBound)
        }

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

    let internal GetDecisions (c:Constraint) =
        match c.Expression with
        | Inequality (lhs, _, rhs) | Equality (lhs, rhs) ->
            let lhsDecisions = LinearExpression.GetDecisions lhs
            let rhsDecisions = LinearExpression.GetDecisions rhs
            lhsDecisions + rhsDecisions
        

    let create (constraintName:string) (cExpr:ConstraintExpression) =
        if System.String.IsNullOrEmpty(constraintName) then
            invalidArg "ConstraintName" "Cannot have Name of Constraint that is null or empty"
        {
            Name = ConstraintName constraintName
            Expression = cExpr
        }


[<RequireQualifiedAccess>]
module Objective =

    let internal GetDecisions (objective:Objective) =
        LinearExpression.GetDecisions objective.Expression

    let create objectiveName sense expression =
        if System.String.IsNullOrEmpty(objectiveName) then
            invalidArg "ObjectiveName" "Cannot have Name of Decision that is null or empty"
        {
            Name = ObjectiveName objectiveName
            Sense = sense
            Expression = expression
        }


[<RequireQualifiedAccess>]
module Model =

    type Model = private {
        _Objective : Objective
        _Constraints : List<Constraint>
    } 
    with
        member public this.Objective = this._Objective
        member public this.Constraints = this._Constraints

    let internal GetDecisions (m:Model) =
        let objectiveDecisions = Objective.GetDecisions m.Objective

        (objectiveDecisions, m.Constraints)
        ||> List.fold (fun decs c -> decs + (Constraint.GetDecisions c))

    let create objective =

        {
            _Objective = objective
            _Constraints = []
        }

    let addConstraint c (model:Model) =

        { model with _Constraints = [c] @ model.Constraints }

    let addConstraints constraints model =
        (model, constraints) ||> Seq.fold (fun model c -> addConstraint c model)


[<RequireQualifiedAccess>]
module Solution =

    let getValues (s:Solution) (m:Map<_,Decision>) =
        let getWithDefault _ d =
            match Map.tryFind d s.DecisionResults with
            | Some v -> v
            | None -> 0.0
        m |> Map.map getWithDefault


[<AutoOpen>]
module Builders =

    let internal isTuple t = t.GetType() |> Reflection.FSharpType.IsTuple

    let internal getFields (t:obj) = t |> Reflection.FSharpValue.GetTupleFields |> Array.toList

    let rec internal flattenFields f =
        f
        |> List.collect(
            fun t ->
                if isTuple t then
                    flattenFields (getFields t)
                else
                    [t]
        )

    let internal tupleToObjectList (t:obj) : List<obj> =
        if isTuple t then
            t |> getFields |> flattenFields
        else
            [t]

    let internal namer (prefix:string) (indices:obj) : string =
        tupleToObjectList indices
        |> List.map (sprintf "%O")
        |> String.concat "_"
        |> (sprintf "%s|%s" prefix)


    type ConstraintBuilder (constraintSetPrefix:string) =

        member this.Yield (cExpr:ConstraintExpression) =
            cExpr

        member this.For(source:seq<'a>, body:'a -> seq<'b * ConstraintExpression>) =
            source
            |> Seq.collect (fun x -> body x |> Seq.map (fun (idx, expr) -> (x, idx), expr))

        member this.For(source:seq<'a>, body:'a -> ConstraintExpression) =
            source |> Seq.map (fun x -> x, body x)

        member this.Run(source:seq<'a * ConstraintExpression>) =
            source |> Seq.map (fun (n, c) -> Constraint.create (namer constraintSetPrefix n) c)


    type DecisionBuilder (decisionSetPrefix:string) =

        let createDecision indices decisionType =
            let name = namer decisionSetPrefix indices
            let decision = Decision.create name decisionType
            indices, decision

        member this.Yield (decisionType:DecisionType) =
            decisionType

        member this.For(source:seq<'a>, body:'a -> 'b) =
            source |> Seq.map (fun x -> x, body x)

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.collect (fun (g, h) ->
            h |> Seq.map (fun (h, i) -> createDecision (a,b,c,d,e,f,g,h) i))))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.map (fun (g, h) -> createDecision (a,b,c,d,e,f,g) h)))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.map (fun (f, g) -> createDecision (a,b,c,d,e,f) g))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.map (fun (e, f) -> createDecision (a,b,c,d,e) f)))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*DecisionType>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.map (fun (d, e) -> createDecision (a,b,c,d) e))))

        member this.Run(a:seq<_*seq<_*seq<_*DecisionType>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.map (fun (c, d) -> createDecision (a,b,c) d)))

        member this.Run(a:seq<_*seq<_*DecisionType>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.map (fun (b, c) -> createDecision (a, b) c))

        member this.Run(a:seq<_*DecisionType>) = 
            a |> Seq.map (fun (a, b) -> createDecision a b)

