namespace Flips.Solver.Cplex.Internals

open System.Collections.Generic

open Flips.Types
open Flips.Types.TypeExtensions
open Flips.Types.TypeExtensions.Cplex

open ILOG.CPLEX
open ILOG.Concert

module rec CplexSolver =
        
    module Primitives =
        type Vars = IReadOnlyDictionary<DecisionName, INumVar>

        let setVariable  ({Type = decisionType; Name = DecisionName name }) (var:INumVar) =
            let lb,ub = 
                match decisionType with
                | Boolean -> 0.,1.
                | Integer (lb, ub) -> float lb, float ub
                | Continuous (lb, ub) -> lb,  ub
            if var.Type = decisionType.ToCplex then
                var.LB <- lb
                var.UB <- ub
            else
                failwithf "can't change type of decision %A from %A to %A" name decisionType var.Type
            

        let createVariable ({Type = decisionType; Name = DecisionName name }) state =

            let lb,ub = 
                match decisionType with
                | Boolean -> 0.,1.
                | Integer (lb, ub) -> float lb, float ub
                | Continuous (lb, ub) -> lb,  ub

            let variable = 
              if state.problem.toBackendSettings.obfuscateVarNames then
                state.cplex.NumVar(lb, ub, decisionType.ToCplex)
              else
                state.cplex.NumVar(lb, ub, decisionType.ToCplex, name)    
            
            variable    

        let buildExpression (vars: Vars) (expr: LinearExpression) state =
            let reducedExpr = Flips.Types.LinearExpression.Reduce expr
            let cplexLinearExpr = state.cplex.LinearNumExpr(Constant = reducedExpr.Offset)
            for KeyValue(decisionName, coeff) in reducedExpr.Coefficients do
                let var = vars.[decisionName]
                cplexLinearExpr.AddTerm(var, coeff)
            cplexLinearExpr

        let addObjective (vars: Vars) (objective: Flips.Types.Objective) state =
            let expr = buildExpression vars objective.Expression state
            let (ObjectiveName(name)) = objective.Name
            match state.cplex.GetObjective() with
            | null -> state.cplex.AddObjective(objective.Sense.ToCplex, expr, name)
            | cplexObjective ->
                if state.options.HasDoNotReuseState then
                    failwithf "shouldn't reuse cplex state"
                else
                    cplexObjective.Expr <- expr
                    cplexObjective.Name <- name
                    cplexObjective.Sense <- objective.Sense.ToCplex
                    cplexObjective

        let addConstraint (vars: Vars) (c: Flips.Types.Constraint) state =
            let lhs = buildExpression vars c.Expression.LHS state
            let rhs = buildExpression vars c.Expression.RHS state
            match c.Expression with
            | Equality (_, _)                  -> state.cplex.AddEq(lhs, rhs)
            | Inequality(_, GreaterOrEqual, _) -> state.cplex.AddGe(lhs, rhs)
            | Inequality(_, LessOrEqual, _)    -> state.cplex.AddLe(lhs, rhs)
        
        let addConstraints vars constraints state =
            [
                for c in constraints do
                    addConstraint vars c state
            ]

        let buildSolution (decisions: Decision seq) (vars:Vars) state (objective: Flips.Types.Objective) =
            let decisionMap =
                decisions
                |> Seq.map (fun d ->
                    match vars.TryGetValue d.Name with
                    | true, v -> d, state.cplex.GetValue v 
                    | false, _ ->
                        if state.ofCplexContext.unknownDecisionTurnsToZero then
                            d, 0.
                        else
                            raise (Exception(sprintf "Decision %A was somehow not setup" d.Name))
                )
                |> Map.ofSeq
            {|
                DecisionResults = decisionMap
                ObjectiveResult = Flips.Types.LinearExpression.Evaluate decisionMap objective.Expression
            |}

        let solve (model: Flips.Model.Model) state =
            let decisionsByName = 
                [
                    for c in model.Constraints do
                        yield! c.Expression.GetDecisions()
                    for o in model.Objectives do
                        yield! o.Expression.GetDecisions()
                ]
                |> HashSet
                |> Seq.map (fun d -> d.Name, d)
                |> readOnlyDict
            
            let vars =
                match state.problem.flipsDecisionToCplexNumVar with
                | :? Dictionary<_,_> as d -> d
                | :? IReadOnlyDictionary<_,_> as d ->
                    let d =
                        [for (KeyValue(k,v)) in d -> k,v]
                        |> dict 
                        |> Dictionary
                    state.problem <- {state.problem with flipsDecisionToCplexNumVar = d }
                    d
                | _ ->
                    let d = Dictionary<_,_>()
                    state.problem <- {state.problem with flipsDecisionToCplexNumVar = d }
                    d

            for KeyValue(name,decision) in decisionsByName do

                match vars.TryGetValue name with
                | false, _ ->
                    vars.[name] <- 
                        let cplexVar = createVariable decision state
                        state.cplex.Add cplexVar |> ignore
                        
                        cplexVar
                | true, var ->
                    
                    if state.options.HasDoNotReuseState then
                        failwithf "using the same solver more than once is not an option"
                    else
                        printfn "variable %A already exists, removing before re-adding" name
                        setVariable decision var
                
            let cx = addConstraints vars model.Constraints state
            let objective = model.Objectives.Head
            let _ = addObjective vars objective state

            // look for WriteToFile option
            state.options.ExportToFiles 
            |> Seq.iter(state.cplex.ExportModel)
            
            if state.cplex.Solve() then
                let solution = buildSolution decisionsByName.Values vars state objective
                Ok solution
            else
                Error ""


    type ICPlexOption =
        inherit System.IComparable
        
    type CplexOption =
        | DoNotReuseState
        | WriteToFile of fileName: string
        interface ICPlexOption

    type CplexOptions =
        {
            options : ICPlexOption Set
        }
        member x.HasDoNotReuseState = x.options.Contains DoNotReuseState
        member x.ExportToFiles =
            x.options 
            |> Set.toSeq
            |> Seq.choose (function | :? CplexOption as x -> Some x | _ -> None)
            |> Seq.choose (function WriteToFile file -> Some file | _ -> None)
            |> Seq.toArray

    type ICplexSolverState =
        internal {
            cplex: Cplex
            mutable problem: CPlexProblemState
            lock: obj
            ofCplexContext: OfCplexContext
            options       : CplexOptions
        }

        static member create () =
            { cplex          = new Cplex()
              problem        = createEmpty()
              lock           = obj()
              ofCplexContext = { unknownDecisionTurnsToZero = true }
              options        = {options =  Set.ofList [DoNotReuseState] }
            }

        static member setOption cplexOption (state: ICplexSolverState) = 
            { state with options = { state.options with options = Set.add cplexOption state.options.options } }

        static member removeOption (cplexOptionPredicate: CplexOption -> bool) state = 
            let newOptions =
                [
                    for i in state.options.options do
                        match i with
                        | :? CplexOption as i ->
                            if not (cplexOptionPredicate i) then
                                i :> ICPlexOption
                        | _ -> ()
                ] |> Set.ofList
            { state with options = { state.options with options = newOptions } }

    type CPlexProblemState = 
        {
            cplexObjectivesToFlips     : IReadOnlyDictionary<IObjective, ObjectiveName>
            cplexNumVarToFlipsDecision : IReadOnlyDictionary<INumVar,DecisionName>
            flipsDecisionToCplexNumVar : IReadOnlyDictionary<DecisionName,INumVar>
            flipsConstraintToCplexExpr : IReadOnlyDictionary<Constraint, IConstraint>
            cplexExprToFlipsConstraint : IReadOnlyDictionary<IConstraint,Constraint>
            toBackendSettings          : ToCplexContext
        } 
        static member empty = 
            {
                cplexObjectivesToFlips     = readOnlyDict []
                cplexNumVarToFlipsDecision = readOnlyDict []
                flipsDecisionToCplexNumVar = readOnlyDict []
                flipsConstraintToCplexExpr = readOnlyDict []
                cplexExprToFlipsConstraint = readOnlyDict []
                toBackendSettings = { obfuscateVarNames = false; obfuscateConstraintNames = false}
            }
        member x.GetBackendVariable (decision: Decision) = x.flipsDecisionToCplexNumVar.[decision.Name]
        member x.GetBackendConstraint (expr: Constraint) = x.flipsConstraintToCplexExpr.[expr]
        member x.GetFlipsDecision(backendVariable: INumVar) = x.cplexNumVarToFlipsDecision.[backendVariable]
        member x.GetFlipsConstraint (expr: IConstraint) = x.cplexExprToFlipsConstraint.[expr]
    let createEmpty () = CPlexProblemState.empty
    
    let createFromModel toBackendSettings =
      { CPlexProblemState.empty with
          cplexNumVarToFlipsDecision = new Dictionary<_,_>()
          flipsDecisionToCplexNumVar = new Dictionary<_,_>()
          flipsConstraintToCplexExpr = new Dictionary<_,_>()
          cplexExprToFlipsConstraint = new Dictionary<_,_>()
          toBackendSettings          = toBackendSettings
      }

    type ToCplexContext = {
        obfuscateVarNames        : bool
        obfuscateConstraintNames : bool
    }

    type OfCplexContext = {
        /// in Flips.Solver.Optano -> 
        ///    ```fsharp
        ///    match Dictionary.tryFind d.Name vars with
        ///    | Some var -> d, var.Value
        ///    | None -> d, 0.0
        ///    ```
        /// some user may prefer an exception
        unknownDecisionTurnsToZero : bool
    }
