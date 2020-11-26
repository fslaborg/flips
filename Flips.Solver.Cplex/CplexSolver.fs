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
            state.cplex.AddObjective(objective.Sense.ToCplex, expr, name)

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
            let vars = Dictionary()
            for KeyValue(name,decision) in decisionsByName do
                vars.[name] <- 
                    let cplexVar = (createVariable decision state)
                    state.cplex.Add cplexVar |> ignore
                    cplexVar
            
            let cx = addConstraints vars model.Constraints state
            let objective = model.Objectives.Head
            let _ = addObjective vars objective state
            if state.cplex.Solve() then
                let solution = buildSolution decisionsByName.Values vars state objective
                Ok solution
            else
                Error ""


    type ICplexSolverState =
        internal {
            cplex: Cplex
            mutable problem: CPlexProblemState
            lock: obj
            ofCplexContext: OfCplexContext
        }
        static member create () =
            { cplex = new Cplex()
              problem = createEmpty()
              lock = obj()
              ofCplexContext = { unknownDecisionTurnsToZero = true }
            }


    type CPlexProblemState = 
        {
            cplexNumVarToFlipsDecision : IReadOnlyDictionary<INumVar,Decision>
            flipsDecisionToCplexNumVar : IReadOnlyDictionary<Decision,INumVar>
            flipsConstraintToCplexExpr : IReadOnlyDictionary<Constraint, IConstraint>
            cplexExprToFlipsConstraint : IReadOnlyDictionary<IConstraint,Constraint>
            toBackendSettings          : ToCplexContext
        } 
        static member empty = 
            {
                cplexNumVarToFlipsDecision = readOnlyDict []
                flipsDecisionToCplexNumVar = readOnlyDict []
                flipsConstraintToCplexExpr = readOnlyDict []
                cplexExprToFlipsConstraint = readOnlyDict []
                toBackendSettings = { obfuscateVarNames = false; obfuscateConstraintNames = false}
            }
        member x.GetBackendVariable (decision: Decision) = x.flipsDecisionToCplexNumVar.[decision]
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
