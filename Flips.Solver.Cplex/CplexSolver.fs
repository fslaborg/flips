namespace Flips.Solver.Cplex.Internals

open System.Collections.Generic

open Flips.Types
open Flips.Types.TypeExtensions
open Flips.Types.TypeExtensions.Cplex

open ILOG.CPLEX
open ILOG.Concert

module internal rec CplexSolver =
        
    module Primitives =
        open ILOG.Concert
    
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

        let createConstraint ({Constraint.Expression = expr; Name = ConstraintName name}) state : IConstraint = failwithf "todo"

    type ICplexSolverState =
        internal {
            cplex: Cplex
            mutable problem: CPlexProblemState
            lock: obj
        }

    type internal CPlexProblemState = 
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

    let createEmpty () = CPlexProblemState.empty
    
    let createFromModel toBackendSettings =
      { CPlexProblemState.empty with
          cplexNumVarToFlipsDecision = new Dictionary<_,_>()
          flipsDecisionToCplexNumVar = new Dictionary<_,_>()
          flipsConstraintToCplexExpr = new Dictionary<_,_>()
          cplexExprToFlipsConstraint = new Dictionary<_,_>()
          toBackendSettings          = toBackendSettings
      }

    type internal ToCplexContext = {
        obfuscateVarNames        : bool
        obfuscateConstraintNames : bool
    }