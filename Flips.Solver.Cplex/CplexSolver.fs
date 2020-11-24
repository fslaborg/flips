namespace Flips.Solver.Cplex.Internals
open Flips.Types
open System.Collections.Generic
open ILOG.Concert

module internal CplexSolver =
  type internal CPlexProblemShape = {
      cplexNumVarToFlipsDecision : IReadOnlyDictionary<INumVar,Decision>
      flipsDecisionToCplexNumVar : IReadOnlyDictionary<Decision,INumVar>
  }
  
  type internal ToCplexContext = {
      obfuscateVarNames        : bool
      obfuscateConstraintNames : bool
  }
