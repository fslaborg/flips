namespace Flips.Legacy

[<AutoOpen>]
module Types =

  open System

  open Flips.Types

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


  /// The results of the optimization if it was successful
  type Solution = {
      DecisionResults : Map<Decision,float>
      [<Obsolete("Please use the Objective.evaluate function instead")>]
      ObjectiveResult : float
  }

  /// The result of calling the solve function. If the solve was successful, the Optimal
  /// case will hold a Solution type. If it was not successful, the Supoptimal case will
  /// be returned with a string reporting what the solver returned.
  type SolveResult =
      | Optimal of Solution
      | Infeasible of string
      | Unbounded of string
      | Unknown of string

#if USE_LEGACY_NAMESPACE
namespace Flips.Types


open System
[<AutoOpen>]
module Obsolete =
  [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
  type SolverType = Flips.Legacy.Types.SolverType
  [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
  type SolverSettings = Flips.Legacy.Types.SolverSettings
  [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
  type Solution = Flips.Legacy.Types.Solution
  [<Obsolete("Use types from `Flips.Legacy.Types` instead")>]
  type SolveResult = Flips.Legacy.Types.SolveResult
#endif
