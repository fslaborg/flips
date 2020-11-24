namespace Flips

open System.Collections.Generic
open Flips.Types
open Flips.FlipsVersion2.Internals




namespace Flips.Types


// todo: this was right under Flips.Solver but forbids to create that as a namespace
namespace Flips.Solve

open Flips.FlipsVersion2.Types
[<RequireQualifiedAccess>]
module Solver =
    
    /// <summary>The function used to call the underlying solver with the model</summary>
    /// <param name="settings">The settings for the solver to use</param>
    /// <param name="model">A model which represents the problem</param>
    /// <returns>A solution which contains results if successful or a message in case of an error</returns>
    let solve (settings:SolverSettings) (model:Flips.Model.Model) =
        
        failwithf "todo: solve this"
        (*
        match settings.SolverType with
        | CBC -> Flips.FlipsVersion2.Internals.ORTools.solve Flips.ORTools.OrToolsSolverType.CBC settings model
        | GLOP -> Flips.FlipsVersion2.Internals.ORTools.solve Flips.ORTools.OrToolsSolverType.GLOP settings model
        | Cplex128 -> Flips.FlipsVersion2.Internals.Optano.solve Flips.Optano.OptanoSolverType.Cplex128 settings model
        | Gurobi900 -> Flips.FlipsVersion2.Internals.Optano.solve Flips.Optano.OptanoSolverType.Gurobi900 settings model
        *)