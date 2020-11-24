namespace Flips

open Flips.Legacy.Types

[<RequireQualifiedAccess>]
module Solver =
    
    /// <summary>The function used to call the underlying solver with the model</summary>
    /// <param name="settings">The settings for the solver to use</param>
    /// <param name="model">A model which represents the problem</param>
    /// <returns>A solution which contains results if successful or a message in case of an error</returns>
    let solve (settings:SolverSettings) (model:Flips.Model.Model) =
        
        match settings.SolverType with
        | CBC -> Flips.Legacy.ORTools.solve Flips.Legacy.ORTools.OrToolsSolverType.CBC settings model
        | GLOP -> Flips.Legacy.ORTools.solve Flips.Legacy.ORTools.OrToolsSolverType.GLOP settings model
        | Cplex128 -> Flips.Legacy.Optano.solve Flips.Legacy.Optano.OptanoSolverType.Cplex128 settings model
        | Gurobi900 -> Flips.Legacy.Optano.solve Flips.Legacy.Optano.OptanoSolverType.Gurobi900 settings model
        //failwithf "todo: solve this"
        (*
        *)