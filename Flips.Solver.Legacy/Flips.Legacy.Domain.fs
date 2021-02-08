namespace Flips.Legacy

open System.Collections.Generic
open Flips
open Flips.Solver
open Flips.Legacy.Types
open Flips.Legacy.Internals


[<RequireQualifiedAccess>]
module Objective =

    /// <summary>A function for evaluating the resulting value of an Objective after solving</summary>
    /// <param name="solution">The solution used for looking up the results of Decisions</param>
    /// <param name="objective">The Objective to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: #ISolution) (objective: #IObjective) =
        LinearExpression.evaluate solution objective.Expression


[<RequireQualifiedAccess>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float> where the values are the recommendations from the solver</returns>
    let getValues (solution: #ISolution) (decisions: #System.Collections.Generic.IDictionary<_,#IDecision>) =
        let inline getWithDefault d =
            match solution.Values.TryGetValue d with
            | true, v -> v
            | fale, _ -> 0.0

        seq { for kvp in decisions -> kvp.Key, getWithDefault kvp.Value}
        |> Map.ofSeq

    /// <summary>A function for evaluating the resulting value of a LinearExpression after solving the model</summary>
    /// <param name="solution">The solution used for lookup up the results of Decisions</param>
    /// <param name="expression">The LinearExpression to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: ISolution) (expression: ILinearExpression) =
        LinearExpression.evaluate solution expression


[<RequireQualifiedAccess>]
module Settings =

    /// The recommended default settings for a solver instance
    let basic =
        {
            SolverType = SolverType.CBC
            MaxDuration = 10_000L
            WriteLPFile = None
            WriteMPSFile = None
            // We want to enable this in a future major release
            // EnableOutput = false
        }

    /// <summary>A function to set whether to write an LP file for the model</summary>
    /// <param name="lpFile">The filename to be used as the output</param>
    /// <param name="settings">The Settings type to build a new value off of</param>
    /// <returns>A new Settings type with the WriteLPFile field updated</returns>
    let setLPFile lpFile settings =
        { settings with WriteLPFile = Some lpFile}

    /// <summary>A function to set whether to write an MPS file for the model</summary>
    /// <param name="mpsFile">The filename to be used as the output</param>
    /// <param name="settings">The Settings type to build a new value off of</param>
    /// <returns>A new Settings type with the WriteMPSFile field updated</returns>
    let setMPSFile mpsFile settings =
        { settings with WriteMPSFile = Some mpsFile}

    /// <summary>A function to set the maximum runtime of the solver in milliseconds</summary>
    /// <param name="maxDuration">The max solve time in milliseconds</param>
    /// <param name="settings">The Settings type to build a new value off of</param>
    /// <returns>A new Settings type with the MaxDuration updated</returns>
    let setMaxDuration maxDuration settings =
        { settings with MaxDuration = maxDuration }

    /// <summary>A function for setting the SolverType to use</summary>
    /// <param name="solverType">The SolverType case to use</param>
    /// <param name="settings">The Settings type to build a new value off of</param>
    /// <returns>A new Settings type with the SolverType updated</returns>
    let setSolverType solverType settings =
        { settings with SolverType = solverType }

    // We will enable this in the next major release
    /// <summary>A function for setting the EnableOutput flag</summary>
    /// <param name="solverType">The bool to set the EnableOutput field to</param>
    /// <param name="settings">The Settings type to build a new value off of</param>
    /// <returns>A new Settings type with the EnableOutput updated</returns>
    //let setEnableOutput enableOutput settings =
    //  { settings with EnableOutput = enableOutput }


#if USE_LEGACY_NAMESPACE
namespace Flips.Types


open System
open Flips.Solver


[<RequireQualifiedAccess;Obsolete("Use Flips.FlipsVersion2.Solution instead")>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float> where the values are the recommendations from the solver</returns>
    let getValues (solution: #ISolution) (decisions: #System.Collections.Generic.IDictionary<_,Decision>) =
        Flips.Legacy.Solution.getValues solution decisions

    /// <summary>A function for evaluating the resulting value of a LinearExpression after solving the model</summary>
    /// <param name="solution">The solution used for lookup up the results of Decisions</param>
    /// <param name="expression">The LinearExpression to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: ISolution) (expression: LinearExpression) =
        Flips.Legacy.Solution.evaluate solution expression



[<RequireQualifiedAccess;Obsolete("Use Flips.Legacy.Settings instead")>]
module Settings =
  let basic = Flips.Legacy.Settings.basic
  let setLPFile = Flips.Legacy.Settings.setLPFile
  let setMaxDuration = Flips.Legacy.Settings.setMaxDuration
  let setMPSFile = Flips.Legacy.Settings.setMPSFile
    
#endif