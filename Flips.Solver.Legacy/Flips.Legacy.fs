namespace Flips.FlipsVersion2

open Flips.Types
open Flips.FlipsVersion2.Types

[<RequireQualifiedAccess>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float> where the values are the recommendations from the solver</returns>
    let getValues (solution: Solution) (decisions: System.Collections.Generic.IDictionary<_,Decision>) =
        let inline getWithDefault d =
            match Map.tryFind d solution.DecisionResults with
            | Some v -> v
            | None -> 0.0

        seq { for kvp in decisions -> kvp.Key, getWithDefault kvp.Value}
        |> Map.ofSeq

    /// <summary>A function for evaluating the resulting value of a LinearExpression after solving the model</summary>
    /// <param name="solution">The solution used for lookup up the results of Decisions</param>
    /// <param name="expression">The LinearExpression to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: Solution) (expression: LinearExpression) =
        LinearExpression.Evaluate solution.DecisionResults expression

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

[<RequireQualifiedAccess;Obsolete("Use Flips.FlipsVersion2.Solution instead")>]
module Solution =

    /// <summary>A function for taking the initial set of Decisions and returning the values the solver found</summary>
    /// <param name="solution">The solution that is used to look up the solver values</param>
    /// <param name="decisions">An IDictionary<'Key, Decision> that will be used for the lookups</param>
    /// <returns>A new Map<'Key,float> where the values are the recommendations from the solver</returns>
    let getValues (solution: Solution) (decisions: System.Collections.Generic.IDictionary<_,Decision>) =
        Flips.FlipsVersion2.Solution.getValues solution decisions

    /// <summary>A function for evaluating the resulting value of a LinearExpression after solving the model</summary>
    /// <param name="solution">The solution used for lookup up the results of Decisions</param>
    /// <param name="expression">The LinearExpression to evaluate the resulting value for</param>
    /// <returns>A float which is the simplification of the LinearExpression</returns>
    let evaluate (solution: Solution) (expression: LinearExpression) =
        Flips.FlipsVersion2.Solution.evaluate solution expression



[<RequireQualifiedAccess;Obsolete("Use Flips.FlipsVersion2.Settings instead")>]
module Settings =
  let basic = Flips.FlipsVersion2.Settings.basic
  let setLPFile = Flips.FlipsVersion2.Settings.setLPFile
  let setMaxDuration = Flips.FlipsVersion2.Settings.setMaxDuration
  let setMPSFile = Flips.FlipsVersion2.Settings.setMPSFile
    
#endif