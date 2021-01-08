namespace Flips.Legacy


open System.Collections.Generic

open Flips
open Flips.Types
open Flips.Legacy.Types
open Flips.Legacy.Internals
open Flips.Solver


module Optano =
    open OPTANO.Modeling.Optimization
    
    type internal OptanoSolverType =
        | Cplex128
        | Gurobi900


    let private createVariable (decision: #IDecision) =
        match decision.Type with
        | Boolean -> new Variable(decision.Name, 0.0, 1.0, Enums.VariableType.Binary)
        | Integer (lb, ub) -> new Variable(decision.Name, lb, ub, Enums.VariableType.Integer)
        | Continuous (lb, ub) -> new Variable(decision.Name, lb, ub, Enums.VariableType.Continuous)


    let addVariable (decisions: Dictionary<string, _>) (decision: #IDecision) (vars: Dictionary<string, Variable>) =
        if not (vars.ContainsKey(decision.Name)) then
            let var = createVariable decision
            decisions.[decision.Name] <- decision
            vars.[decision.Name] <- var


    let private buildExpression (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (expr: ILinearExpression) =
        let mutable exprAccumulator = Expression.Sum([0.0])

        for term in expr.Terms do
            match term with
            | Constant constant ->
                exprAccumulator <- exprAccumulator + constant
            | LinearElement (coefficient, decision) ->
                addVariable decisions decision vars
                exprAccumulator <- exprAccumulator + (coefficient * vars.[decision.Name])

        exprAccumulator


    let private addObjective (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objective: #IObjective) priority (optanoModel: Model) =
        let expr = buildExpression decisions vars objective.Expression

        let sense =
            match objective.Sense with
            | Minimize -> Enums.ObjectiveSense.Minimize
            | Maximize -> Enums.ObjectiveSense.Maximize

        let optanoObjective = new Objective(expr, objective.Name, sense, priority)
        optanoModel.AddObjective(optanoObjective)


    let private addObjectives (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objectives: #IObjective list) (optanoModel: Model) =
        let numOfObjectives = objectives.Length

        objectives
        // The order of the objectives is the priority order
        // OPTANO sorts objectives by PriorityLevel desc
        |> List.iteri (fun i objective -> addObjective decisions vars objective (numOfObjectives - i) optanoModel)

        optanoModel


    let private addConstraint (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (c: #IConstraint) (optanoModel: Model) =
        let lhsExpr = buildExpression decisions vars c.LHSExpression
        let rhsExpr = buildExpression decisions vars c.RHSExpression
        
        let optanoConstraint =
            match c.Relationship with
            | Equal             -> Constraint.Equals(lhsExpr, rhsExpr)
            | LessOrEqual       -> Constraint.LessThanOrEqual(lhsExpr, rhsExpr)
            | GreaterOrEqual    -> Constraint.GreaterThanOrEqual(lhsExpr, rhsExpr)

        optanoModel.AddConstraint(optanoConstraint, c.Name)


    let private addConstraints (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (constraints: seq<#IConstraint>) (optanoModel: Model) =
        for c in constraints do
            addConstraint decisions vars c optanoModel |> ignore


    let private buildSolution (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (optanoSolution: Solution) (objective: #IObjective) =
        
        let decisionMap =
            decisions
            |> Seq.map (fun (KeyValue(decisionName, decision)) -> decision, match Dictionary.tryFind decisionName vars with | Some v -> v.Value | None -> 0.0)
            |> Map

        let solution =
            { new ISolution with
                member _.Values = decisionMap :> IReadOnlyDictionary<IDecision, float> }

        {
            DecisionResults = decisionMap
            ObjectiveResult = LinearExpression.evaluate solution objective.Expression
        }


    let private writeLPFile (optanoModel:Model) (filePath:string) =
        if System.IO.File.Exists(filePath) then
            System.IO.File.Delete(filePath)
        use outputStream = new System.IO.FileStream(filePath, System.IO.FileMode.CreateNew)
        let lpExporter = Exporter.LPExporter(outputStream)
        lpExporter.Write(optanoModel)

    let private writeMPSFile (optanoModel:Model) (filePath:string) =
        if System.IO.File.Exists(filePath) then
            System.IO.File.Delete(filePath)
        use outputStream = new System.IO.FileStream(filePath, System.IO.FileMode.CreateNew)
        let exporter = Exporter.MPSExporter(outputStream)
        exporter.Write(optanoModel)


    let private gurobi900Solve (settings:Flips.Legacy.Types.SolverSettings) (optanoModel:Model) =
        use solver = new Solver.Gurobi900.GurobiSolver()
        solver.Configuration.TimeLimit <- float settings.MaxDuration / 1000.0
        solver.Solve(optanoModel)


    let private cplex128Solve (settings:Flips.Legacy.Types.SolverSettings) (optanoModel:Model) =
        use solver = new Solver.Cplex128.CplexSolver()
        solver.Configuration.TimeLimit <- float settings.MaxDuration / 1000.0
        solver.Solve(optanoModel)


    let internal solve (solverType: OptanoSolverType) (settings: SolverSettings) (model: Flips.Types.Model) =

        let optanoModel = new Model()
        let vars = Dictionary()
        let decisions = Dictionary()
        addConstraints decisions vars model.Constraints optanoModel
        let orderedObjective = List.rev model.Objectives
        addObjectives decisions vars orderedObjective optanoModel |> ignore

        settings.WriteLPFile |> Option.iter (writeLPFile optanoModel)
        settings.WriteMPSFile |> Option.iter (writeMPSFile optanoModel)

        let optanoSolution =
            match solverType with
            | Cplex128 -> cplex128Solve settings optanoModel
            | Gurobi900 -> gurobi900Solve settings optanoModel

        match optanoSolution.ModelStatus, optanoSolution.Status with
        | Solver.ModelStatus.Feasible, (Solver.SolutionStatus.Optimal | Solver.SolutionStatus.Feasible) ->
            let solution = buildSolution decisions vars optanoSolution (List.head orderedObjective)
            Optimal solution
        | Solver.ModelStatus.Infeasible, _ ->
            SolveResult.Infeasible "The model was found to be infeasible"
        | Solver.ModelStatus.Unbounded, _ ->
            SolveResult.Unbounded "The model was found to be unbounded"
        | _ ->
            SolveResult.Unknown "The model status is unknown. Unable to solve."
