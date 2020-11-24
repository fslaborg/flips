namespace Flips.FlipsVersion2.Internals


open System.Collections.Generic

open Flips
open Flips.Types
open Flips.FlipsVersion2.Types
open Flips.FlipsVersion2.Internals


module Optano =
    open OPTANO.Modeling.Optimization
    
    type internal OptanoSolverType =
        | Cplex128
        | Gurobi900


    let private createVariable (DecisionName name:DecisionName) (decisionType:DecisionType) =
        match decisionType with
        | Boolean -> new Variable(name, 0.0, 1.0, Enums.VariableType.Binary)
        | Integer (lb, ub) -> new Variable(name, lb, ub, Enums.VariableType.Integer)
        | Continuous (lb, ub) -> new Variable(name, lb, ub, Enums.VariableType.Continuous)


    let addVariable decisionName (decisionType:DecisionType) (vars:Dictionary<DecisionName, Variable>) =
        if not (vars.ContainsKey(decisionName)) then
            let var = createVariable decisionName decisionType
            vars.[decisionName] <- var


    let private buildExpression (vars:Dictionary<DecisionName,Variable>) (expr:LinearExpression) =
        let reducedExpr = Flips.Types.LinearExpression.Reduce expr

        let constantExpr = Expression.Sum([reducedExpr.Offset])
        let decisionExpr =
            reducedExpr.Coefficients
            |> Seq.map (fun kvp ->
                          addVariable kvp.Key reducedExpr.DecisionTypes.[kvp.Key] vars
                          kvp.Value * vars.[kvp.Key])
            |> (fun terms -> Expression.Sum(terms))

        constantExpr + decisionExpr


    let private addObjective (vars:Dictionary<DecisionName, Variable>) (objective:Flips.Types.Objective) priority (optanoModel:Model) =
        let (ObjectiveName name) = objective.Name
        let expr = buildExpression vars objective.Expression

        let sense =
            match objective.Sense with
            | Minimize -> Enums.ObjectiveSense.Minimize
            | Maximize -> Enums.ObjectiveSense.Maximize

        let optanoObjective = new Objective(expr, name, sense, priority)
        optanoModel.AddObjective(optanoObjective)

    let private addObjectives (vars:Dictionary<DecisionName, Variable>) (objectives:Flips.Types.Objective list) (optanoModel:Model) =
        let numOfObjectives = objectives.Length

        objectives
        // The order of the objectives is the priority order
        // OPTANO sorts objectives by PriorityLevel desc
        |> List.iteri (fun i objective -> addObjective vars objective (numOfObjectives - i) optanoModel)

        optanoModel


    let private addEqualityConstraint (vars:Dictionary<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (optanoModel:Model) =
        let lhsExpr = buildExpression vars lhs
        let rhsExpr = buildExpression vars rhs
        let c = Constraint.Equals(lhsExpr, rhsExpr)
        optanoModel.AddConstraint(c, n)


    let private addInequalityConstraint (vars:Dictionary<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (inequality:Inequality) (optanoModel:Model) =
        let lhsExpr = buildExpression vars lhs
        let rhsExpr = buildExpression vars rhs

        let c =
            match inequality with
            | LessOrEqual ->
                Constraint.LessThanOrEqual(lhsExpr, rhsExpr)
            | GreaterOrEqual ->
                Constraint.GreaterThanOrEqual(lhsExpr, rhsExpr)

        optanoModel.AddConstraint(c, n)


    let private addConstraint (vars:Dictionary<DecisionName, Variable>) (c:Types.Constraint) (optanoModel:Model) =
        match c.Expression with
        | Equality (lhs, rhs) -> addEqualityConstraint vars c.Name lhs rhs optanoModel
        | Inequality (lhs, inequality, rhs) -> addInequalityConstraint vars c.Name lhs rhs inequality optanoModel


    let private addConstraints (vars:Dictionary<DecisionName, Variable>) (constraints:FSharp.Collections.List<Types.Constraint>) (optanoModel:Model) =
        for c in constraints do
            addConstraint vars c optanoModel |> ignore



    let private buildSolution (decisions:seq<Decision>) (vars:Dictionary<DecisionName, Variable>) (optanoSolution:Solution) (objective:Flips.Types.Objective) =
        let decisionMap =
            decisions
            |> Seq.map (fun d ->
                            match Dictionary.tryFind d.Name vars with
                            | Some var -> d, var.Value
                            | None -> d, 0.0)
            |> Map.ofSeq

        {
            DecisionResults = decisionMap
            ObjectiveResult = Flips.Types.LinearExpression.Evaluate decisionMap objective.Expression
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


    let private gurobi900Solve (settings:Types.SolverSettings) (optanoModel:Model) =
        use solver = new Solver.Gurobi900.GurobiSolver()
        solver.Configuration.TimeLimit <- float settings.MaxDuration / 1000.0
        solver.Solve(optanoModel)


    let private cplex128Solve (settings:Types.SolverSettings) (optanoModel:Model) =
        use solver = new Solver.Cplex128.CplexSolver()
        solver.Configuration.TimeLimit <- float settings.MaxDuration / 1000.0
        solver.Solve(optanoModel)


    let internal solve (solverType:OptanoSolverType) (settings:SolverSettings) (model:Flips.Model.Model) =

        let optanoModel = new Model()
        let vars = Dictionary()
        addConstraints vars model.Constraints optanoModel
        let orderedObjective = List.rev model.Objectives
        addObjectives vars orderedObjective optanoModel |> ignore

        settings.WriteLPFile |> Option.iter (writeLPFile optanoModel)
        settings.WriteMPSFile |> Option.iter (writeMPSFile optanoModel)

        let optanoSolution =
            match solverType with
            | Cplex128 -> cplex128Solve settings optanoModel
            | Gurobi900 -> gurobi900Solve settings optanoModel

        match optanoSolution.ModelStatus, optanoSolution.Status with
        | Solver.ModelStatus.Feasible, (Solver.SolutionStatus.Optimal | Solver.SolutionStatus.Feasible) ->
            let solution = buildSolution (Model.getDecisions model) vars optanoSolution (List.head orderedObjective)
            Optimal solution
        | Solver.ModelStatus.Infeasible, _ ->
            SolveResult.Infeasible "The model was found to be infeasible"
        | Solver.ModelStatus.Unbounded, _ ->
            SolveResult.Unbounded "The model was found to be unbounded"
        | _ ->
            SolveResult.Unknown "The model status is unknown. Unable to solve."
