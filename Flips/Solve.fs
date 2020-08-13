namespace Flips

open Flips.Types


module internal ORTools =
    open Google.OrTools.LinearSolver

    type internal OrToolsSolverType =
        | CBC
        | GLOP


    let private buildExpression (varMap:Map<DecisionName,Variable>) (expr:LinearExpression) =
        let reducedExpr = Flips.Types.LinearExpression.Reduce expr
        
        let decisionExpr =
            reducedExpr.Coefficients
            |> Seq.map (fun kvp -> kvp.Value * varMap.[kvp.Key])
            |> fun x -> 
                match Seq.isEmpty x with 
                | true -> LinearExpr() 
                | false -> Seq.reduce (+) x
        
        let resultExpr = reducedExpr.Offset + decisionExpr
        resultExpr


    let private createVariable (solver:Solver) (DecisionName name:DecisionName) (decisionType:DecisionType) =
        match decisionType with
        | Boolean -> solver.MakeBoolVar(name)
        | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, name)
        | Continuous (lb, ub) -> solver.MakeNumVar(float lb, float ub, name)


    let private createVariableMap (solver:Solver) (decisions:seq<Decision>) =
        decisions
        |> Seq.map (fun d -> d.Name, d.Type)
        |> Map.ofSeq
        |> Map.map (fun name decisionType -> createVariable solver name decisionType)


    let private setObjective (varMap:Map<DecisionName, Variable>) (objective:Flips.Types.Objective) (solver:Solver) =
        let expr = buildExpression varMap objective.Expression

        match objective.Sense with
        | Minimize -> solver.Minimize(expr)
        | Maximize -> solver.Maximize(expr)


    let private addEqualityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (solver:Solver) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs
        let c = Google.OrTools.LinearSolver.Equality(lhsExpr, rhsExpr, true)
        solver.Add(c)


    let private addInequalityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (inequality:Inequality) (solver:Solver) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs
        let constraintExpr = lhsExpr - rhsExpr

        match inequality with
        | LessOrEqual -> 
            let c = RangeConstraint(constraintExpr, System.Double.NegativeInfinity, 0.0)
            solver.Add(c)
        | GreaterOrEqual -> 
            let c = RangeConstraint(constraintExpr, 0.0, System.Double.PositiveInfinity)
            solver.Add(c)


    let private addConstraint (varMap:Map<DecisionName, Variable>) (c:Types.Constraint) (solver:Solver) =
        match c.Expression with
        | Equality (lhs, rhs) -> addEqualityConstraint varMap c.Name lhs rhs solver
        | Inequality (lhs, inequality, rhs) -> addInequalityConstraint varMap c.Name lhs rhs inequality solver


    let private addConstraints (varMap:Map<DecisionName, Variable>) (constraints:List<Types.Constraint>) (solver:Solver) =
        for c in constraints do
            addConstraint varMap c solver |> ignore


    let private buildSolution (decisions:seq<Decision>) (varMap:Map<DecisionName, Variable>) (solver:Solver) (objective:Types.Objective) =
        let decisionMap =
            decisions
            |> Seq.map (fun d -> 
                            match Map.tryFind d.Name varMap with 
                            | Some var -> d, var.SolutionValue() 
                            | None -> d, 0.0)
            |> Map.ofSeq

        {
            DecisionResults = decisionMap
            ObjectiveResult = solver.Objective().BestBound()
        }


    let private writeLPFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsLpFormat(false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let internal solve (solverType:OrToolsSolverType) (settings:SolverSettings) (model:Flips.Model.Model) =

        let solver = 
            match solverType with
            | CBC -> Solver.CreateSolver("MIP Solver", "CBC_MIXED_INTEGER_PROGRAMMING")
            | GLOP -> Solver.CreateSolver("LP Solver", "GLOP_LINEAR_PROGRAMMING")

        solver.SetTimeLimit(settings.MaxDuration)
        solver.EnableOutput()
    
        let vars = createVariableMap solver (Model.getDecisions model)
        addConstraints vars model.Constraints solver
        setObjective vars model.Objective solver
    
        Result.Ok ()
        //// Write LP Formulation to file if requested
        //settings.WriteLPFile |> Option.map (writeLPFile solver) |> ignore
    
        //let resultStatus = solver.Solve()
    
        //match resultStatus with
        //| Solver.ResultStatus.OPTIMAL -> 
        //    buildSolution (Model.getDecisions model) vars solver model.Objective
        //    |> SolveResult.Optimal
        //| _ ->
        //    "Unable to find optimal solution"
        //    |> SolveResult.Suboptimal


module internal Optano =
    open OPTANO.Modeling.Optimization

    type internal OptanoSolverType =
        | Cplex128
        | Gurobi900


    let private buildExpression (varMap:Map<DecisionName, Variable>) (expr:LinearExpression) =
        let reducedExpr = Flips.Types.LinearExpression.Reduce expr

        let constant = Expression.Sum([reducedExpr.Offset])
        let variables =
            reducedExpr.Coefficients
            |> Seq.map (fun kvp -> kvp.Value * varMap.[kvp.Key])
            |> (fun terms -> Expression.Sum(terms))

        constant + variables


    let private createVariable (decision:Decision) =
        let (DecisionName name) = decision.Name
        match decision.Type with
        | Boolean -> new Variable(name, 0.0, 1.0, Enums.VariableType.Binary)
        | Integer (lb, ub) -> new Variable(name, lb, ub, Enums.VariableType.Integer)
        | Continuous (lb, ub) -> new Variable(name, lb, ub, Enums.VariableType.Continuous)
            

    let private createVariableMap (decisions:seq<Decision>) =
        decisions
        |> Seq.map (fun d -> d.Name, d)
        |> Map.ofSeq
        |> Map.map (fun _ d -> createVariable d)


    let private setObjective (varMap:Map<DecisionName, Variable>) (objective:Flips.Types.Objective) (optanoModel:Model) =
        let (ObjectiveName name) = objective.Name
        let expr = buildExpression varMap objective.Expression

        let sense = 
            match objective.Sense with
            | Minimize -> Enums.ObjectiveSense.Minimize
            | Maximize -> Enums.ObjectiveSense.Maximize

        let optanoObjective = new Objective(expr, name, sense)
        optanoModel.AddObjective(optanoObjective)


    let private addEqualityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (optanoModel:Model) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs
        let c = Constraint.Equals(lhsExpr, rhsExpr)
        optanoModel.AddConstraint(c, n)


    let private addInequalityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (inequality:Inequality) (optanoModel:Model) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs

        let c = 
            match inequality with
            | LessOrEqual -> 
                Constraint.LessThanOrEqual(lhsExpr, rhsExpr)
            | GreaterOrEqual -> 
                Constraint.GreaterThanOrEqual(lhsExpr, rhsExpr)

        optanoModel.AddConstraint(c, n)


    let private addConstraint (varMap:Map<DecisionName, Variable>) (c:Types.Constraint) (optanoModel:Model) =
        match c.Expression with
        | Equality (lhs, rhs) -> addEqualityConstraint varMap c.Name lhs rhs optanoModel
        | Inequality (lhs, inequality, rhs) -> addInequalityConstraint varMap c.Name lhs rhs inequality optanoModel


    let private addConstraints (vars:Map<DecisionName, Variable>) (constraints:List<Types.Constraint>) (optanoModel:Model) =
        for c in constraints do
            addConstraint vars c optanoModel |> ignore


    let private buildSolution (decisions:seq<Decision>) (varMap:Map<DecisionName, Variable>) (optanoSolution:Solution) =
        let decisionMap =
            decisions
            |> Seq.map (fun d -> 
                            match Map.tryFind d.Name varMap with 
                            | Some var -> d, var.Value 
                            | None -> d, 0.0)
            |> Map.ofSeq

        {
            DecisionResults = decisionMap
            ObjectiveResult = optanoSolution.BestBound
        }


    let private writeLPFile (optanoModel:Model) (filePath:string) =
        if System.IO.File.Exists(filePath) then
            System.IO.File.Delete(filePath)
        use outputStream = new System.IO.FileStream(filePath, System.IO.FileMode.CreateNew)
        let lpExporter = Exporter.LPExporter(outputStream)
        lpExporter.Write(optanoModel)


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
        let varMap = createVariableMap (Model.getDecisions model)
        addConstraints varMap model.Constraints optanoModel
        setObjective varMap model.Objective optanoModel

        let optanoSolution = 
            match solverType with
            | Cplex128 -> cplex128Solve settings optanoModel
            | Gurobi900 -> gurobi900Solve settings optanoModel

        match optanoSolution.ModelStatus, optanoSolution.Status with
        | Solver.ModelStatus.Infeasible, _ -> Suboptimal "Model is infeasible"
        | Solver.ModelStatus.InfOrUnbd, _ -> Suboptimal "Model is infeasible or unbounded"
        | Solver.ModelStatus.Unbounded, _ -> Suboptimal "Model is unbounded"
        | Solver.ModelStatus.Unknown, _ -> Suboptimal "Model status is unknown"
        | Solver.ModelStatus.Feasible, (Solver.SolutionStatus.Optimal | Solver.SolutionStatus.Feasible) -> 
            let solution = buildSolution (Model.getDecisions model) varMap optanoSolution
            Optimal solution
        | _ -> Suboptimal "Model state is undetermined"


[<RequireQualifiedAccess>]
module Solver =

    let solve (settings:SolverSettings) (model:Flips.Model.Model) =

        match settings.SolverType with
        | CBC -> ORTools.solve ORTools.OrToolsSolverType.CBC settings model
        //| GLOP -> ORTools.solve ORTools.OrToolsSolverType.GLOP settings model
        //| Cplex128 -> Optano.solve Optano.OptanoSolverType.Cplex128 settings model
        //| Gurobi900 -> Optano.solve Optano.OptanoSolverType.Gurobi900 settings model

    
