namespace Flips.Legacy

open System.Collections.Generic

open Flips
open Flips.Types
open Flips.Legacy.Internals
open Flips.Solver

module ORTools =
    open Google.OrTools.LinearSolver
    
    [<RequireQualifiedAccess>]
    type Inequality =
        | LessOrEqual
        | GreaterOrEqual

    type internal OrToolsSolverType =
        | CBC
        | GLOP

    let private createVariable (solver:Solver) (decision: #IDecision) =
        match decision.Type with
        | Boolean -> solver.MakeBoolVar(decision.Name)
        | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, decision.Name)
        | Continuous (lb, ub) -> solver.MakeNumVar(float lb, float ub, decision.Name)


    let addVariable (solver:Solver) (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (decision: #IDecision) =
        if not (vars.ContainsKey(decision.Name)) then
            let var = createVariable solver decision
            decisions.[decision.Name] <- decision
            vars.[decision.Name] <- var

    let private buildExpression solver (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (expr: #ILinearExpression) =
        let mutable exprAccumulator = LinearExpr()

        for term in expr.Terms do
            match term with
            | Constant constant ->
                exprAccumulator <- exprAccumulator + constant
            | LinearElement (coefficient, decision) ->
                addVariable solver decisions vars decision
                exprAccumulator <- exprAccumulator + (coefficient * vars.[decision.Name])

        exprAccumulator


    let private setObjective (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objective: #IObjective) (solver: Solver) =
        let expr = buildExpression solver decisions vars objective.Expression

        match objective.Sense with
        | Minimize -> solver.Minimize(expr)
        | Maximize -> solver.Maximize(expr)


    let private addEqualityConstraint (decisions: Dictionary<string, _>) (vars:Dictionary<string, Variable>) (constraintName:string) (lhs: #ILinearExpression) (rhs: #ILinearExpression) (solver:Solver) =
        let lhsExpr = buildExpression solver decisions vars lhs
        let rhsExpr = buildExpression solver decisions vars rhs
        // note: this is work around for
        // https://github.com/google/or-tools/issues/2231
        // https://github.com/matthewcrews/flips/issues/104
        let dictionary = new Dictionary<_, _>()
        let mutable num = lhsExpr.Visit(dictionary)
        num <- num + rhsExpr.DoVisit(dictionary, -1.0)
        let c = solver.MakeConstraint(0.0 - num, 0.0 - num, constraintName)
        for item in dictionary do
            c.SetCoefficient(item.Key, item.Value)


    let private addInequalityConstraint (decisions: Dictionary<string, _>) (vars:Dictionary<string, Variable>) (constraintName: string) (lhs: #ILinearExpression) (rhs: #ILinearExpression) (inequality: Inequality) (solver:Solver) =
        let lhsExpr = buildExpression solver decisions vars lhs
        let rhsExpr = buildExpression solver decisions vars rhs
        let constraintExpr = lhsExpr - rhsExpr
        
        let lb, ub =
            match inequality with
            | Inequality.LessOrEqual -> System.Double.NegativeInfinity, 0.0
            | Inequality.GreaterOrEqual -> 0.0, System.Double.PositiveInfinity

        // note: this is work around for
        // https://github.com/google/or-tools/issues/2231
        // https://github.com/matthewcrews/flips/issues/104
        let dictionary = new Dictionary<_, _>()
        let num = constraintExpr.Visit(dictionary)
        let c = solver.MakeConstraint(lb - num, ub - num, constraintName)
        for item in dictionary do
            c.SetCoefficient(item.Key, item.Value)


    let private addConstraint (decisions: Dictionary<string, _>) (vars:Dictionary<string, Variable>) (c: #IConstraint) (solver:Solver) =
        match c.Relationship with
        | Equal -> addEqualityConstraint decisions vars c.Name c.LHSExpression c.RHSExpression solver
        | LessOrEqual -> addInequalityConstraint decisions vars c.Name c.LHSExpression c.RHSExpression Inequality.LessOrEqual solver
        | GreaterOrEqual -> addInequalityConstraint decisions vars c.Name c.LHSExpression c.RHSExpression Inequality.GreaterOrEqual solver


    let private addConstraints (decisions: Dictionary<string, _>) (varMap: Dictionary<string, Variable>) (constraints: #IConstraint list) (solver:Solver) =
        for c in constraints do
            addConstraint decisions varMap c solver |> ignore


    let private buildSolution (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (solver:Solver) (objective: #IObjective) =

        let decisionMap =
            decisions
            |> Seq.map (fun (KeyValue(decisionName, decision)) -> decision, match Dictionary.tryFind decisionName vars with | Some v -> v.SolutionValue() | None -> 0.0)
            |> Map

        let solution =
            { new ISolution with
                member _.Values = decisionMap :> IReadOnlyDictionary<IDecision, float> }

        {
            DecisionResults = decisionMap
            ObjectiveResult = LinearExpression.evaluate solution objective.Expression
        }


    let private writeLPFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsLpFormat(false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let private writeMPSFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsMpsFormat(false, false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let internal solveForObjective (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objective: #IObjective) (solver: Solver) =
        setObjective decisions vars objective solver

        let resultStatus = solver.Solve()

        match resultStatus with
        | Solver.ResultStatus.OPTIMAL ->
            Result.Ok (solver,objective)
        | _ ->
            Result.Error resultStatus


    let internal addObjectiveAsConstraint (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objective: #IObjective) (objectiveValue: float) (solver: Solver) =
        let rhs = LinearExpression.OfFloat objectiveValue
        match objective.Sense with
        | Maximize ->
            addInequalityConstraint decisions vars objective.Name objective.Expression rhs Inequality.GreaterOrEqual solver
        | Minimize ->
            addInequalityConstraint decisions vars objective.Name objective.Expression rhs Inequality.LessOrEqual solver
        
        // The underlying API is mutable :/
        solver

    let rec internal solveForObjectives (decisions: Dictionary<string, _>) (vars:Dictionary<string, Variable>) (objectives: #IObjective list) (solver: Solver) =

        match objectives with
        | [] ->
            failwith "Model without Objective" // Argument should be a special type
        | objective :: [] ->
            solveForObjective decisions vars objective solver
        | objective :: remaining ->
            solveForObjective decisions vars objective solver
            |> Result.map (fun (solver,_) -> addObjectiveAsConstraint decisions vars objective (solver.Objective().BestBound()) solver)
            |> Result.bind (solveForObjectives decisions vars remaining)


    let internal solve (solverType:OrToolsSolverType) (settings: SolverSettings) (model:Flips.Types.Model) =

        let solver =
            match solverType with
            | CBC -> Solver.CreateSolver("CBC")
            | GLOP -> Solver.CreateSolver("GLOP")

        solver.SetTimeLimit(settings.MaxDuration)

        // We will enable this in the next major release
        //if settings.EnableOutput then
        //    solver.EnableOutput()
        //else
        //    solver.SuppressOutput()

        let vars = Dictionary()
        let decisions = Dictionary()
        addConstraints decisions vars model.Constraints solver
        
        model.Objectives
        |> List.tryHead 
        |> Option.iter (fun objective -> 
          // Write LP/MPS Formulation to file if requested with first objective
          setObjective decisions vars objective solver
          settings.WriteLPFile |> Option.iter (writeLPFile solver)
          settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
        )

        let result = solveForObjectives decisions vars (List.rev model.Objectives) solver

        match result with
        | Result.Ok (solver,objective) ->
            match model.Objectives with
            | firstObjective :: _ when firstObjective <> objective ->
              // Write LP/MPS Formulation to file again if requested
              // doing it again in case the solved objective isn't the first one
              settings.WriteLPFile |> Option.iter (writeLPFile solver)
              settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
            | [] | [_] | _ -> () 

            buildSolution decisions vars solver model.Objectives.[0]
            |> SolveResult.Optimal 
        | Result.Error errorStatus ->
            match errorStatus with
            | Solver.ResultStatus.INFEASIBLE ->
                SolveResult.Infeasible "The model was found to be infeasible"
            | Solver.ResultStatus.UNBOUNDED ->
                SolveResult.Unbounded "The model was found to be unbounded"
            | _ ->
                SolveResult.Unknown "The model status is unknown. Unable to solve."
