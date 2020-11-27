namespace Flips.Solver.CBC

open System.Collections.Generic
open Flips
open Google.OrTools.LinearSolver

module internal Dictionary =

    let tryFind k (d:Dictionary<_,_>) =
        match d.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None


module internal ORTools =

    let private createVariable (solver:Solver) (DecisionName name:DecisionName) (decisionType:DecisionType) =
        match decisionType with
        | Boolean -> solver.MakeBoolVar(name)
        | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, name)
        | Continuous (lb, ub) -> solver.MakeNumVar(float lb, float ub, name)


    let addVariable (solver:Solver) decisionName (decisionType:DecisionType) (vars:Dictionary<DecisionName, Variable>) =
        if not (vars.ContainsKey(decisionName)) then
            let var = createVariable solver decisionName decisionType
            vars.[decisionName] <- var

    let private buildExpression solver (vars:Dictionary<DecisionName,Variable>) (expr:LinearExpression) =
        let reducedExpr = Flips.Types.LinearExpression.Reduce expr

        let decisionExpr =
            reducedExpr.Coefficients
            |> Seq.map (fun kvp ->
                          addVariable solver kvp.Key reducedExpr.DecisionTypes.[kvp.Key] vars
                          kvp.Value * vars.[kvp.Key])
            |> Array.ofSeq

        let offsetExpr = LinearExpr() + reducedExpr.Offset
        let resultExpr = decisionExpr.Sum() + offsetExpr
        resultExpr


    let private setObjective (vars:Dictionary<DecisionName, Variable>) (objective:Flips.Types.Objective) (solver:Solver) =
        let expr = buildExpression solver vars objective.Expression

        match objective.Sense with
        | Minimize -> solver.Minimize(expr)
        | Maximize -> solver.Maximize(expr)


    let private addEqualityConstraint (vars:Dictionary<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (solver:Solver) =
        let lhsExpr = buildExpression solver vars lhs
        let rhsExpr = buildExpression solver vars rhs
        // note: this is work around for
        // https://github.com/google/or-tools/issues/2231
        // https://github.com/matthewcrews/flips/issues/104
        let dictionary = new Dictionary<_, _>()
        let mutable num = lhsExpr.Visit(dictionary)
        num <- num + rhsExpr.DoVisit(dictionary, -1.0)
        let c = solver.MakeConstraint(0.0 - num, 0.0 - num, n)
        for item in dictionary do
            c.SetCoefficient(item.Key, item.Value)


    let private addInequalityConstraint (vars:Dictionary<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (inequality:Inequality) (solver:Solver) =
        let lhsExpr = buildExpression solver vars lhs
        let rhsExpr = buildExpression solver vars rhs
        let constraintExpr = lhsExpr - rhsExpr
        
        let lb, ub =
            match inequality with
            | LessOrEqual -> System.Double.NegativeInfinity, 0.0
            | GreaterOrEqual -> 0.0, System.Double.PositiveInfinity

        // note: this is work around for
        // https://github.com/google/or-tools/issues/2231
        // https://github.com/matthewcrews/flips/issues/104
        let dictionary = new Dictionary<_, _>()
        let num = constraintExpr.Visit(dictionary)
        let c = solver.MakeConstraint(lb - num, ub - num, n)
        for item in dictionary do
            c.SetCoefficient(item.Key, item.Value)


    let private addConstraint (vars:Dictionary<DecisionName, Variable>) (c:Types.Constraint) (solver:Solver) =
        match c.Expression with
        | Equality (lhs, rhs) -> addEqualityConstraint vars c.Name lhs rhs solver
        | Inequality (lhs, inequality, rhs) -> addInequalityConstraint vars c.Name lhs rhs inequality solver


    let private addConstraints (varMap:Dictionary<DecisionName, Variable>) (constraints:FSharp.Collections.List<Types.Constraint>) (solver:Solver) =
        for c in constraints do
            addConstraint varMap c solver |> ignore


    let private buildSolution (decisions:seq<Decision>) (vars:Dictionary<DecisionName, Variable>) (solver:Solver) : Flips.Solver.ISolution =
        
        let decisionMap =
            decisions
            |> Seq.map (fun d ->
                            match Dictionary.tryFind d.Name vars with
                            | Some (var: Variable) -> d, var.SolutionValue()
                            | None -> d, 0.0)
            |> readOnlyDict

        { new Flips.Solver.ISolution 
            with member _.Values = decisionMap
        }


    let private writeLPFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsLpFormat(false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let private writeMPSFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsMpsFormat(false, false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let internal solveForObjective (vars:Dictionary<DecisionName, Variable>) (objective:Flips.Types.Objective) (solver:Solver) =
        setObjective vars objective solver

        let resultStatus = solver.Solve()

        match resultStatus with
        | Solver.ResultStatus.OPTIMAL ->
            Result.Ok (solver,objective)
        | _ ->
            Result.Error resultStatus



    let internal addObjectiveAsConstraint (vars:Dictionary<DecisionName, Variable>) (objective:Flips.Types.Objective) (objectiveValue:float) (solver:Solver) =
        let objectiveAsConstraint =
            let (ObjectiveName name) = objective.Name
            match objective.Sense with
            | Maximize ->
                Constraint.create name (objective.Expression >== objectiveValue)
            | Minimize ->
                Constraint.create name (objective.Expression <== objectiveValue)

        // The underlying API is mutable :/
        addConstraint vars objectiveAsConstraint solver |> ignore
        solver

    let rec internal solveForObjectives (vars:Dictionary<DecisionName, Variable>) (objectives:Flips.Types.Objective list) (solver:Solver) =

        match objectives with
        | [] ->
            failwith "Model without Objective" // Argument should be a special type
        | objective :: [] ->
            solveForObjective vars objective solver
        | objective :: remaining ->
            solveForObjective vars objective solver
            |> Result.map (fun (solver,_) -> addObjectiveAsConstraint vars objective (solver.Objective().BestBound()) solver)
            |> Result.bind (solveForObjectives vars remaining)


    let internal solve (settings:Settings) (model:Flips.Types.Model) : Result<Flips.Solver.ISolution, SolverError> =

        let solver = Solver.CreateSolver("CBC")

        solver.SetTimeLimit(settings.MaxDuration)

        // Turning console output on or off
        if settings.EnableOutput then
            solver.EnableOutput()
        else
            solver.SuppressOutput()

        let vars = Dictionary()
        addConstraints vars model.Constraints solver
        
        model.Objectives
        |> List.tryHead 
        |> Option.iter (fun objective -> 
          // Write LP/MPS Formulation to file if requested with first objective
          // This should really be called before each call to the `solveForObjective` function
          settings.WriteLPFile |> Option.iter (writeLPFile solver)
          settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
          setObjective vars objective solver
        )

        let result = solveForObjectives vars (List.rev model.Objectives) solver

        match result with
        | Result.Ok (solver, objective) ->
            match model.Objectives with
            | firstObjective :: _ when firstObjective <> objective ->
              // Write LP/MPS Formulation to file again if requested
              // doing it again in case the solved objective isn't the first one
              settings.WriteLPFile |> Option.iter (writeLPFile solver)
              settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
            | [] | [_] | _ -> () 

            buildSolution (Model.getDecisions model) vars solver
            |> Result.Ok
        | Result.Error errorStatus ->
            match errorStatus with
            | Solver.ResultStatus.INFEASIBLE ->
                SolverError.Infeasible "The model was found to be infeasible"
                |> Result.Error
            | Solver.ResultStatus.UNBOUNDED ->
                SolverError.Unbounded "The model was found to be unbounded"
                |> Result.Error
            | _ ->
                SolverError.Unknown "The model status is unknown. Unable to solve."
                |> Result.Error


type Solver (settings:Settings) =

    let settings = settings
    
    member _.Solve (model:Flips.Types.Model) =
      ORTools.solve settings model

    interface Flips.Solver.ISolver<SolverError> with
        member _.Solve (model:Flips.Types.Model) =
            ORTools.solve settings model