module Flips.Solve


open Google.OrTools.LinearSolver
open Flips.Domain




let private buildExpression (vars:Map<DecisionName,Variable>) (LinearExpression (names, coefs, decs, offset):LinearExpression) =
    let decisionExpr =
        names
        |> Seq.map (fun n -> coefs.[n] * vars.[n])
        |> fun x -> 
            match Seq.isEmpty x with 
            | true -> LinearExpr() 
            | false -> Seq.reduce (+) x
        
    offset + decisionExpr


let private createVariable (solver:Solver) (DecisionName name:DecisionName) (decisionType:DecisionType) =
    match decisionType with
    | Boolean -> solver.MakeBoolVar(name)
    | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, name)
    | Continuous (lb, ub) -> solver.MakeNumVar(float lb, float ub, name)


let private createVariableMap (solver:Solver) (decisions:Map<DecisionName, Decision>) =
    decisions
    |> Map.map (fun n d -> createVariable solver n d.Type)


let private setObjective (vars:Map<DecisionName, Variable>) (objective:Flips.Domain.Objective) (solver:Solver) =
    let expr = buildExpression vars objective.Expression

    match objective.Sense with
    | Minimize -> solver.Minimize(expr)
    | Maximize -> solver.Maximize(expr)


let private addConstraint (vars:Map<DecisionName, Variable>) (Constraint (lhs, comparison, rhs):Constraint) (solver:Solver) =
    let lhsExpr = buildExpression vars lhs
    let rhsExpr = buildExpression vars rhs
    let constraintExpr = lhsExpr - rhsExpr

    match comparison with
    | LessOrEqual -> 
        let c = RangeConstraint(constraintExpr, System.Double.NegativeInfinity, 0.0)
        solver.Add(c)
    | GreaterOrEqual -> 
        let c = RangeConstraint(constraintExpr, 0.0, System.Double.PositiveInfinity)
        solver.Add(c)
    | Equal -> 
        let c = Equality(lhsExpr, rhsExpr, true)
        solver.Add(c)
    |> ignore


let private addConstraints (vars:Map<DecisionName, Variable>) (constraints:List<Constraint>) (solver:Solver) =
    for c in constraints do
        addConstraint vars c solver |> ignore


let private buildSolution (vars:Map<DecisionName, Variable>) (solver:Solver) (objective:Objective) =
    let decisions =
        vars
        |> Map.toSeq
        |> Seq.map (fun (n, v) -> n, v.SolutionValue())
        |> Map.ofSeq

    {
        DecisionResults = decisions
        ObjectiveResult = solver.Objective().BestBound()
    }

let private writeLPFile (solver:Solver) (filePath:string) =
    let lpFile = solver.ExportModelAsLpFormat(false)
    System.IO.File.WriteAllText(filePath, lpFile)

let solve (settings:SolverSettings) (model:Flips.Domain.Model.Model) =
    let solver = Solver.CreateSolver("MIP Solver", "CBC_MIXED_INTEGER_PROGRAMMING")
    solver.SetTimeLimit(settings.MaxDuration)
    solver.EnableOutput()

    let vars = createVariableMap solver model.Decisions
    addConstraints vars model.Constraints solver
    setObjective vars model.Objective solver

    // Write LP Formulation to file if requested
    settings.WriteLPFile |> Option.map (writeLPFile solver) |> ignore

    let resultStatus = solver.Solve()

    match resultStatus with
    | Solver.ResultStatus.OPTIMAL -> 
        buildSolution vars solver model.Objective
        |> SolveResult.Optimal
    | _ ->
        "Unable to find optimal solution"
        |> SolveResult.Suboptimal
    
