module Flips.Solve


open Google.OrTools.LinearSolver
open Flips.Domain




let private buildExpression (vars:Map<DecisionName,Variable>) (LinearExpression (names, coefs, decs, offset):LinearExpression) =
    let decisionExpr =
        names
        |> Seq.map (fun n -> coefs.[n] * vars.[n])
        |> fun x -> 
            match Seq.isEmpty x with 
            | true -> new LinearExpr() 
            | false -> Seq.reduce (+) x
        
    offset + decisionExpr


let private createVariable (solver:Solver) (DecisionName name:DecisionName) (decisionType:DecisionType) =
    match decisionType with
    | Boolean -> solver.MakeBoolVar(name)
    | Integer (lb, ub) -> solver.MakeIntVar(lb, ub, name)
    | Continuous (lb, ub) -> solver.MakeNumVar(lb, ub, name)


let private createVariableMap (solver:Solver) (decisions:Map<DecisionName, Decision>) =
    decisions
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (fun decision -> decision, createVariable solver decision.Name decision.Type)
    |> Map.ofSeq


let private setObjective (vars:Map<Decision, Variable>) (objective:Flips.Domain.Objective) (solver:Solver) =
    let expr = buildExpression vars objective.Expression

    match objective.Sense with
    | Minimize -> solver.Minimize(expr)
    | Maximize -> solver.Maximize(expr)


let private addConstraint (vars:Map<Decision, Variable>) (Constraint (lhs, comparison, rhs):Constraint) (solver:Solver) =
    let lhsExpr = buildExpression vars lhs
    let rhsExpr = buildExpression vars rhs
    let constraintExpr = lhsExpr - rhsExpr

    match comparison with
    | LessOrEqual -> 
        let c = new RangeConstraint(constraintExpr, System.Double.NegativeInfinity, 0.0)
        solver.Add(c)
    | GreaterOrEqual -> 
        let c = new RangeConstraint(constraintExpr, 0.0, System.Double.PositiveInfinity)
        solver.Add(c)
    | Equal -> 
        let c = new Equality(lhsExpr, rhsExpr, true)
        solver.Add(c)
    |> ignore


let private addConstraints (vars:Map<Decision, Variable>) (constraints:List<Constraint>) (solver:Solver) =
    for c in constraints do
        addConstraint vars c solver |> ignore


let private buildSolution (vars:Map<Decision, Variable>) (solver:Solver) (objective:Objective) =
    let decisions =
        vars
        |> Map.toSeq
        |> Seq.map (fun (d, v) -> d.Name, v.SolutionValue())
        |> Map.ofSeq

    let objectiveResults = Map.ofList [(objective, solver.Objective().BestBound())]

    {
        DecisionResults = decisions
        ObjectiveResults = objectiveResults
    }


let solve (settings:SolverSettings) (model:Flips.Domain.Model.Model) =
    let solver = Solver.CreateSolver("MIP Solver", "CBC_MIXED_INTEGER_PROGRAMMING")
    solver.SetTimeLimit(settings.MaxDuration)
    solver.EnableOutput()


    let vars = createVariableMap solver model.Decisions
    addConstraints vars model.Constraints solver


    // TODO Update to support multiobjective
    setObjective vars model.Objectives.[0] solver
