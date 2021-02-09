namespace Flips.Solver.CBC

open System.Collections.Generic
open Flips
open Google.OrTools.LinearSolver

module internal Dictionary =

    let tryFind k (d:Dictionary<_,_>) =
        match d.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None


[<RequireQualifiedAccess>]
module internal Math =

    let kahanSum (xs: float list) =
        let rec kahanSumAux (xs: float list) (sum: float) (c: float) =
            match xs with
            | [] -> sum
            | x::xs ->
            let y = x - c in
            let t = sum + y in
            let c = (t - sum) - y in
            kahanSumAux xs t c

        match xs with
        | [] -> 0.0
        | _ -> kahanSumAux xs 0.0 0.0


module internal ORTools =

    [<RequireQualifiedAccess>]
    type Inequality =
        | LessOrEqual
        | GreaterOrEqual

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

        vars.[decision.Name]


    let private setObjective (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (objective: #IObjective) (solver: Solver) =
        let decisionCoefficients = Dictionary<IDecision, float list>()
        let mutable offsets = []

        for term in objective.Expression.Terms do
            match term with
            | Constant c ->
                offsets <- c::offsets
            | LinearElement (c, d) ->
                match Dictionary.tryFind d decisionCoefficients with
                | Some coefs -> 
                    decisionCoefficients.[d] <- c::coefs
                | None ->
                    decisionCoefficients.Add (d, [c])

        let mutable exprAccumulator = LinearExpr()

        for KeyValue(decision, coefficients) in decisionCoefficients do
            let coefficient = Math.kahanSum coefficients
            addVariable solver decisions vars decision |> ignore
            exprAccumulator <- exprAccumulator + (coefficient * vars.[decision.Name])

        exprAccumulator <- exprAccumulator + (Math.kahanSum offsets)

        match objective.Sense with
        | Minimize -> solver.Minimize exprAccumulator
        | Maximize -> solver.Maximize exprAccumulator


    let private addConstraint (decisions: Dictionary<string, _>) (vars:Dictionary<string, Variable>) (constraintName: string) (lhs: #ILinearExpression) (rhs: #ILinearExpression) (relationship: Relationship) (solver:Solver) =
        let decisionCoefficients = Dictionary<IDecision, float list>()
        let mutable offsets = []

        for term in lhs.Terms do
            match term with
            | Constant c ->
                offsets <- (-1.0 * c)::offsets // Because we will move it to the RHS
            | LinearElement (c, d) ->
                match Dictionary.tryFind d decisionCoefficients with
                | Some coefs -> 
                    decisionCoefficients.[d] <- c::coefs
                | None ->
                    decisionCoefficients.Add (d, [c])

        for term in rhs.Terms do
            match term with
            | Constant c ->
                offsets <- c::offsets
            | LinearElement (c, d) ->
                match Dictionary.tryFind d decisionCoefficients with
                | Some coefs -> 
                    decisionCoefficients.[d] <- (-1.0 * c)::coefs
                | None ->
                    decisionCoefficients.Add (d, [-1.0 * c])
     
        let rhsConstant = Math.kahanSum offsets

        let newConstraint =
            match relationship with
            | Equal ->
                solver.MakeConstraint(rhsConstant, rhsConstant, constraintName)
            | LessOrEqual -> 
                solver.MakeConstraint(System.Double.NegativeInfinity, rhsConstant, constraintName)
            | GreaterOrEqual -> 
                solver.MakeConstraint(rhsConstant, System.Double.PositiveInfinity, constraintName)
        
        for KeyValue(decision, coefficients) in decisionCoefficients do
            let coefficient = Math.kahanSum coefficients
            let variable = addVariable solver decisions vars decision
            newConstraint.SetCoefficient(variable, coefficient)


    let private addConstraints (decisions: Dictionary<string, _>) (varMap: Dictionary<string, Variable>) (constraints: seq<#IConstraint>) (solver:Solver) =
        for c in constraints do
            addConstraint decisions varMap c.Name c.LHSExpression c.RHSExpression c.Relationship solver |> ignore


    let private buildSolution (decisions: Dictionary<string, _>) (vars: Dictionary<string, Variable>) (solver:Solver) : Flips.Solver.ISolution =
        
        let decisionMap =
            decisions
            |> Seq.map (fun (KeyValue(decisionName, decision)) -> decision, match Dictionary.tryFind decisionName vars with | Some v -> v.SolutionValue() | None -> 0.0)
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
            addConstraint decisions vars objective.Name objective.Expression rhs Relationship.GreaterOrEqual solver
        | Minimize ->
            addConstraint decisions vars objective.Name objective.Expression rhs Relationship.LessOrEqual solver
    
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


    let internal solve (settings: Settings) (model: #IModel) : Result<Flips.Solver.ISolution, SolverError> =

        let solver = 
            // Setting the SolverType between CBC which can handle MIP and GLOP which is strictly LP
            match settings.SolverType with
            | CBC ->
                Solver.CreateSolver("CBC")
            | GLOP ->
                Solver.CreateSolver("GLOP")

        solver.SetTimeLimit(settings.MaxDuration)

        // Turning console output on or off
        if settings.EnableOutput then
            solver.EnableOutput()
        else
            solver.SuppressOutput()

        // A dictionary for tracking the Variables that are created for DecisionName
        let vars = Dictionary()
        // A dictionary for mapping DecisionName to Decision
        let decisions = Dictionary()
        addConstraints decisions vars model.Constraints solver
        
        model.Objectives
        |> List.tryHead 
        |> Option.iter (fun objective -> 
          // Write LP/MPS Formulation to file if requested with first objective
          // This should really be called before each call to the `solveForObjective` function
          settings.WriteLPFile |> Option.iter (writeLPFile solver)
          settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
          setObjective decisions vars objective solver
        )

        SolverError.Unknown "The model status is unknown. Unable to solve."
        |> Result.Error

        //let result = solveForObjectives decisions vars (List.rev model.Objectives) solver

        //match result with
        //| Result.Ok (solver, objective) ->
        //    match model.Objectives with
        //    | firstObjective :: _ when firstObjective <> objective ->
        //      // Write LP/MPS Formulation to file again if requested
        //      // doing it again in case the solved objective isn't the first one
        //      settings.WriteLPFile |> Option.iter (writeLPFile solver)
        //      settings.WriteMPSFile |> Option.iter (writeMPSFile solver)
        //    | [] | [_] | _ -> () 

        //    buildSolution decisions vars solver
        //    |> Result.Ok
        //| Result.Error errorStatus ->
        //    match errorStatus with
        //    | Solver.ResultStatus.INFEASIBLE ->
        //        SolverError.Infeasible "The model was found to be infeasible"
        //        |> Result.Error
        //    | Solver.ResultStatus.UNBOUNDED ->
        //        SolverError.Unbounded "The model was found to be unbounded"
        //        |> Result.Error
        //    | _ ->
        //        SolverError.Unknown "The model status is unknown. Unable to solve."
        //        |> Result.Error


type Solver (settings:Settings) =

    let settings = settings
    
    member _.Solve (model: #IModel) =
        ORTools.solve settings model
