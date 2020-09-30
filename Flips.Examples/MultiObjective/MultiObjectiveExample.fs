module Flips.Examples.MultiObjective

open Flips
open Flips.Types
open Flips.SliceMap

type Job = Job of int
type Machine = Machine of int

let solve () =
    let rng = System.Random(123)
    // Declare the parameters for our model
    let maxAssignments = 5.0
    let maxMachineAssignments = 2.0
    let jobs = [1..9] |> List.map Job
    let machines = [1..5] |> List.map Machine
    let jobRevenue =
        jobs
        |> List.map (fun j -> j, float (rng.Next(5, 10)))
        |> SMap.ofList

    let jobMachineWaste =
        [for j in jobs do
            for m in machines ->
                (j, m), float (rng.Next(1, 5))
        ] |> SMap2.ofList

    let assignments =
        DecisionBuilder "Assignment" {
            for j in jobs do
                for m in machines ->
                    Boolean
        } |> SMap2.ofSeq

    // Sum the total revenue for the jobs selected
    let revenueExpr = sum (jobRevenue .* assignments)

    // Sum the waste for running a job on a particular machine
    let wasteExpr = sum (assignments .* jobMachineWaste)

    // Create an objective to maximize the revenue
    let revenueObjective = Objective.create "MaxRevenue" Maximize revenueExpr

    // Create an objective to minimize the waste
    let wasteObjective = Objective.create "MinWaste" Minimize wasteExpr

    // Create a constraint which limits the number of jobs that can be assigned
    let assignmentConstraint =
        Constraint.create "MaxNumberOfAssignments" (sum assignments <== maxAssignments)

    // Create constraints for a job only being assigned once
    let oneAssignmentConstraints =
        ConstraintBuilder "OneAssignment" {
            for j in jobs ->
                sum assignments.[j, All] <== 1.0
        }

    // Create constraints so that no machine can have more than max number of jobs
    let machineAssignmentConstraints =
        ConstraintBuilder "MaxMachineAssignement" {
            for m in machines ->
                sum assignments.[All, m] <== maxMachineAssignments
        }

    let model =
        Model.create revenueObjective
        |> Model.addObjective wasteObjective
        |> Model.addConstraint assignmentConstraint
        |> Model.addConstraints machineAssignmentConstraints
        |> Model.addConstraints oneAssignmentConstraints


    // Create a Settings type which tells the Solver which types of underlying solver to use,
    // the time alloted for solving, and whether to write an LP file to disk
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000L
        WriteLPFile = None
    }

    // Call the `solve` function in the Solve module to evaluate the model
    let result = Solver.solve settings model

    printfn "-- Result --"

    // Match the result of the call to solve
    // If the model could not be solved it will return a `Suboptimal` case with a message as to why
    // If the model could be solved, it will print the value of the Objective Function and the
    // values for the Decision Variables
    match result with
    | Optimal solution ->
        let revenueResult = Solution.evaluate solution revenueExpr

        printfn "Revenue: %f" revenueResult
        
        printfn "Waste: %f" solution.ObjectiveResult

        for (decision, value) in solution.DecisionResults |> Map.toSeq do
            printfn "Decision: %A\tValue: %f" decision.Name value
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
