open System
open FlipsExampleStocks
open FlipsExample
open Flips.Domain

[<EntryPoint>]
let main argv =
    printfn "Hello Flips Example!"

    // stocks
    let items =  ["AAPL"; "ADBE"; "CVX"; "GOOG"; "IBM"; "MDLZ"; "MSFT"; "NFLX"; "ORCL"; "SBUX" ]

    // get initial data directly from finance.yahoo.com using CSV data provider
    let startDate = DateTime(2005, 1, 1) 
    let endDate = DateTime(2020, 1, 31) 
    let stockData = getStockData items startDate endDate   

    let returns = 
        [for ticker, mean, min in stockData do 
            ticker, mean] 
        |> Map.ofList
    
    let risks = 
        [for ticker, mean, min in stockData do 
            ticker, -min] 
        |> Map.ofList       
     
    // Decision - solver will change this vector to find minimal risk
    let weights =
          [for item in items do
              item, Decision.createContinuous (sprintf "Portfolio Share: %s" item) 0.0 infinity]
          |> Map.ofList

    let profits = 
        [ for item in items do 
            item, weights.[item] * returns.[item]] 
        |> Map.ofList           

    // Objective
    let objectiveExpression = List.sum [for item in items -> weights.[item] * risks.[item]]
    let objective = Objective.create "MinimizeRisk" Minimize objectiveExpression

    // Max Profit Constraints: at least 2.5%
    let maxProfitConstraintExpression = List.sum [for item in items -> profits.[item]]
    let maxProfitConstraint = Constraint.create "MaxProfit" (maxProfitConstraintExpression >== 0.025)

    // Weights sum constraint: weights sum should be equal 100%
    let weightsSumConstraintExpression = List.sum [for item in items -> weights.[item] * 1.0]
    let maxWeightConstraint = Constraint.create "WeightSum" (weightsSumConstraintExpression == 1.0)

    // Weights Less than 1 constarint: each weight shuld be <= 100%
    let weightsLessThan1Constraints =
        [for item in items ->
            Constraint.create (sprintf "MaxWeight%s" item) (weights.[item] <== 1.0)]

    // Model
    let model =
        Model.create objective
        |> Model.addConstraint maxProfitConstraint
        |> Model.addConstraint maxWeightConstraint
        |> Model.addConstraints weightsLessThan1Constraints
        
    // Settings
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 1_000_000L
        WriteLPFile = None
    }

    // Solve
    let results = Flips.Solve.solve settings model

    // Printing results:
    printfn "-- Result --"
    match results with
    | Suboptimal msg -> printfn "Unable to solve. Error: %s" msg
    | Optimal solution ->
        printfn "Objective Value - Risk: %f %%" (solution.ObjectiveResult * 100.0)
    
        for (decision, value) in solution.DecisionResults |> Map.toSeq |> Seq.filter(fun (key, value) -> value > 0.0) do
            let (DecisionName name) = decision.Name
            printfn "Decision: %s\tValue: %f %%" name (value * 100.0)
    
    0  // return an integer exit code