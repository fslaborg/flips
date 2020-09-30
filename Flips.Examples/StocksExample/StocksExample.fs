module Flips.Examples.StocksExample

open System
open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Statistics 
open Flips
open Flips.Types

type YahooStocks = CsvProvider<"2020-01-01,1.11,1.22,1.33,1.44,1.55,5666777888", Schema = " Date (date), Open (float), High(float), Low(float), Close(float), Adj Close (float), Volume(int64)">

let solve () =

    // Raw data preparation (downloaded directly from yahoo using CSV data provider)
    let getStockReturns (tickers : string list) startDate endDate = 
        
        let period1 = DateTimeOffset(startDate).ToUnixTimeSeconds().ToString();
        let period2 = DateTimeOffset(endDate).ToUnixTimeSeconds().ToString();

        let getUriString ticker = String.Format("https://query1.finance.yahoo.com/v7/finance/download/{0}?period1={1}&period2={2}&interval=1mo&events=history", ticker, period1, period2)
            
        let stockReturns = 
            tickers
            |> Seq.map(fun ticker -> 
                let uriString = getUriString ticker 
                let prices = YahooStocks.Load(uriString).Cache()
                let returns = 
                    prices.Rows 
                    |> Seq.sortBy(fun r -> r.Date) 
                    |> Seq.map(fun r -> r.``Adj Close``) 
                    |> Seq.pairwise 
                    |> Seq.map (fun (x1, x2) -> (x2 - x1) / x1)
                ticker, returns, Statistics.Mean(returns), returns |> Seq.min )
        
        stockReturns

    let getStockData  (tickers : string list) startDate endDate  = 
        let stockReturns = getStockReturns tickers startDate endDate
        stockReturns |> Seq.map (fun (ticker, returns, mean, min) -> ticker, mean, min)


    // Flips job here...
    
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
    let results = Solver.solve settings model

    // Printing results:
    printfn "-- Result --"
    match results with
    | Optimal solution ->
        printfn "Objective Value - Risk: %f %%" (solution.ObjectiveResult * 100.0)
    
        for (decision, value) in solution.DecisionResults |> Map.toSeq |> Seq.filter(fun (key, value) -> value > 0.0) do
            let (DecisionName name) = decision.Name
            printfn "Decision: %s\tValue: %f %%" name (value * 100.0)
    | errorCase -> 
        printfn "Unable to solve. Error: %A" errorCase
    