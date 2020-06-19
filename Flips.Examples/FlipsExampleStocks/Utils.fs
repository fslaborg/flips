namespace FlipsExampleStocks

module FlipsExample =

    open FSharp.Data
    open System
    open MathNet.Numerics
    open MathNet.Numerics.Statistics 

    type YahooStocks = CsvProvider<"2020-01-01,1.11,1.22,1.33,1.44,1.55,5666777888", Schema = " Date (date), Open (float), High(float), Low(float), Close(float), Adj Close (float), Volume(int64)">

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

  
       
        




