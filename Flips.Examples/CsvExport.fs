module Flips.Examples.CsvExport
open System.IO
open System.Globalization

open CsvHelper
open CsvHelper.Configuration
open Flips.Types

let csvConfig = CsvConfiguration(CultureInfo.InvariantCulture, Delimiter = "\t")

let exportVariablesToFile problemName (results: Map<Decision, float>) (csvConfig: CsvConfiguration) =

    let outputFolder = Path.Combine(__SOURCE_DIRECTORY__, "output") |> DirectoryInfo
    outputFolder.Create()
    let outputFile = Path.Combine(outputFolder.FullName, sprintf "%s.tsv" problemName)

    // begin csv writing
    use csvWriter = new StreamWriter(outputFile)
    use csvWriter =
        
        let headers = ["Decision Name";"Result"]
        let csvWriter = new CsvWriter(csvWriter, csvConfig)
        headers |> List.iter csvWriter.WriteField
        csvWriter.NextRecord()
        csvWriter
    
    for (decision, value) in results |> Map.toSeq do
        let (DecisionName name) = decision.Name
        
        csvWriter.WriteField(name)
        csvWriter.WriteField(string value)
        csvWriter.NextRecord()
