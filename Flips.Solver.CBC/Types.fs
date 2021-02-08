namespace Flips.Solver.CBC

type SolverError =
  | Infeasible of string
  | Unbounded of string
  | Unknown of string

type SolverType =
    | CBC
    | GLOP

type Setting =
    private
    | MaxDuration of int64
    | WriteLPFile of string
    | WriteMPSFile of string
    | EnableOuput of bool
    | SolverType of SolverType


type set =

    static member maxDuration (value: int64) =
        Setting.MaxDuration value

    static member WriteLPFile (value: string) =
        Setting.WriteLPFile value

    static member WriteMPSFile (value: string) =
        Setting.WriteMPSFile value

    static member EnableOutput (value: bool) =
        Setting.EnableOuput value

    static member SolverType (value: SolverType) =
        Setting.SolverType value

type Settings (settings: Setting list) =
    
    let mutable maxDuration = 10_000L
    let mutable writeLPFile = None
    let mutable writeMPSFile = None
    let mutable enableOutput = false
    let mutable solverType = CBC

    let assign (setting: Setting) =
        match setting with
        | MaxDuration d -> maxDuration <- d
        | WriteLPFile f -> writeLPFile <- Some f
        | WriteMPSFile f -> writeMPSFile <- Some f
        | EnableOuput b -> enableOutput <- b
        | SolverType s -> solverType <- s

    do List.iter assign settings

    override this.ToString() =
        [
            sprintf "SolverType : %A" solverType
            sprintf "MaxDuration : %i" maxDuration
            sprintf "WriteLPFile : %A" writeLPFile
            sprintf "WriteMPSFile : %A" writeMPSFile
            sprintf "EnableOutput : %A" enableOutput
        ] |> string

    member _.MaxDuration = maxDuration
    member _.WriteLPFile = writeLPFile
    member _.WriteMPSFile = writeMPSFile
    member _.EnableOutput = enableOutput
    member _.SolverType = solverType
