namespace Flips.Solver.CBC

type Setting =
    private
    | MaxDuration of int64
    | WriteLPFile of string
    | WriteMPSFile of string


type set =

    static member inline maxDuration (value: int64) =
        Setting.MaxDuration value

    static member inline WriteLPFile (value: string) =
        Setting.WriteLPFile value

    static member inline WriteMPSFile (value: string) =
        Setting.WriteMPSFile value


type Settings (settings: Setting list) =
    
    let mutable maxDuration = 10_000L
    let mutable writeLPFile = None
    let mutable writeMPSFile = None

    let assign (setting: Setting) =
        match setting with
        | MaxDuration d -> maxDuration <- d
        | WriteLPFile f -> writeLPFile <- Some f
        | WriteMPSFile f -> writeMPSFile <- Some f

    do List.iter assign settings

    override this.ToString() =
        [
            sprintf "MaxDuration : %i" maxDuration
            sprintf "WriteLPFile : %A" writeLPFile
            sprintf "WriteMPSFile : %A" writeMPSFile
        ] |> string
