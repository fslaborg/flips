open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open System

let rootDir = __SOURCE_DIRECTORY__ </> ".."

// File system information
let solutionFile = "Flips.sln"

// Github repo
let repo = "https://github.com/fslaborg/flips"

// Read additional information from the release notes document
let release = ReleaseNotes.load (rootDir @@ "RELEASE_NOTES.md")

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName: string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _ -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

let srcGlob = rootDir @@ "Flips/*.??proj"
let fsSrcGlob = rootDir @@ "Flips/*.fs"
let fsTestGlob = rootDir @@ "Flips.Tests/*.fs"
let fsExpGlob = rootDir @@ "Flips.Examples/*.fs"
let bin = rootDir @@ "bin"
let docs = rootDir @@ "docs"
let libGlob = rootDir @@ "Flips/*.fsproj"
let examplesDir = rootDir @@ "Flips.Examples"

let fsSrcAndTest =
    !!fsSrcGlob ++ fsTestGlob ++ fsExpGlob
    -- (rootDir @@ "Flips/obj/**")
    -- (rootDir @@ "Flips.Tests/obj/**")
    -- (rootDir @@ "Flips.Examples/obj/**")

let configuration () =
    FakeVar.getOrDefault "configuration" "Release"

let getEnvFromAllOrNone (s: string) =
    let envOpt (envVar: string) =
        if String.isNullOrEmpty envVar then None else Some(envVar)

    let procVar = Environment.GetEnvironmentVariable(s) |> envOpt

    let userVar =
        Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt

    let machVar =
        Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine)
        |> envOpt

    match procVar, userVar, machVar with
    | Some(v), _, _
    | _, Some(v), _
    | _, _, Some(v) -> Some(v)
    | _ -> None

let initTargets () =
    // Set default
    FakeVar.set "configuration" "Release"

    // --------------------------------------------------------------------------------------
    // Set configuration mode based on target

    Target.create "ConfigDebug" <| fun _ -> FakeVar.set "configuration" "Debug"

    Target.create "ConfigRelease" <| fun _ -> FakeVar.set "configuration" "Release"

    // --------------------------------------------------------------------------------------
    // Clean tasks

    Target.create "Clean"
    <| fun _ ->
        let clean () =
            !!(rootDir @@ "tests/**/bin")
            ++ (rootDir @@ "tests/**/obj")
            ++ (rootDir @@ "src/**/bin")
            ++ (rootDir @@ "src/**/obj")
            |> Seq.toList
            |> List.append [ bin ]
            |> Shell.cleanDirs

        TaskRunner.runWithRetries clean 10

    Target.create "CleanDocs"
    <| fun _ ->
        let clean () =
            !!(docs @@ "RELEASE_NOTES.md") |> List.ofSeq |> List.iter Shell.rm

        TaskRunner.runWithRetries clean 10

    Target.create "CopyDocFiles"
    <| fun _ ->
        [ docs @@ "RELEASE_NOTES.md", rootDir @@ "RELEASE_NOTES.md" ]
        |> List.iter (fun (target, source) -> Shell.copyFile target source)

    Target.create "PrepDocs" ignore

    // --------------------------------------------------------------------------------------
    // Restore tasks

    let restoreSolution () =
        solutionFile
        |> DotNet.restore (fun c ->
            { c with
                MSBuildParams =
                    { c.MSBuildParams with
                        DisableInternalBinLog = true } })

    Target.create "Restore" <| fun _ -> TaskRunner.runWithRetries restoreSolution 5

    // --------------------------------------------------------------------------------------
    // Build tasks

    Target.create "Build"
    <| fun _ ->
        let setParams (defaults: DotNet.BuildOptions) =
            { defaults with
                NoRestore = true
                MSBuildParams =
                    { defaults.MSBuildParams with
                        Verbosity = Some(Quiet)
                        Targets = [ "Build" ]
                        DisableInternalBinLog = true
                        Properties =
                            [ "Optimize", "True"
                              "DebugSymbols", "True"
                              "Configuration", configuration ()
                              "Version", release.AssemblyVersion
                              "DependsOnNETStandard", "true" ] } }

        restoreSolution ()

        !!libGlob ++ (examplesDir @@ "Flips.Examples.fsproj")
        |> List.ofSeq
        |> List.iter (DotNet.build setParams)

    // --------------------------------------------------------------------------------------
    // Run the unit tests

    Target.create "RunTests"
    <| fun _ ->
        DotNet.test
            (fun c ->
                { c with
                    MSBuildParams =
                        { c.MSBuildParams with
                            DisableInternalBinLog = true } })
            (rootDir @@ "Flips.Tests/Flips.Tests.fsproj")

    // --------------------------------------------------------------------------------------
    // Build and release NuGet targets

    Target.create "NuGet"
    <| fun _ ->
        Paket.pack (fun p ->
            { p with
                OutputPath = bin
                Version = release.NugetVersion
                ReleaseNotes = Fake.Core.String.toLines release.Notes
                ProjectUrl = repo
                MinimumFromLockFile = true
                IncludeReferencedProjects = true
                ToolType = ToolType.CreateCLIToolReference() })

    Target.create "NuGetPublish"
    <| fun _ ->
        Paket.push (fun p ->
            { p with
                ApiKey =
                    match getEnvFromAllOrNone "NUGET_KEY" with
                    | Some key -> key
                    | None -> failwith "The NuGet API key must be set in a NUGET_KEY environment variable"
                WorkingDir = bin
                ToolType = ToolType.CreateCLIToolReference() })

    // --------------------------------------------------------------------------------------
    // Release Scripts

    Target.create "Publish"
    <| fun p ->
        // Expect version to be released a argument as a sanity check to prevent releases by accident.
        let announcedVersion =
            match p.Context.Arguments |> List.tryHead with
            | None -> failwithf "Please specify a version number, e.g. '... -t Publish 2.5.0'"
            | Some version -> version

        if announcedVersion <> release.NugetVersion then
            failwithf
                "Version '%s' does not match latest version in the RELEASE_NOTES '%s'"
                announcedVersion
                release.NugetVersion

        let currentBranch = Git.Information.getBranchName ""

        if not (Git.Information.isCleanWorkingCopy "") then
            failwith "You must have a clean working copy to release. Please commit or stash your changes."

        if currentBranch <> "main" then
            failwithf "You must be on the 'main' branch to release. Current branch is '%s'" currentBranch

        let local = Git.Information.getCurrentSHA1 ""
        let remote = Git.Branches.getSHA1 "" ("origin/" + currentBranch)

        if Git.Information.isAheadOf "" local remote then
            failwithf
                "Your local branch must not be ahead of the remote branch '%s'. Please push your changes before releasing."
                currentBranch

        let tagName = sprintf "v%s" release.NugetVersion
        Git.Branches.tag "" tagName

    // --------------------------------------------------------------------------------------
    // Run all targets by default. Invoke 'build -t <Target>' to override

    Target.create "All" ignore
    Target.create "Dev" ignore
    Target.create "Release" ignore

    "Clean" ==> "Restore" ==> "Build" |> ignore

    "Build" ==> "RunTests" |> ignore

    "All" <== [ "RunTests" ]

    "CleanDocs" ==> "CopyDocFiles" ==> "PrepDocs" |> ignore

    "All" ==> "NuGet" ?=> "NuGetPublish" |> ignore

    "All" ==> "PrepDocs" |> ignore

    "ConfigDebug" ?=> "Clean" |> ignore
    "ConfigRelease" ?=> "Clean" |> ignore

    "Dev" <== [ "All"; "ConfigDebug"; "PrepDocs" ]

    "Release" <== [ "All"; "NuGet"; "ConfigRelease" ]

    "Release" ==> "Publish" |> ignore

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefaultWithArguments "Dev"

    0 // return an integer exit code
