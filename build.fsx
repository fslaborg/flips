// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#nowarn "0213"
#r "paket:
source https://api.nuget.org/v3/index.json
storage: none
nuget Fake.Core.Target ~> 5
nuget Fake.Core.ReleaseNotes ~> 5
nuget Fake.DotNet.AssemblyInfoFile ~> 5
nuget Fake.DotNet.Cli ~> 5
nuget Fake.DotNet.MSBuild ~> 5
nuget Fake.DotNet.Paket ~> 5
nuget Fake.IO.FileSystem ~> 5
nuget Fake.JavaScript.Yarn ~> 5
nuget Fake.JavaScript.Npm ~> 5
nuget Fake.Tools.Git ~> 5
nuget FSharp.Json ~> 0
nuget FSharpLint.Core ~> 0
nuget NuGet.Packaging 5.6
"
#load "./tools/FSharpLint.fs"
#load "./.fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open Tools.Linting
open System
open System.IO

// File system information
let solutionFile = "Flips.sln"

// Github repo
let repo = "https://github.com/fslaborg/flips"

// Read additional information from the release notes document
let release = ReleaseNotes.load (__SOURCE_DIRECTORY__ @@ "RELEASE_NOTES.md")

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)
    
let srcGlob     = __SOURCE_DIRECTORY__ @@ "Flips/*.??proj"
let fsSrcGlob   = __SOURCE_DIRECTORY__ @@ "Flips/*.fs"
let fsTestGlob  = __SOURCE_DIRECTORY__ @@ "Flips.Tests/*.fs"
let fsExpGlob   = __SOURCE_DIRECTORY__ @@ "Flips.Examples/*.fs"
let bin         = __SOURCE_DIRECTORY__ @@ "bin"
let docs        = __SOURCE_DIRECTORY__ @@ "docs"
let libGlob     = __SOURCE_DIRECTORY__ @@ "Flips/*.fsproj"
let examplesDir = __SOURCE_DIRECTORY__ @@ "Flips.Examples"

let fsSrcAndTest =
    !! fsSrcGlob
    ++ fsTestGlob
    ++ fsExpGlob
    -- (__SOURCE_DIRECTORY__  @@ "Flips/obj/**")
    -- (__SOURCE_DIRECTORY__  @@ "Flips.Tests/obj/**")
    -- (__SOURCE_DIRECTORY__  @@ "Flips.Examples/obj/**")
    
let configuration() =
    FakeVar.getOrDefault "configuration" "Release"

let getEnvFromAllOrNone (s: string) =
    let envOpt (envVar: string) =
        if String.isNullOrEmpty envVar then None
        else Some(envVar)

    let procVar = Environment.GetEnvironmentVariable(s) |> envOpt
    let userVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt
    let machVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine) |> envOpt

    match procVar,userVar,machVar with
    | Some(v), _, _
    | _, Some(v), _
    | _, _, Some(v)
        -> Some(v)
    | _ -> None

// Set default
FakeVar.set "configuration" "Release"

// --------------------------------------------------------------------------------------
// Set configuration mode based on target

Target.create "ConfigDebug" <| fun _ ->
    FakeVar.set "configuration" "Debug"

Target.create "ConfigRelease" <| fun _ ->
    FakeVar.set "configuration" "Release"

// --------------------------------------------------------------------------------------
// Clean tasks

Target.create "Clean" <| fun _ ->
    let clean() =
        !! (__SOURCE_DIRECTORY__  @@ "tests/**/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "tests/**/obj")
        ++ (__SOURCE_DIRECTORY__  @@ "tools/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "tools/obj")
        ++ (__SOURCE_DIRECTORY__  @@ "src/**/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "src/**/obj")
        |> Seq.toList
        |> List.append [ bin ]
        |> Shell.cleanDirs
    TaskRunner.runWithRetries clean 10

Target.create "CleanDocs" <| fun _ ->
    let clean() =
        !! (docs @@ "RELEASE_NOTES.md")
        |> List.ofSeq
        |> List.iter Shell.rm

    TaskRunner.runWithRetries clean 10

Target.create "CopyDocFiles" <| fun _ ->
    [ docs @@ "RELEASE_NOTES.md", __SOURCE_DIRECTORY__ @@ "RELEASE_NOTES.md" ]
    |> List.iter (fun (target, source) -> Shell.copyFile target source)

Target.create "PrepDocs" ignore

// --------------------------------------------------------------------------------------
// Restore tasks

let restoreSolution () =
    solutionFile
    |> DotNet.restore id

Target.create "Restore" <| fun _ ->
    TaskRunner.runWithRetries restoreSolution 5

// --------------------------------------------------------------------------------------
// Build tasks

Target.create "Build" <| fun _ ->
    let setParams (defaults: DotNet.BuildOptions) =
        { defaults with
            NoRestore = true
            MSBuildParams = 
            { defaults.MSBuildParams with
                Verbosity = Some(Quiet)
                Targets = ["Build"]
                Properties =
                    [
                        "Optimize", "True"
                        "DebugSymbols", "True"
                        "Configuration", configuration()
                        "Version", release.AssemblyVersion
                        "DependsOnNETStandard", "true"
                    ]
            }
         }
    restoreSolution()

    !! libGlob
    ++ (examplesDir @@ "Flips.Examples.fsproj")
    |> List.ofSeq
    |> List.iter (DotNet.build setParams)

// --------------------------------------------------------------------------------------
// Lint source code

Target.create "Lint" <| fun _ ->
    fsSrcAndTest
    -- (__SOURCE_DIRECTORY__  @@ "src/**/AssemblyInfo.*")
    |> (fun fGlob -> [(false, fGlob)])
    |> Seq.map (fun (b,glob) -> (b,glob |> List.ofSeq))
    |> List.ofSeq
    |> FSharpLinter.lintFiles

// --------------------------------------------------------------------------------------
// Run the unit tests

Target.create "RunTests" <| fun _ ->
    DotNet.test id (__SOURCE_DIRECTORY__ @@ "Flips.Tests/Flips.Tests.fsproj")

// --------------------------------------------------------------------------------------
// Build and release NuGet targets

Target.create "NuGet" <| fun _ ->
    Paket.pack(fun p ->
        { p with
            OutputPath = bin
            Version = release.NugetVersion
            ReleaseNotes = Fake.Core.String.toLines release.Notes
            ProjectUrl = repo
            MinimumFromLockFile = true
            IncludeReferencedProjects = true })

Target.create "NuGetPublish" <| fun _ ->
    Paket.push(fun p ->
        { p with
            ApiKey = 
                match getEnvFromAllOrNone "NUGET_KEY" with
                | Some key -> key
                | None -> failwith "The NuGet API key must be set in a NUGET_KEY environment variable"
            WorkingDir = bin })

// --------------------------------------------------------------------------------------
// Release Scripts

let gitPush msg =
    Git.Staging.stageAll ""
    Git.Commit.exec "" msg
    Git.Branches.push ""

Target.create "GitPush" <| fun p ->
    p.Context.Arguments
    |> List.choose (fun s ->
        match s.StartsWith("--Msg=") with
        | true -> Some(s.Substring 6)
        | false -> None)
    |> List.tryHead
    |> function
    | Some(s) -> s
    | None -> (sprintf "Bump version to %s" release.NugetVersion)
    |> gitPush

Target.create "GitTag" <| fun _ ->
    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" "origin" release.NugetVersion

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build -t <Target>' to override

Target.create "All" ignore
Target.create "Dev" ignore
Target.create "Release" ignore
Target.create "Publish" ignore

"Clean"
    ==> "Restore"
    ==> "Build"

"Restore" ==> "Lint"

"Lint" 
    ?=> "Build"
    ?=> "RunTests"
    ?=> "CleanDocs"

"Build" ==> "RunTests"

"All"
    ==> "GitPush"
    ?=> "GitTag"

"All" <== ["Lint"; "RunTests" ]

"CleanDocs"
    ==> "CopyDocFiles"
    ==> "PrepDocs"

"All"
    ==> "NuGet"
    ?=> "NuGetPublish"

"All" ==> "PrepDocs"

"ConfigDebug" ?=> "Clean"
"ConfigRelease" ?=> "Clean"

"Dev" <== ["All"; "ConfigDebug"; "PrepDocs"]

"Release" <== ["All"; "NuGet"; "ConfigRelease"]

"Publish" <== ["Release"; "ConfigRelease"; "NuGetPublish"; "Build"; "GitTag"; "GitPush" ]

Target.runOrDefaultWithArguments "Dev"
