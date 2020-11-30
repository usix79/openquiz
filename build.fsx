#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System

open Fake.Core
open Fake.DotNet
open Fake.IO

Target.initEnvironment ()

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let ptestsPath = Path.getFullName "./src/PerfTests"
let clientDeployPath = Path.combine clientPath "deploy"
let clientPublicPath = Path.combine clientPath "public"
let deployDir = Path.getFullName "./deploy"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    Command.RawCommand (npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnetWithArgs (args:string list) cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd (System.String.Join (" ", args))
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let dotnet = dotnetWithArgs []

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore

Target.create "Clean" (fun _ ->
    [deployDir; clientDeployPath]
    |> Shell.cleanDirs
)

Target.create "InstallClient" (fun _ -> npm "install" ".")

Target.create "Build" (fun _ ->
    dotnet "build" serverPath

    Shell.regexReplaceInFileWithEncoding
        "let app = \".+\""
       ("let app = \"" + release.NugetVersion + "\"")
        Text.Encoding.UTF8
        (Path.combine clientPath "Version.fs")
    npm "run build" "."
)

Target.create "Run" (fun _ ->
    let server = async { dotnet "watch run" serverPath }
    let client = async { npm "run start" "." }
    let browser = async {
        do! Async.Sleep 5000
        openBrowser "http://localhost:8080"
    }

    let vsCodeSession = Environment.hasEnvironVar "vsCodeSession"
    let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"

    let tasks =
        [ if not safeClientOnly then yield server
          yield client
          if not vsCodeSession then yield browser ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

Target.create "PTests" (fun p ->
    dotnet "build" ptestsPath
    dotnetWithArgs p.Context.Arguments "run -c Release" ptestsPath
)

Target.create "Docker" (fun _ ->
    let publicDir = Path.combine deployDir "public"
    let publishArgs = sprintf "publish -c Release -o \"%s\"" deployDir
    dotnet publishArgs serverPath

    Shell.copyDir publicDir clientPublicPath FileFilter.allFiles
    Shell.copyDir publicDir clientDeployPath FileFilter.allFiles

    let dockerUser = "usix"
    let dockerImageName = "openquiz"
    let tag = sprintf "%s/%s" dockerUser dockerImageName

    let args = sprintf "build -t %s ." tag
    runTool "docker" args __SOURCE_DIRECTORY__
)

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Docker"

"Clean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Build"