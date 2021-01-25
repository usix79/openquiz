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
open Ionic.Zip
open Amazon.SimpleSystemsManagement

Target.initEnvironment ()

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let ptestsPath = Path.getFullName "./src/PerfTests"
let clientDeployPath = Path.combine clientPath "deploy"
let clientPublicPath = Path.combine clientPath "public"
let deployDir = Path.getFullName "./deploy"
let bundleDir = Path.getFullName "./bundle"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    RawCommand (npmPath, arguments)
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
    RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let getOrCreateRandomParam' type' count paramName =
    use client = new AmazonSimpleSystemsManagementClient()

    try
        let paramResp = client.GetParameterAsync(Model.GetParameterRequest(Name = paramName)).Result
        paramResp.Parameter.Value
    with
    | :? AggregateException as ex ->
        match ex.InnerException with
        | :? Model.ParameterNotFoundException ->
            let rand = Random(DateTime.Now.Millisecond)
            let newGlobalId =
                Seq.init count (fun _ -> rand.Next(10).ToString())
                |> String.concat ""
            client.PutParameterAsync(Model.PutParameterRequest(Name = paramName, Type = type', Value = newGlobalId)).Wait()
            newGlobalId
        | _ -> raise ex

let getOrCreateRandomParam = getOrCreateRandomParam' ParameterType.String 16

Target.create "Clean" (fun _ ->
    [deployDir; clientDeployPath; bundleDir]
    |> Shell.cleanDirs)

Target.create "InstallClient" (fun _ -> npm "install" ".")

Target.create "Build" (fun _ ->
    dotnet "build" serverPath
    dotnet "fable --run webpack --mode production" clientPath)

Target.create "Run" (fun _ ->
    let server = async { dotnet "watch run --environment Development" serverPath }
    let client = async { dotnet "fable watch --run webpack-dev-server" clientPath }

    let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"

    [ if not safeClientOnly then yield server
      yield client
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore)

Target.create "PTests" (fun p ->
    dotnet "build" ptestsPath
    dotnetWithArgs p.Context.Arguments "run -c Release" ptestsPath)

let zipDir sourceDir (destFile:string) =
    use zip = new ZipFile()
    zip.AddDirectory sourceDir |> ignore
    zip.Save destFile

Target.create "Bundle" (fun _ ->
    let serverBundleDir = Path.combine bundleDir "server"
    let clientBundleDir = Path.combine bundleDir "client"
    let publishArgs = sprintf "publish -c Release -o \"%s\"" serverBundleDir
    dotnet publishArgs serverPath

    Shell.copyDir (Path.combine serverBundleDir ".ebextensions") "./aws/.ebextensions" FileFilter.allFiles
    zipDir serverBundleDir (Path.combine bundleDir "openquiz-api.zip")

    Shell.copyDir clientBundleDir clientPublicPath FileFilter.allFiles
    Shell.copyDir clientBundleDir clientDeployPath FileFilter.allFiles
    // zipDir clientBundleDir (Path.combine bundleDir "openquiz-static.zip")
    )

Target.create "DevEnv" (fun _ ->
    getOrCreateRandomParam' ParameterType.SecureString 24 "/OpenQuiz/Development/GwtSecret" |> ignore
    let globalId = getOrCreateRandomParam "/OpenQuiz/GlobalId"
    printfn "OpenQuiz GlobalId: %s" globalId
    let args = (sprintf " -c globalId=%s OpenQuiz-Development" globalId)
    runTool "npx" ("cdk synth" + args) __SOURCE_DIRECTORY__
    runTool "npx" ("cdk deploy" + args) __SOURCE_DIRECTORY__
)

Target.create "Deploy" (fun _ ->
    getOrCreateRandomParam' ParameterType.SecureString 24 "/OpenQuiz/Production/GwtSecret" |> ignore
    let globalId = getOrCreateRandomParam "/OpenQuiz/GlobalId"
    printfn "OpenQuiz GlobalId: %s" globalId
    let args = (sprintf " -c globalId=%s OpenQuiz-Production" globalId)
    runTool "npx" ("cdk synth" + args) __SOURCE_DIRECTORY__
    runTool "npx" ("cdk deploy" + args) __SOURCE_DIRECTORY__
)

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Bundle"
    ==> "DevEnv"

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Bundle"
    ==> "Deploy"

"Clean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Build"