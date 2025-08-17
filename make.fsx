#r "nuget: FsToolkit.ErrorHandling"
#r "nuget: AWSSDK.SimpleSystemsManagement"



open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Threading
open FsToolkit.ErrorHandling
open Amazon.SimpleSystemsManagement
open System.Net


let serverPath = Path.Combine(__SOURCE_DIRECTORY__, "src/server")
let clientPath = Path.Combine(__SOURCE_DIRECTORY__, "src/client")
let ptestsPath = Path.Combine(__SOURCE_DIRECTORY__, "src/perf")
let clientDeployPath = Path.Combine(clientPath, "deploy")
let clientPublicPath = Path.Combine(clientPath, "public")
let webpackOutputPath = Path.Combine(clientPath, "deploy")
let deployDir = Path.Combine(__SOURCE_DIRECTORY__, "deploy")
let bundleDir = Path.Combine(__SOURCE_DIRECTORY__, "bundle")
let bundleOutputDir = Path.Combine(bundleDir, "output")


let inline (^) f x = f x

let exec (cmd: string) (args: string) (workDir: string) =

    let cmdTxt = $"EXEC {cmd} {args} from {workDir}"
    Console.WriteLine cmdTxt

    // Explicit string args resolve ProcessStartInfo(string,string) overload
    let startInfo = ProcessStartInfo(cmd, args)
    startInfo.WorkingDirectory <- workDir
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true

    use proc = new Process()
    proc.StartInfo <- startInfo

    proc.Start() |> ignore
    proc.WaitForExit()

    match proc.ExitCode with
    | 0 -> Ok()
    | x -> Error ^ $"{cmdTxt} exit with code {x}"

let dotnet = exec "dotnet"

let npm = exec "npm"

let npx = exec "npx"

let getArgs () =
    if fsi.CommandLineArgs.Length > 1 then
        fsi.CommandLineArgs.[1..]
        |> List.ofArray
        |> List.map ^ fun x -> x.ToUpperInvariant()
    else
        []


let rec rmDir dir =
    if Directory.Exists(dir) then
        printfn $"Remove {dir}"
        Directory.Delete(dir, true) |> ignore

let rec copyDir sourceDir targetDir =
    printfn $"Copy {sourceDir} to {targetDir}"
    Directory.CreateDirectory(targetDir) |> ignore

    for file in Directory.GetFiles(sourceDir) do
        let fileName = Path.GetFileName(file)
        let destFile = Path.Combine(targetDir, fileName)
        File.Copy(file, destFile, true)

    for subdir in Directory.GetDirectories(sourceDir) do
        let dirName = Path.GetFileName(subdir)
        let destSubDir = Path.Combine(targetDir, dirName)
        copyDir subdir destSubDir

let make (targets: List<string * (string list -> Result<unit, string>)>) =

    match getArgs () with
    | [] -> printfn $"Usage: make <target> [args]"
    | target :: args ->
        printfn $"OpenQuiz Make {target}"

        let targets =
            targets
            |> List.map (fun (name, action) -> name.ToUpperInvariant(), action)
            |> Map.ofList

        match targets.TryFind target with
        | Some action ->
            match action args with
            | Ok _ -> Console.WriteLine $"OK"
            | Error err -> Console.WriteLine $"ERROR {err}"
        | None -> Console.WriteLine $"ERROR Unknown Target"

let restore _ =
    result {
        do! dotnet "tool restore" __SOURCE_DIRECTORY__
        do! dotnet "restore" __SOURCE_DIRECTORY__
        do! npm "install" __SOURCE_DIRECTORY__
    }

let clean _ =
    rmDir webpackOutputPath

    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__
        do! dotnet "fable clean --yes" __SOURCE_DIRECTORY__
    }

let build _ =
    result {
        do! dotnet "build" __SOURCE_DIRECTORY__
        do! dotnet "fable" clientPath
        do! npx "webpack --mode production" __SOURCE_DIRECTORY__
    }

let rebuild args =
    result {
        do! clean args
        do! build args
    }

let run _ =

    let clientTask () =
        dotnet $"fable watch {clientPath} --run webpack-dev-server" __SOURCE_DIRECTORY__
        |> ignore

    let serverTask () =
        dotnet "watch run --environment Development" serverPath |> ignore

    let clientThread = new Thread(ThreadStart clientTask)
    clientThread.Start()

    let serverThread = new Thread(ThreadStart serverTask)
    serverThread.Start()

    clientThread.Join()
    serverThread.Join()

    Ok()

let bundle _ =
    result {
        let serverBundleDir = Path.Combine(bundleDir, "server")
        let clientBundleDir = Path.Combine(bundleDir, "client")

        rmDir bundleDir

        let publishArgs = $"publish -c Release -o \"{serverBundleDir}\""
        do! dotnet publishArgs serverPath
        copyDir "./aws/.ebextensions" (Path.Combine(serverBundleDir, ".ebextensions"))

        ZipFile.CreateFromDirectory(serverBundleDir, Path.Combine(bundleDir, "openquiz-api.zip"))
        |> ignore

        copyDir clientPublicPath clientBundleDir
        copyDir clientDeployPath clientBundleDir
    }

let getOrCreateRandomParam type' count paramName =
    use client = new AmazonSimpleSystemsManagementClient()

    try
        let paramResp =
            client.GetParameterAsync(Model.GetParameterRequest(Name = paramName)).Result

        paramResp.Parameter.Value
    with :? AggregateException as ex ->
        match ex.InnerException with
        | :? Model.ParameterNotFoundException ->
            let rand = Random(DateTime.Now.Millisecond)

            let newGlobalId =
                Seq.init count (fun _ -> rand.Next(10).ToString()) |> String.concat ""

            client
                .PutParameterAsync(Model.PutParameterRequest(Name = paramName, Type = type', Value = newGlobalId))
                .Wait()

            newGlobalId
        | _ -> raise ex

let devEnv _ =
    result {
        getOrCreateRandomParam ParameterType.SecureString 36 "/OpenQuiz/Development/GwtSecret"
        |> ignore

        let globalId = getOrCreateRandomParam ParameterType.String 16 "/OpenQuiz/GlobalId"
        printfn $"OpenQuiz GlobalId: {globalId}"

        let args = $"-c globalId={globalId} OpenQuiz-Development"
        do! npx $"cdk synth {args}" __SOURCE_DIRECTORY__
        do! npx $"cdk deploy {args}" __SOURCE_DIRECTORY__
    }

let deployToEnvironment envName stackName args =
    result {
        do! rebuild args
        do! bundle args

        // After bundle, drop release notes directly into bundle/client
        let releaseNotesSrc = Path.Combine(__SOURCE_DIRECTORY__, "RELEASE_NOTES.md")
        let bundledClientDir = Path.Combine(bundleDir, "client")
        Directory.CreateDirectory(bundledClientDir) |> ignore
        let releaseNotesDst = Path.Combine(bundledClientDir, "RELEASE_NOTES.html")

        if File.Exists(releaseNotesSrc) then
            let md = File.ReadAllText(releaseNotesSrc)
            let encoded = WebUtility.HtmlEncode(md)

            let html =
                $"""<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Release Notes</title></head><body><pre style='white-space:pre-wrap;font-family:monospace;'>%s{encoded}</pre></body></html>"""

            File.WriteAllText(releaseNotesDst, html)
            printfn $"Bundled release notes: {releaseNotesDst}"
        else
            printfn "RELEASE_NOTES.md not found; skipping bundled RELEASE_NOTES.html"

        getOrCreateRandomParam ParameterType.SecureString 36 $"/OpenQuiz/{envName}/GwtSecret"
        |> ignore

        let globalId = getOrCreateRandomParam ParameterType.String 16 "/OpenQuiz/GlobalId"
        printfn $"OpenQuiz GlobalId: {globalId}"

        let args = $"-c globalId={globalId} {stackName}"
        do! npx $"cdk synth {args}" __SOURCE_DIRECTORY__
        do! npx $"cdk deploy {args}" __SOURCE_DIRECTORY__
    }


[ "RESTORE", restore
  "CLEAN", clean
  "BUILD", build
  "REBUILD", rebuild
  "RUN", run
  "BUNDLE", bundle
  "DEVENV", devEnv
  "STAGE", deployToEnvironment "Stage" "OpenQuiz-Stage"
  "DEPLOY", deployToEnvironment "Production" "OpenQuiz-Production" ]
|> make