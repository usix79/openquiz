module Common

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Net.Http

open Microsoft.AspNetCore.Http
open Fable.Remoting.Server
open Newtonsoft.Json.Linq

open Shared

let NYI = Error "Not Yet Implemented"

type ARES<'Value> = Async<Result<'Value, string>>

let executedResponse f req =
    async{
        let! v = f req.Token req.Arg
        return {Status = Executed; Value = v; ST = DateTime.UtcNow}
    }

let generateRandomToken () =
    let randomNumber =  Array.zeroCreate 32
    use rng = RandomNumberGenerator.Create()
    rng.GetBytes(randomNumber)
    System.Convert.ToBase64String(randomNumber)
        .Replace("/", "_")
        .Replace("+", "-")

let tryParseInt32 (str:string) =
    match System.Int32.TryParse(str) with
    | true, n -> Some n
    | _ -> None

let toEpoch dt =
    (dt - DateTime.UnixEpoch).TotalSeconds
    |> Convert.ToInt64
    |> Convert.ToDecimal

let fromEpoch (epoch:decimal) =
    epoch
    |> Convert.ToDouble
    |> DateTime.UnixEpoch.AddSeconds

module Result =
    let toOption = function
    | Ok entity -> Some entity
    | Error _ -> None

    let fromOption error = function
    | Some entity -> Ok entity
    | None -> Error error

module Async =

    let retn x = async { return x }

    let map f m =
        async {
            let! x = m
            return f x
        }

    let bind f m =
        async {
            let! x = m
            return! f x
        }

module AsyncResult =

    let fromResult x = async { return x }

    let retn x = async { return Ok x }

    let map f m =
        async {
            match! m with
            | Ok a -> return Ok(f a)
            | Error e -> return Error e
        }

    let mapError f m =
        async {
            match! m with
            | Ok a -> return Ok a
            | Error e -> return Error (f e)
        }

    let ifError f m =
        async {
            match! m with
            | Ok a -> return Ok a
            | Error e -> return Ok (f e)
        }

    let bind f m =
        async {
            match! m with
            | Ok a -> return! f a
            | Error e -> return Error e
        }

    let side f m =
        async {
            match! m with
            | Ok a ->
                match! f a with
                | Ok _ -> return Ok a
                | Error e -> return Error e
            | Error e -> return Error e
        }

    let sideIf p f m =
        if p then side f m else m

    let sideRes f m =
        async {
            match! m with
            | Ok a ->
                match f a with
                | Ok _ -> return Ok a
                | Error e -> return Error e
            | Error e -> return Error e
        }

    let next mn m =
        bind (fun _ -> mn) m

module Config =
    open Microsoft.Extensions.Configuration

    let getJwtSecret (cfg:IConfiguration) = cfg.["jwtsecret"]
    let getCognitoClientId (cfg:IConfiguration) = cfg.["cognitoClientId"]
    let getCognitoClientName (cfg:IConfiguration) = cfg.["cognitoClientName"]
    let getFilesAccessPoint (cfg:IConfiguration) = cfg.["filesAccessPoint"]
    let getAppsyncEndpoint (cfg:IConfiguration) = cfg.["appsync-endpoint"]
    let getAppsyncApiKey (cfg:IConfiguration) = cfg.["appsync-apikey"]
    let getAppsyncRegion (cfg:IConfiguration) = cfg.["appsync-region"]
    let getRedirectUrl (cfg:IConfiguration) =
#if DEBUG
        cfg.["redirectUrlDebug"]
#else
        cfg.["redirectUrl"]
#endif

    let getAppSyncCfg (cfg:IConfiguration) =
        {Endpoint = getAppsyncEndpoint cfg; Region = getAppsyncRegion cfg; ApiKey = getAppsyncApiKey cfg}

module Aws =
    let private httpClient = new HttpClient()

    let getCognitoUri clientName =
        sprintf "https://%s.auth.eu-central-1.amazoncognito.com" clientName

    let getUserToken clientName clientId redirectUrl code =

        async {
            let uri = sprintf "%s/oauth2/token" (getCognitoUri clientName)

            let values = new Dictionary<string, string>()
            values.Add("grant_type", "authorization_code")
            values.Add("client_id", clientId)
            values.Add("code", code)
            values.Add("redirect_uri", redirectUrl)
            let content = new FormUrlEncodedContent(values);

            let! resp = httpClient.PostAsync(uri, content) |> Async.AwaitTask
            let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

            match resp.StatusCode with
            | Net.HttpStatusCode.OK ->
                let json = JObject.Parse respStr

                let res = {|
                    IdToken = json.GetValue("id_token").ToString()
                    AccessToken = json.GetValue("access_token").ToString()
                    RefreshToken = json.GetValue("refresh_token").ToString()
                |}

                return Ok res
            | _ ->
                Serilog.Log.Error ("{@Op} {@Status} {@Resp}", "Aws.getUserToken", resp.StatusCode,  respStr)
                return Error (resp.StatusCode.ToString())
        }

    let getUserInfo clientName accessToken =

        async {
            let uri = sprintf "%s/oauth2/userInfo" (getCognitoUri clientName)
            let httpReq = new HttpRequestMessage(HttpMethod.Get, uri)
            httpReq.Headers.Add ("Authorization", sprintf "Bearer %s" accessToken)

            let! resp = httpClient.SendAsync(httpReq) |> Async.AwaitTask
            let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

            match resp.StatusCode with
            | Net.HttpStatusCode.OK ->
                let json = JObject.Parse respStr

                let username = json.GetValue("username").ToString()

                let picture = json.GetValue("picture").ToString();

                let picture =
                    match username with
                    | _ when not (String.IsNullOrEmpty picture) && username.Contains ("facebook") ->
                        let picJson = JObject.Parse picture
                        picJson.["data"].["url"].ToString()
                    | _ -> picture

                let res = {|
                    Sub = json.GetValue("sub").ToString()
                    Username = username
                    Name = json.GetValue("name").ToString()
                    Picture = picture
                |}

                return Ok res
            | _ ->
                Serilog.Log.Error ("{@Op} {@Status} {@Resp}", "Aws.getUserToken", resp.StatusCode,  respStr)
                return Error (resp.StatusCode.ToString())

        }

    let private publishQuery = sprintf """{ "query": "mutation quizMessage {quizMessage(quizId: %i, token: \"%s\", body: \"%s\", version: %i){quizId, token, body, version} }"}"""

    let publishQuizMessage (endpoint:string) region quizId token version evt =
        async {
            try
                let! creds = Amazon.Runtime.FallbackCredentialsFactory.GetCredentials().GetCredentialsAsync() |> Async.AwaitTask

                let signer = new Aws4RequestSigner.AWS4RequestSigner(creds.AccessKey, creds.SecretKey)

                let body =
                    DynamicRecord.serialize evt
                    |> Text.UTF8Encoding.UTF8.GetBytes
                    |> Convert.ToBase64String

                let data = publishQuery quizId token body version

                let content = new StringContent(data, Text.Encoding.UTF8, "application/graphql")

                let origReq = new HttpRequestMessage(HttpMethod.Post, endpoint, Content = content)

                let! signedReq = signer.Sign(origReq, "appsync", region) |> Async.AwaitTask

                if creds.UseToken then
                    signedReq.Headers.Add("X-Amz-Security-Token", creds.Token)

                let! resp = httpClient.SendAsync(signedReq) |> Async.AwaitTask
                let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
                match resp.StatusCode with
                | Net.HttpStatusCode.OK -> return ()
                | _ -> return Serilog.Log.Error ("{@Op} {@Status} {@Resp}", "Aws.appsync", resp.StatusCode,  respStr)
            with
            | ex -> return Serilog.Log.Error ("{@Op} {@Exception}", "Aws.appsync", ex)
        }

let ofOption error = function Some s -> Ok s | None -> Error error

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()

module Diag =
    let status () =
        printfn "THREADS:%i WI(%i %i) MEMORY:%i Heap:%i GCC(%i %i %i) AllockRage:%i  MI(%i %i %i %i %i) Locks:%i"
            Threading.ThreadPool.ThreadCount
            Threading.ThreadPool.PendingWorkItemCount
            Threading.ThreadPool.CompletedWorkItemCount
            (Environment.WorkingSet / 1000000L)
            (GC.GetTotalMemory(false) / 1000000L)
            (GC.CollectionCount(0))
            (GC.CollectionCount(1))
            (GC.CollectionCount(2))
            (GC.GetTotalAllocatedBytes(false))
            (GC.GetGCMemoryInfo().FragmentedBytes)
            (GC.GetGCMemoryInfo().HeapSizeBytes)
            (GC.GetGCMemoryInfo().HighMemoryLoadThresholdBytes)
            (GC.GetGCMemoryInfo().MemoryLoadBytes)
            (GC.GetGCMemoryInfo().TotalAvailableMemoryBytes)
            (Threading.Monitor.LockContentionCount)

    let ulimit () =
        let command = "ulimit -a";

        use proc = new System.Diagnostics.Process()
        proc.StartInfo.FileName <- "/bin/sh"
        proc.StartInfo.Arguments <- "-c \" " + command + " \""
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.RedirectStandardError <- true
        proc.Start() |> ignore

        printfn "ULIMITS: %s" (proc.StandardOutput.ReadToEnd())
        printfn "ULIMITS ERROR: %s" (proc.StandardError.ReadToEnd())

        proc.WaitForExit();