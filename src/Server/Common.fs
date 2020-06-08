module Common

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Threading.Tasks
open System.Net.Http

open Microsoft.AspNetCore.Http
open Fable.Remoting.Server
open Newtonsoft.Json.Linq
open Serilog

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

    let getJwtSecret (cfg:IConfiguration) =
        cfg.["jwtsecret"]

    let getCognitoClientId (cfg:IConfiguration) =
        cfg.["cognitoClientId"]

    let getCognitoClientName (cfg:IConfiguration) =
        cfg.["cognitoClientName"]

    let getRedirectUrl (cfg:IConfiguration) =
#if DEBUG
        cfg.["redirectUrlDebug"]
#else
        cfg.["redirectUrl"]
#endif

    let getFilesAccessPoint (cfg:IConfiguration) =
        cfg.["filesAccessPoint"]

module Http =
    open Microsoft.AspNetCore.Http
    open Giraffe

    let (|QInt|_|) (ctx: HttpContext) key =
       match ctx.TryGetQueryStringValue key with
       | Some str ->
           match System.Int32.TryParse(str) with
           | (true,int) -> Some(int)
           | _ -> None
       | None -> None

    let (|QStr|_|) (ctx: HttpContext) key =
        ctx.TryGetQueryStringValue key

module Aws =
    open System.Collections.Generic
    open System.Net.Http
    open Newtonsoft.Json.Linq

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
                printfn "RESP: %s" respStr
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

module Sse =

    type private Subscription<'msg> = {
        SubscriptionId : string
        Response : HttpResponse
        Filter : 'msg -> bool
    }

    type private AgentCommand<'msg> =
        | Subscribe of Subscription<'msg>
        | Unsubcribe of string
        | Send of string*'msg
        | Heartbeat

    type SseService<'msg> () =

        let heartbeatTxt = "event: heartbeat\ndata: .\n\n"

        let msgToText msg = sprintf "event: message\ndata: %s\n\n" (DynamicRecord.serialize msg)

        let writeMessage (resp:HttpResponse) (msg : string) =
            async {
                try
                    do! resp.WriteAsync msg |> Async.AwaitTask
                    do! resp.Body.FlushAsync() |> Async.AwaitTask
                with
                | ex -> Log.Error ("{@Proc} {@Step} {@Exn}", "SSE", "writeMessage", ex)
            }

        let agent = MailboxProcessor<AgentCommand<'msg>>.Start(fun inbox ->

            let rec loop (subs : Map<string, Subscription<'msg>>) =
                async {
                    try
                        let! cmd = inbox.Receive()
                        match cmd with
                        | Subscribe sub ->
                            let subs = (subs.Add (sub.SubscriptionId, sub))
                            Log.Information ("{@Op} {@Proc} {@Count}", "Subscribe", "SSE", subs.Count)
                            return! loop subs
                        | Unsubcribe subId ->
                            let subs = (subs.Remove subId)
                            Log.Information ("{@Op} {@Proc} {@Count}", "Unsubscribe", "SSE", subs.Count)
                            return! loop subs
                        | Heartbeat ->
                            let sw = Diagnostics.Stopwatch.StartNew()

                            do!
                            subs |> Map.toSeq |> Seq.map (fun (_,sub) -> writeMessage sub.Response heartbeatTxt)
                            //|> FSharpx.Control.Async.ParallelCatchWithThrottle 2
                            |> Async.Sequential
                            |> Async.map (fun _ ->
                                sw.Stop()
                                Log.Information("{@Op} {@Proc} {@ListenersCount} {@Duration}", "Heartbeat", "SSE", subs.Count, sw.ElapsedMilliseconds)
                            )
                            //|> Async.Start
                        | Send (topic,msg) ->
                            let sw = Diagnostics.Stopwatch.StartNew()
                            do!
                            subs |> Map.toSeq
                            |> Seq.filter (fun (_, sub) -> sub.Filter msg)
                            |> Seq.map (fun (_,sub) -> writeMessage sub.Response (msgToText msg))
                            //|> FSharpx.Control.Async.ParallelCatchWithThrottle 2
                            |> Async.Sequential
                            |> Async.map (fun arr ->
                                sw.Stop()
                                Log.Information("{@Op} {@Proc} {@Topic} {@ListenersCount} {@Duration}", "Message", "SSE", topic, arr.Length, sw.ElapsedMilliseconds)
                            )
                            //|> Async.Start
                    with
                    | ex -> Log.Error ("{@Proc} {@Exn}", "SSE", ex)

                    return! loop subs
                }

            loop Map.empty
        )

        let rec ticker () =
            async{
                do! Task.Delay (1000 * 30) |> Async.AwaitTask
                agent.Post Heartbeat

                return! ticker()
            }

        do ticker() |> Async.Start

        member x.Subscribe subscriptionId (resp : HttpResponse) (filter : 'msg -> bool) =
            agent.Post (Subscribe {SubscriptionId = subscriptionId; Response = resp; Filter = filter})

        member x.Unsubscribe subscriptionId =
            agent.Post (Unsubcribe subscriptionId)

        member x.Send topic msg =
            agent.Post (Send (topic,msg))

        member x.WriteMessage (resp : HttpResponse) (msg:'msg) =
            writeMessage resp (msgToText msg) |> Async.StartAsTask

        member x.WriteHeartbeat (resp : HttpResponse)  =
            writeMessage resp heartbeatTxt |> Async.StartAsTask

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

    // type DiagListener () =
    //     inherit Diagnostics.Tracing.EventListener()




    //     override x.OnEventSourceCreated es =

    //         if es.Name = "Microsoft-Windows-DotNETRuntime" then
    //             let GC_KEYWORD =                 0x0000001L
    //             let TYPE_KEYWORD =               0x0080000L
    //             let GCHEAPANDTYPENAMES_KEYWORD = 0x1000000L
    //             let ThreadingKeyword    = 0x00010000L

    //             let flags =
    //                 (GC_KEYWORD ||| TYPE_KEYWORD ||| GCHEAPANDTYPENAMES_KEYWORD (*||| ThreadingKeyword*))
    //             printfn "AGA Listener flags %A" flags
    //             let kwObj = Enum.ToObject(typeof<Diagnostics.Tracing.EventKeywords>, flags)
    //             let kw = unbox kwObj
    //             printfn "KW %A" kw
    //             x.EnableEvents (es, Diagnostics.Tracing.EventLevel.Informational, kw)

    //     override x.OnEventWritten ed =
    //         printfn "Message: %s" (DateTime.UtcNow.ToString("HH:mm:ss:fff"))
    //         ed.Payload
    //         |> Seq.iteri (fun idx p -> printfn "%s %A" ed.PayloadNames.[idx] p)