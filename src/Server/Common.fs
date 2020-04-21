module Common

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Threading.Tasks
open System.Net.Http

open Microsoft.FSharp.Reflection
open Microsoft.AspNetCore.Http
open Fable.Remoting.Server
open Newtonsoft.Json.Linq
open Serilog

open Shared

let NYI = Error "Not Yet Implemented"

let executedResponse f req =
    {Status = Executed; Value = f req.Token req.Arg; ST = DateTime.UtcNow}

let generateRandomToken () =
    let randomNumber =  Array.zeroCreate 32
    use rng = RandomNumberGenerator.Create()
    rng.GetBytes(randomNumber)
    System.Convert.ToBase64String(randomNumber)
        .Replace("/", "_")
        .Replace("+", "-")

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
        | Send of 'msg
        | Heartbeat

    type SseService<'msg> () =

        let heartbeatTxt = "event: heartbeat\ndata: .\n\n"

        let msgToText msg = sprintf "event: message\ndata: %s\n\n" (DynamicRecord.serialize msg)

        let writeMessage (resp:HttpResponse) (msg : string) =
            async {
                do! resp.WriteAsync msg |> Async.AwaitTask
                do! resp.Body.FlushAsync() |> Async.AwaitTask
            }

        let agent = MailboxProcessor<AgentCommand<'msg>>.Start(fun inbox ->

            let rec loop (subs : Map<string, Subscription<'msg>>) =
                async {
                    try
                        let! cmd = inbox.Receive()
                        match cmd with
                        | Subscribe sub -> return! loop (subs.Add (sub.SubscriptionId, sub))
                        | Unsubcribe subId -> return! loop (subs.Remove subId)
                        | Heartbeat ->
                            let! _ = subs |> Map.toSeq |> Seq.map (fun (_,sub) -> writeMessage sub.Response heartbeatTxt)
                                        |> Async.Parallel

                            ()
                        | Send msg ->
                            let! _ =
                                subs |> Map.toSeq
                                |> Seq.filter (fun (_, sub) -> sub.Filter msg)
                                |> Seq.map (fun (_,sub) -> writeMessage sub.Response (msgToText msg))
                                |> Async.Parallel
                            ()
                    with
                    | ex -> Log.Error ("{@Pp} {@Exn}", "SSE", ex)

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

        member x.Send msg =
            agent.Post (Send msg)

        member x.WriteMessage (resp : HttpResponse) (msg:'msg) =
            writeMessage resp (msgToText msg) |> Async.StartAsTask

        member x.WriteHeartbeat (resp : HttpResponse)  =
            writeMessage resp heartbeatTxt |> Async.StartAsTask