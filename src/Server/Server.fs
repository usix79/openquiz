open System
open System.IO
open System.Threading.Tasks

open AWS.Logger
open AWS.Logger.SeriLog
open Giraffe
open Giraffe.Core
open Giraffe.ResponseWriters
open Giraffe.SerilogExtensions
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open FSharp.Control.Tasks.ContextInsensitive
open FSharp.Control.Tasks.V2
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Rewrite
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Serilog

open Shared
open Common

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let isProdMode = Directory.Exists "public"
let publicPath = Directory.GetCurrentDirectory() + "/" + if isProdMode then "public" else "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let errorHandler (ex: Exception) (routeInfo: RouteInfo<HttpContext>) =
    let contextLogger = routeInfo.httpContext.Logger()
    let errorMsgTemplate = "EXCEPTION in {Proc} at {RoutePath}"
    contextLogger.Error(ex, errorMsgTemplate, routeInfo.methodName, routeInfo.path)
    Propagate Errors.SessionIsNotActive

let apiHandler api =
    Remoting.createApi()
    |> Remoting.withRouteBuilder (Infra.routeBuilder "")
    |> Remoting.fromContext api
    |> Remoting.withErrorHandler errorHandler
    |> Remoting.buildHttpHandler

let loginHandler _next (ctx: HttpContext)  =
    task {
        let cfg = ctx.GetService<IConfiguration>()
        let clientId = Config.getCognitoClientId cfg
        let clientName = Config.getCognitoClientName cfg
        let redirectUrl = Config.getRedirectUrl cfg

        let url = sprintf "%s/login?client_id=%s&response_type=code&redirect_uri=%s" (Aws.getCognitoUri clientName) clientId redirectUrl
        return! redirectTo false url _next ctx
    }

let imgHandler (dir,key) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext)  ->
        task {
            let cfg = ctx.GetService<IConfiguration>()
            let buketName = Config.getFilesAccessPoint cfg

            let! data = Bucket.downloadFile buketName (sprintf "%s/%s" dir key)
            ctx.SetContentType data.ContentType
            return! ctx.WriteStreamAsync true data.Body None None
        }

let indexHandler =
    warbler (fun _ ->
        Data.Quizzes.getDescriptors()
        |> List.filter Domain.Quizzes.isPubQuiz
        |> Index.layout
        |> htmlView
    )

let gameChangedSse = Sse.SseService<QuizChangedEvent>()

CommonService.DomainEvents.subscribeOnQuizChanges (fun evt ->
        Log.Information ("{@Op} {@Evt}", "Event", evt)
        gameChangedSse.Send evt
     )

let sseHandler _next (ctx: HttpContext)  =
    let (|QInt|_|) key =
       match ctx.TryGetQueryStringValue key with
       | Some str ->
           match System.Int32.TryParse(str) with
           | (true,int) -> Some(int)
           | _ -> None
       | None -> None

    let (|QStr|_|) key =
        ctx.TryGetQueryStringValue key

    let error code txt =
        task {
            ctx.SetContentType "plain/text"
            ctx.SetStatusCode code
            return! ctx.WriteTextAsync txt
        }

    task {
        match "quiz", "start", "token" with
        | QInt quizId, QInt startVersion, QStr listenToken ->
            match Data.Quizzes.get quizId with
            | Some quiz when quiz.Dsc.ListenToken = listenToken->
                ctx.SetContentType "text/event-stream"
                do! gameChangedSse.WriteHeartbeat ctx.Response
                //do! ctx.Response.Body.FlushAsync()

                if quiz.Version > startVersion then
                    do! Task.Delay(1000)
                    let evt = Presenter.quizChangeEvent quiz
                    do! gameChangedSse.WriteMessage ctx.Response evt

                gameChangedSse.Subscribe ctx.TraceIdentifier ctx.Response (fun evt -> evt.Id = quizId)

                ctx.RequestAborted.WaitHandle.WaitOne() |> ignore

                gameChangedSse.Unsubscribe ctx.TraceIdentifier

                return None
            | Some _ -> return! error 401 "wrong token"
            | None -> return! error 400 "game not found"

        | _ ->
            return! error 400 "(-)"
    }

let appRouter =
    choose [
        route "/" >=> indexHandler
        route "/index.html" >=> redirectTo false "/"
        route "/default.html" >=> redirectTo false "/"
        route "/login" >=> loginHandler
        routef "/img/%s/%s" imgHandler
        route "/sse" >=> sseHandler
        apiHandler SecurityService.api
        apiHandler MainService.api
        apiHandler AdminService.api
        apiHandler TeamService.api
    ]

let serilogConfig = {
    SerilogConfig.defaults with
        IgnoredRequestFields =
            Ignore.fromRequest
            |> Field.host
            |> Field.queryString
            |> Field.fullPath
            |> Field.userAgent
            |> Field.contentType
            |> Field.requestHeaders
        IgnoredResponseFields =
            Ignore.fromResponse
            |> Field.responseContentType
}
let appRouterWithLogging = SerilogAdapter.Enable(appRouter, serilogConfig)

let awsLogConfig = AWSLoggerConfig("openquiz")

Log.Logger <-
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .WriteTo.AWSSeriLog(awsLogConfig, textFormatter=Formatting.Compact.RenderedCompactJsonFormatter())
    .CreateLogger()

let configureApp (app : IApplicationBuilder) =


#if !DEBUG
    let app = app.UseRewriter (RewriteOptions().AddRedirectToHttps())
#endif

    app.UseResponseCompression()
       .UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe appRouterWithLogging

let configureServices (services : IServiceCollection) =
    services.AddResponseCompression() |> ignore
    services.AddGiraffe() |> ignore

//let realPublicPath = System.IO.Path.Combine(Environment.CurrentDirectory, publicPath)

[<EntryPoint>]
let main _ =
    printfn "Working directory - %s" (Directory.GetCurrentDirectory())

    WebHost
        .CreateDefaultBuilder()
        .UseWebRoot(publicPath)
        .UseContentRoot(publicPath)
        .ConfigureAppConfiguration(fun builder -> builder.AddSystemsManager("/openquiz") |> ignore)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
        .Build()
        .Run()

    0 // return an integer exit code