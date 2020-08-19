open System
open System.IO

open AWS.Logger
open AWS.Logger.SeriLog
open Giraffe
open Giraffe.SerilogExtensions
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open FSharp.Control.Tasks.V2
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
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

let appRouter =
    choose [
        route "/ping" >=> text ("pong")
        route "/index.html" >=> redirectTo false "/"
        route "/default.html" >=> redirectTo false "/"
        route "/login" >=> loginHandler
        apiHandler SecurityService.api
        apiHandler MainService.api
        apiHandler AdminService.api
        apiHandler TeamService.api
        apiHandler RegService.api
        apiHandler AudService.api
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
    app.UseResponseCompression()
       .UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe appRouterWithLogging

let configureServices (services : IServiceCollection) =
    services.AddResponseCompression() |> ignore
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    printfn "Working directory - %s" (Directory.GetCurrentDirectory())
    printfn "ulimit -a \n%s" (Diag.run "ulimit -a")

    let host =
        WebHost
            .CreateDefaultBuilder()
            .UseWebRoot(publicPath)
            .UseContentRoot(publicPath)
            .ConfigureAppConfiguration(fun builder -> builder.AddSystemsManager("/openquiz") |> ignore)
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
            .Build()


    let cfg = host.Services.GetService<IConfiguration>()

    Data2.Quizzes.subscribe (fun quiz ->
            let evt = Presenter.quizChangeEvent quiz
            Log.Information ("{@Op} {@Evt}", "Event", evt)

            Aws.publishQuizMessage
                (Config.getAppsyncEndpoint cfg)
                (Config.getAppsyncRegion cfg)
                quiz.Dsc.QuizId
                quiz.Dsc.ListenToken
                quiz.Version evt
         )

    //Agents.publish (Agents.PublishResults (834,"open-quiz-media"))
    host.Run()

    0 // return an integer exit code