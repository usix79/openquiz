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
open Env

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let port =
    "SERVER_PORT"
    |> tryGetEnv
    |> Option.map uint16
    |> Option.defaultValue 8085us

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

let appRouter env =
    choose [
        route "/ping" >=> text ("pong")
        route "/api/login" >=> loginHandler
        route "/api/ping" >=> text ("pong")
        apiHandler <| SecurityService.api env
        apiHandler <| MainService.api env
        apiHandler <| AdminService.api env
        apiHandler <| TeamService.api env
        apiHandler <| RegService.api env
        apiHandler <| AudService.api env
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

let appRouterWithLogging env = SerilogAdapter.Enable(appRouter env, serilogConfig)

let buildEnvironment logger =
    let configurer = {
        new Env.IConfigurer with
        member _.DynamoTablePrefix = "OQ"
    }

    let dbEnv = {
        new Env.IDbEnv with
        member _.Logger = logger
        member _.Configurer = configurer
    }

    let database = {
        new Env.IDatabase with
        member _.GetItem (tableName, reader) = Data2.getItem' dbEnv tableName reader
        member _.PutItem (tableName, version) = Data2.putItem' dbEnv version tableName
        member _.DelItem (tableName) = Data2.deleteItem' dbEnv tableName
        member _.CheckItem (tableName) = Data2.checkItem' dbEnv tableName
        member _.GetProjection (tableName, reader, projection) = Data2.getItemProjection' dbEnv projection tableName reader
        member _.PutOptimistic (id, get, put, logic) = Data2.putOptimistic' dbEnv get put id logic
        member _.Query (tableName, projection, reader) = Data2.query' dbEnv tableName projection reader
    }

    { new Env.IAppEnv with
        member _.Logger = logger
        member _.Configurer = configurer
        member _.Database = database
    }

[<EntryPoint>]
let main args =
    printfn "Working directory - %s" (Directory.GetCurrentDirectory())
    printfn "ulimit -a \n%s" (Diag.run "ulimit -a")

    let host =
        WebHost
            .CreateDefaultBuilder(args)
            .ConfigureAppConfiguration(fun ctx builder ->
                printfn "Configuring App:%A Env: %A" ctx.HostingEnvironment.ApplicationName ctx.HostingEnvironment.EnvironmentName
                builder.AddSystemsManager("/OpenQuiz/" + ctx.HostingEnvironment.EnvironmentName) |> ignore )
            .Configure(fun ctx app ->
                let logger =
                  LoggerConfiguration()
                    .Destructure.FSharpTypes()
                    .WriteTo.Console()
                    .WriteTo.AWSSeriLog(
                        AWSLoggerConfig("OpenQuiz-" + ctx.HostingEnvironment.EnvironmentName),
                        textFormatter=Formatting.Compact.RenderedCompactJsonFormatter())
                    .CreateLogger()

                Log.Logger <- logger

                let env = buildEnvironment logger

                app.UseGiraffe (appRouterWithLogging env)
                )
            .ConfigureServices(fun services ->
                services.AddGiraffe() |> ignore )
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