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
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
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

let loginHandler env _next (ctx: HttpContext)  =
    task {
        let clientId = (env:>ICfg).Configurer.UserPoolClientId
        let redirectUrl = env.Configurer.AppUrl
        let url = sprintf "%s/login?client_id=%s&response_type=code&redirect_uri=%s" env.Configurer.LoginUrl clientId redirectUrl
        return! redirectTo false url _next ctx
    }

let appRouter env =
    choose [
        route "/ping" >=> text ("pong")
        route "/api/login" >=> loginHandler env
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

let buildEnvironment envName logger (cfg:IConfiguration) =
    let configurer = {
        new Env.IConfigurer with
        member _.DynamoTablePrefix = "OpenQuiz-" + envName
        member _.JwtSecret = cfg.["jwtsecret"]
        member _.UserPoolClientId = cfg.["UserPoolClientId"]
        member _.LoginUrl = cfg.["LoginUrl"]
        member _.AppUrl = cfg.["AppUrl"]
        member _.BucketName = cfg.["BucketName"]
        member _.BucketUrl = cfg.["BucketUrl"]
        member _.AppSyncCfg = {
            Endpoint = cfg.["appsync-endpoint"]
            Region = cfg.["appsync-region"]
            ApiKey = cfg.["appsync-apikey"]
        }
    }

    let publisherEnv = {
        new Env.IPublisherEnv with
        member _.Logger = logger
        member _.Configurer = configurer
    }

    let publisherAgent = Agents.publisherAgent publisherEnv

    { new Env.IAppEnv with
        member _.Logger = logger
        member _.Configurer = configurer
        member _.Publish cmd =
            printfn "PUBLISH: %A" cmd
            publisherAgent.Post cmd
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

                let env = buildEnvironment ctx.HostingEnvironment.EnvironmentName logger ctx.Configuration

                Data2.Quizzes.subscribe (fun quiz ->
                        let event' = Presenter.quizChangeEvent quiz
                        env.Logger.Information ("{@Op} {@Evt}", "Event", event')

                        Aws.publishQuizMessage
                            env
                            quiz.Dsc.QuizId
                            quiz.Dsc.ListenToken
                            quiz.Version
                            event' )

                app.UseGiraffe (appRouterWithLogging env) )
            .ConfigureServices(fun services ->
                services.AddGiraffe() |> ignore )
            .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
            .Build()

    //Agents.publish (Agents.PublishResults (834,"open-quiz-media"))
    host.Run()

    0 // return an integer exit code