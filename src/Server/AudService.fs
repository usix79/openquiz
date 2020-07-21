module rec AudService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let api (context:HttpContext) : IAudApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =

        let ff f = (fun (quizIdStr:string) req ->
            match tryParseInt32 quizIdStr with
            | Some quizId ->
                Data2.Quizzes.getDescriptor quizId
                |> AsyncResult.bind (fun quiz -> f quiz req)
            | None ->
                Log.Error ("{Api} {Error} {Quiz}", "aud", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
                |> AsyncResult.fromResult
        )

        SecurityService.exec logger proc <| SecurityService.authorizeAudience secret (ff f)

    let api : IAudApi = {
        getQuiz = ex "getState" getQuiz
        getHistory = ex "getHistory" getHistory
    }

    api

let getQuiz quiz _ =
    Data2.Quizzes.get quiz.QuizId
    |> AsyncResult.map Audience.quizCard

let getHistory quiz _ =
    Data2.Quizzes.get quiz.QuizId
    |> AsyncResult.map Audience.quizHistory
