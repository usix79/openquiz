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
            match Int32.TryParse quizIdStr with
            | true, quizId ->
                match Data.Quizzes.getDescriptor quizId with
                | Some quiz -> f quiz req
                | None ->
                    Log.Error ("{Api} {Error} {Quiz}", "aud", "Quiz Not Found", quizId)
                    Error "Quiz not found"
            | _ ->
                Log.Error ("{Api} {Error} {Quiz}", "aud", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
        )

        SecurityService.exec logger proc <| SecurityService.authorizeAudience secret (ff f)

    let api : IAudApi = {
        getQuiz = ex "getState" getQuiz
        getHistory = ex "getHistory" getHistory
        getResults = ex "getResults" getResults
    }

    api

let getQuiz quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")

        return Audience.quizCard quiz
    }

let getHistory quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")

        return Audience.quizHistory quiz
    }

let getResults quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")
        let teams = Data.Teams.getAllInQuiz quiz.Dsc.QuizId

        return {|Teams = teams |> teamResults false; Questions = questionResults quiz|}
    }