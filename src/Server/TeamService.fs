module rec TeamService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let api (context:HttpContext) : ITeamApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =

        let ff f = (fun (sessionId:int) (teamKey:Domain.TeamKey) req ->
            match Data.Teams.getDescriptor teamKey.QuizId teamKey.TeamId with
            | Some team when team.ActiveSessionId = sessionId -> f team req
            | Some _ -> Error Errors.SessionIsNotActive
            | None -> Error "Team not found"
            |> AsyncResult.ret
        )

        SecurityService.exec logger proc <| SecurityService.authorizeTeam secret (ff f)

    let api : ITeamApi = {
        takeActiveSession = SecurityService.exec logger "takeActiveSession" <| SecurityService.authorizeTeam secret takeActiveSession
        getState = ex "getState" getState
        answers = ex "answers" answers
        getHistory = ex "getHistory" getHistory
        getResults = ex "getResults" getResults
    }

    api

let getState team _ =
    result{
        let! quiz = (Data.Quizzes.get team.QuizId, "Quiz not found")
        let! team = (Data.Teams.get team.QuizId team.TeamId, "Team not found")

        return Teams.quizCard quiz team
    }

let takeActiveSession (sessionId:int) (teamKey:Domain.TeamKey) req =
    let logic (team:Domain.Team) =
        team |> Domain.Teams.dsc (fun dsc -> {dsc with ActiveSessionId = sessionId} |> Ok)

    result{
        let! team = CommonService.updateTeam teamKey logic
        let! quiz = (Data.Quizzes.get team.Dsc.QuizId, "Quiz not found")

        return Teams.quizCard quiz team
    } |> AsyncResult.ret

let answers team req =
    let logic (team:Domain.Team) =
        let now = DateTime.UtcNow
        req |> Map.toList
        |> List.fold (fun team (qwKey,(aw,jpd)) -> team |> Result.bind (Domain.Teams.registerAnswer (qwKeyToDomain qwKey) aw jpd now)) (Ok team)

    match team.Status with
    | Domain.Admitted ->
        CommonService.updateTeamNoReply team.Key logic
        Ok ()
    | _ -> Error "Team's status does not suppose sending answers"

let getHistory team _ =
    result{
        let! quiz = (Data.Quizzes.get team.QuizId, "Quiz not found")
        let! team = (Data.Teams.get team.QuizId team.TeamId, "Team not found")

        return Teams.quizHistory quiz team
    }

let getResults quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")
        let teams = Data.Teams.getAllInQuiz quiz.Dsc.QuizId

        return {|Teams = teams |> teamResults false; Questions = questionResults quiz|}
    }
