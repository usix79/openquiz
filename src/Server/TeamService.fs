module rec TeamService

open System
open System.Text.RegularExpressions
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
        )

        SecurityService.execute logger proc <| SecurityService.authorizeTeam secret (ff f)

    let api : ITeamApi = {
        takeActiveSession = SecurityService.execute logger "takeActiveSession" <| SecurityService.authorizeTeam secret takeActiveSession
        getState = ex "getState" getState
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
    }
