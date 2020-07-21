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
            Data2.Teams.getDescriptor teamKey.QuizId teamKey.TeamId
            |> AsyncResult.bind (fun team ->
                if team.ActiveSessionId = sessionId then f team req
                else (Error Errors.SessionIsNotActive) |> AsyncResult.fromResult
                )
        )

        SecurityService.exec logger proc <| SecurityService.authorizeTeam secret (ff f)

    let api : ITeamApi = {
        takeActiveSession = SecurityService.exec logger "takeActiveSession" <| SecurityService.authorizeTeam secret takeActiveSession
        getState = ex "getState" getState
        answers = ex "answers" answers
        getHistory = ex "getHistory" getHistory
    }

    api

let getState team _ =
    Data2.Quizzes.get team.QuizId
    |> AsyncResult.bind (fun quiz ->
        Data2.Teams.get team.Key
        |> AsyncResult.map (Teams.quizCard quiz))

let takeActiveSession (sessionId:int) (teamKey:Domain.TeamKey) req =
    let logic (team:Domain.Team) =
        team |> Domain.Teams.dsc (fun dsc -> {dsc with ActiveSessionId = sessionId} |> Ok)

    Data2.Teams.update teamKey logic
    |> AsyncResult.bind (fun team ->
        Data2.Quizzes.get team.Dsc.QuizId
        |> AsyncResult.map (fun quiz -> Teams.quizCard quiz team))

let answers team req =
    let logic (team:Domain.Team) =
        let now = DateTime.UtcNow
        req
        |> Map.toList
        |> List.fold (fun team (qwKey,(aw,jpd)) ->
            team |> Result.bind (Domain.Teams.registerAnswer (qwKeyToDomain qwKey) aw jpd now)) (Ok team)

    match team.Status with
    | Domain.Admitted -> Ok ()
    | _ -> Error "Team's status does not suppose sending answers"
    |> AsyncResult.fromResult
    |> AsyncResult.next (Data2.Teams.update team.Key logic)
    |> AsyncResult.map ignore

let getHistory team _ =
    Data2.Quizzes.get team.QuizId
    |> AsyncResult.bind (fun quiz ->
        Data2.Teams.get team.Key
        |> AsyncResult.map (fun team -> Teams.quizHistory quiz team))
