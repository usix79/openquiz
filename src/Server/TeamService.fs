module rec TeamService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Env
open Presenter

let api env (context:HttpContext) : ITeamApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret =  (env :> ICfg).Configurer.JwtSecret

    let ex proc f =

        let ff f = (fun (sessionId:int) (teamKey:Domain.TeamKey) req ->
            Data2.Teams.getDescriptor env teamKey.QuizId teamKey.TeamId
            |> AsyncResult.bind (fun team ->
                if team.ActiveSessionId = sessionId then f team req
                else (Error Errors.SessionIsNotActive) |> AsyncResult.fromResult
                )
        )

        SecurityService.exec logger proc <| SecurityService.authorizeTeam secret (ff f)

    let api : ITeamApi = {
        takeActiveSession = SecurityService.exec logger "takeActiveSession" <| SecurityService.authorizeTeam secret (takeActiveSession env)
        getState = ex "getState" <| getState env
        answers = ex "answers" <| answers env
        getHistory = ex "getHistory" <| getHistory env
        vote = ex "vote" <| vote env
    }

    api

let getState env team _ =
    Data2.Quizzes.get env team.QuizId
    |> AsyncResult.bind (fun quiz ->
        Data2.Teams.get env team.Key
        |> AsyncResult.map (Teams.quizCard quiz))

let takeActiveSession env (sessionId:int) (teamKey:Domain.TeamKey) req =
    let logic (team:Domain.Team) =
        team |> Domain.Teams.dsc (fun dsc -> {dsc with ActiveSessionId = sessionId} |> Ok)

    Data2.Teams.update env teamKey logic
    |> AsyncResult.bind (fun team ->
        Data2.Quizzes.get env team.Dsc.QuizId
        |> AsyncResult.map (fun quiz -> Teams.quizCard quiz team))

let answers env team req =
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
    |> AsyncResult.next (Data2.Teams.update env team.Key logic)
    |> AsyncResult.map ignore

let getHistory env team _ =
    Data2.Quizzes.get env team.QuizId
    |> AsyncResult.bind (fun quiz ->
        Data2.Teams.get env team.Key
        |> AsyncResult.map (fun team -> Teams.quizHistory quiz team))


let vote env team req =
    let logic (team:Domain.Team) =
        team
        |> Domain.Teams.updateAnswer (qwKeyToDomain req.Key) (fun aw ->
            {aw with Vote = req.Vote}, (aw.Vote <> req.Vote))
        |> function
        | team, true -> Ok team
        | _ -> Error "vote not applied"

    Data2.Teams.update env team.Key logic
    |> AsyncResult.side (fun _ -> PublishResults team.QuizId |> (env:>IPublisher).Publish |> AsyncResult.retn)
    |> AsyncResult.map ignore