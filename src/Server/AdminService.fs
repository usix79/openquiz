module rec AdminService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let api (context:HttpContext) : IAdminApi =
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
                    Log.Error ("{Api} {Error} {Quiz}", "admin", "Quiz Not Found", quizId)
                    Error "Quiz not found"
            | _ ->
                Log.Error ("{Api} {Error} {Quiz}", "admin", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
        )

        Security.execute logger proc <| Security.authorizeAdmin secret (ff f)

    let api : IAdminApi = {
        getTeams = ex  "getTeams" <| getTeams
        createTeam = ex  "createTeam" <| createTeam
        getTeamCard = ex  "getTeamCard" <| getTeamCard
        updateTeamCard = ex  "updateTeamCard" <| updateTeamCard
        changeTeamStatus = ex  "changeTeamStatus" <| changeTeamStatus
    }

    api

let getTeams quiz req =
    Data.Teams.getDescriptors quiz.QuizId
    |> List.map Admin.teamRecord
    |> Ok

let createTeam quiz req =
    let teamName = req.TeamName.Trim()

    let creator (key : Domain.TeamKey) =
        let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId
        match Domain.Teams.validatePublicTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew key.TeamId teamName quiz |> Ok

    result {
        let! team = CommonService.createTeam quiz.QuizId creator

        return! Ok {|Record = Admin.teamRecord team.Dsc|}
    }

let getTeamCard quiz req =
    match Data.Teams.getDescriptor quiz.QuizId req.TeamId with
    | Some team -> Admin.teamCard team |> Ok
    | None -> Error "Team not found"

let updateTeamCard quiz req =
    let logic (team : Domain.Team) =
        { team with
            Dsc = {
                team.Dsc with
                    Name = req.TeamName
                    Status = teamStatusToDomain req.TeamStatus
                    EntryToken = req.EntryToken
            }
        } |> Ok

    match CommonService.updateTeam {QuizId = quiz.QuizId; TeamId = req.TeamId} logic with
    | Ok team -> Ok <| Admin.teamRecord team.Dsc
    | Error txt -> Error txt

let changeTeamStatus quiz req =
    let logic (team : Domain.Team) =
        { team with
            Dsc = {
                team.Dsc with
                    Status = teamStatusToDomain req.TeamStatus
            }
        } |> Ok

    match CommonService.updateTeam {QuizId = quiz.QuizId; TeamId = req.TeamId} logic with
    | Ok team -> Ok <| Admin.teamRecord team.Dsc
    | Error txt -> Error txt
