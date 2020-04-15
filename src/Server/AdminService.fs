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
        getQuizCard = ex  "getQuizCard" <| getQuizCard
        changeQuizStatus = ex  "changeQuizStatus" <| changeQuizStatus
        getPackages = ex "getPackages" <| getPackages
        setPackage = ex "setPackage" <| setPackage
        getPackageCard = ex "getPackageCard" <| getPackageCard
        uploadFile = ex "uploadFile" <| uploadFile (Config.getFilesAccessPoint cfg)
        startCountDown = ex "startCountDown" <| startCountDown
        pauseCountDown = ex "pauseCountDown" <| pauseCountDown
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

let getQuizCard quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz Not Found")
        return Admin.quizCard quiz
    }

let changeQuizStatus quiz req =
    let logic quiz =
        quiz |> Domain.Quizzes.changeStatus (quizStatusToDomain req.QuizStatus) CommonService.packageLoader |> Ok

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let getPackages quiz req =
    result {
        let! exp = (Data.Experts.get quiz.Producer, "Producer Not Found")

        return
            exp.Packages
            |> List.map Data.Packages.getDescriptor
            |> List.filter (fun p -> p.IsSome)
            |> List.map (fun p -> packageRecord p.Value)
    }

let setPackage quiz req =
    let logic quiz =
        quiz |> Domain.Quizzes.setPackageId req.PackageId |> Ok

    result {
        let! exp = (Data.Experts.get quiz.Producer, "Producer Not Found")

        do!
            match req.PackageId with
            | Some id ->
                match exp.Packages |> List.tryFind ((=) id) with
                | Some _ -> Ok ()
                | None -> Error "Package is not produced by quiz producer"
            | None -> Ok ()

        let! quiz = CommonService.updateQuiz quiz.QuizId logic

        return Admin.quizCard quiz
    }

let getPackageCard quiz req =
    result {
        let! exp = (Data.Experts.get quiz.Producer, "Producer Not Found")

        let! _ = (exp.Packages |> List.tryFind ((=) req.PackageId), "Package is not produced by quiz producer")

        return
            match Data.Packages.get req.PackageId with
            | Some pkg -> Some (packageCard pkg)
            | None -> None
    }


let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody

let startCountDown quiz req =
    let logic quiz =
        match req.CurrentQw with
        | Some qw ->
            quiz |> Domain.Quizzes.startCountdown qw.Name qw.Seconds qw.Text qw.ImgKey
                            qw.Answer qw.Comment qw.CommentImgKey req.PackageQwIdx DateTime.UtcNow
        | None -> Error "Question is empty"

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let pauseCountDown quiz _ =
    let logic quiz =
        quiz |> Domain.Quizzes.pauseCountdown

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }
