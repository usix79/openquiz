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
            |> AsyncResult.ret
        )

        SecurityService.exec logger proc <| SecurityService.authorizeAdmin secret (ff f)

    let api : IAdminApi = {
        getTeams = ex  "getTeams" getTeams
        createTeam = ex  "createTeam" createTeam
        getTeamCard = ex  "getTeamCard" getTeamCard
        updateTeamCard = ex  "updateTeamCard" updateTeamCard
        changeTeamStatus = ex  "changeTeamStatus" changeTeamStatus
        getQuizCard = ex  "getQuizCard" getQuizCard
        changeQuizStatus = ex  "changeQuizStatus" changeQuizStatus
        getPackages = ex "getPackages" getPackages
        setPackage = ex "setPackage" setPackage
        getPackageCard = ex "getPackageCard" getPackageCard
        uploadFile = ex "uploadFile" <| uploadFile (Config.getFilesAccessPoint cfg)
        startCountDown = ex "startCountDown" startCountDown
        pauseCountDown = ex "pauseCountDown" pauseCountDown
        settleTour = ex "settleTour" settleTour
        nextTour = ex "nextTour" nextTour
        nextQuestion = ex "nextQuestion" nextQuestion
        nextQuestionPart = ex "nextQuestionPart" nextQuestionPart
        getAnswers = ex "getAnswers" getAnswers
        updateResults = ex "updateResults" updateResults
        getResults = ex "getResults" getResults
        getListenToken = ex "getListenToken" getListenToken
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
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNewAdmin key.TeamId teamName quiz Domain.Admitted  |> Ok

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
        let! exp = async{ return! Data2.Experts.get quiz.Producer } |> Async.RunSynchronously

        return
            exp.AllPackages
            |> List.map Data.Packages.getDescriptor
            |> List.filter (fun p -> p.IsSome)
            |> List.map (fun p -> packageRecord p.Value)
    }

let setPackage quiz req =
    let logic quiz =
        quiz |> Domain.Quizzes.setPackageId req.PackageId |> Ok

    result {
        let! exp = async{ return! Data2.Experts.get quiz.Producer } |> Async.RunSynchronously

        do!
            match req.PackageId with
            | Some id when not <| Domain.Experts.isAuthorizedForPackage id exp  -> Error "You are not autorized to access the package"
            | _ -> Ok ()

        let! quiz = CommonService.updateQuiz quiz.QuizId logic

        return Admin.quizCard quiz
    }

let getPackageCard quiz req =
    result {
        let! exp = async{ return! Data2.Experts.get quiz.Producer } |> Async.RunSynchronously

        do!
            if not <| Domain.Experts.isAuthorizedForPackage req.PackageId exp then Error "You are not autorized to access the package"
            else Ok ()

        return
            match Data.Packages.get req.PackageId with
            | Some pkg -> Some (packageCard pkg)
            | None -> None
    }


let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody

let nextQuestion quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestion
        | None -> Error "Question is empty"

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let nextQuestionPart quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestionPart
        | None -> Error "Question is empty"

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let startCountDown quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.startCountdown DateTime.UtcNow
        | None -> Error "Question is empty"

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let pauseCountDown quiz _ =
    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId Domain.Quizzes.pauseCountdown
        return Admin.quizCard quiz
    }

let settleTour quiz _ =
    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId Domain.Quizzes.settle
        settleAnswers quiz
        return Admin.quizCard quiz
    }

type SettleItem = {
    Idx : Domain.QwKey
    Jury : string -> bool
    Points: decimal
    JeopardyPoints: decimal option
    WithChoice: bool
}

let settleAnswers (quiz : Domain.Quiz) =

    let logic (items: SettleItem list) (team : Domain.Team) =
        let now = DateTime.UtcNow
        items
        |> List.fold (fun team item -> team |> Domain.Teams.settleAnswer item.Idx item.Jury item.Points item.JeopardyPoints item.WithChoice now) team
        |> Ok

    let createItem (slip:Domain.SingleAwSlip) qwIdx =
        if not (String.IsNullOrWhiteSpace slip.Answer) then
            let key = {Domain.QwKey.TourIdx = quiz.CurrentTourIndex; Domain.QwKey.QwIdx = qwIdx}
            Some {Idx = key ; Jury = (Jury.jury slip.Answer); Points = slip.Points; JeopardyPoints = slip.JeopardyPoints; WithChoice = slip.WithChoice}
        else
            None

    match quiz.CurrentTour with
    | Some tour ->
        let items =
            match tour.Slip with
            | Domain.Single slip -> [createItem slip 0]
            | Domain.Multiple (_, slips) -> slips |> List.mapi (fun idx slip -> createItem slip idx)
            |> List.choose id

        async {
            for teamId in Data.Teams.getIds quiz.Dsc.QuizId do
                CommonService.updateTeamNoReply {QuizId = quiz.Dsc.QuizId; TeamId = teamId} (logic items)
        } |> Async.Start

    | _ -> ()

let nextTour quiz _ =
    let logic quiz =
        quiz |> Domain.Quizzes.next CommonService.packageLoader |> Ok

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic
        return Admin.quizCard quiz
    }

let getAnswers quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")
        let teams = Data.Teams.getAllInQuiz quiz.Dsc.QuizId

        return Admin.AnswersBundle quiz teams
    }

let updateResults quiz req =
    let logic qwKey res team =
        team |> Domain.Teams.updateResult qwKey res DateTime.UtcNow |> Ok

    for r in req do
        CommonService.updateTeamNoReply {QuizId = quiz.QuizId; TeamId = r.TeamId} (logic (qwKeyToDomain r.QwKey) r.Res)

    Ok()

let getResults quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")
        let teams = Data.Teams.getAllInQuiz quiz.Dsc.QuizId

        return {|Teams = teams |> teamResults true; Questions = questionResults quiz|}
    }

let getListenToken quiz _ =
    quiz.ListenToken |> Ok