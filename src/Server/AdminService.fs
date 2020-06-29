module rec AdminService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog


open Shared
open Common
open Presenter

module AR = AsyncResult

let api (context:HttpContext) : IAdminApi =
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
                Log.Error ("{Api} {Error} {Quiz}", "admin", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
                |> AsyncResult.fromResult
        )

        SecurityService.exec logger proc <| SecurityService.authorizeAdmin secret (ff f)

    let api : IAdminApi = {
        getTeams = ex  "getTeams" getTeams
        createTeam = ex  "createTeam" createTeam
        createTeamBatch = ex  "createTeamBatch" createTeamBatch
        getTeamCard = ex  "getTeamCard" getTeamCard
        updateTeamCard = ex  "updateTeamCard" updateTeamCard
        changeTeamStatus = ex  "changeTeamStatus" changeTeamStatus
        getQuizCard = ex  "getQuizCard" getQuizCard
        changeQuizStatus = ex  "changeQuizStatus" changeQuizStatus
        getPackages = ex "getPackages" getPackages
        setPackage = ex "setPackage" setPackage
        getPackageCard = ex "getPackageCard" getPackageCard
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
        changeStreamUrl = ex  "changeStreamUrl" changeStreamUrl
    }

    api

let getTeams quiz req =
    Data2.Teams.getDescriptors quiz.QuizId
    |> AR.map (List.map Admin.teamRecord)

let createTeam quiz req =
    let teamName = req.TeamName.Trim()

    let creator teamsInQuiz teamId =
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNewAdmin teamId teamName quiz Domain.Admitted  |> Ok

    Data2.Teams.getDescriptors quiz.QuizId
    |> AR.bind (fun teamsInQuiz ->
        Data2.Teams.create quiz.QuizId (creator teamsInQuiz)
        |> AR.map (fun team -> {|Record = Admin.teamRecord team.Dsc|}))

let createTeamBatch quiz req =

    let creator teamName teamId =
        Domain.Teams.createNewAdmin teamId teamName quiz Domain.Admitted  |> Ok

    req.TeamNames
    |> List.map (fun teamName -> Data2.Teams.create quiz.QuizId (creator teamName))
    |> List.iter (Async.RunSynchronously >> ignore)

    AR.retn ()

let getTeamCard quiz req =
    Data2.Teams.getDescriptor quiz.QuizId req.TeamId
    |> AR.map Admin.teamCard

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

    Data2.Teams.update {QuizId = quiz.QuizId; TeamId = req.TeamId} logic
    |> AR.map (fun team -> Admin.teamRecord team.Dsc)

let changeTeamStatus quiz req =
    let logic (team : Domain.Team) =
        { team with Dsc = { team.Dsc with Status = teamStatusToDomain req.TeamStatus } } |> Ok

    Data2.Teams.update {QuizId = quiz.QuizId; TeamId = req.TeamId} logic
    |> AR.map (fun team -> Admin.teamRecord team.Dsc)

let getQuizCard quiz _ =
    Data2.Quizzes.get quiz.QuizId
    |> AR.map Admin.quizCard

let changeQuizStatus quiz req =
    let logic quiz =
        quiz |> Domain.Quizzes.changeStatus (quizStatusToDomain req.QuizStatus) Data2.Packages.provider |> Ok

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard

let getPackages quiz _ =
    Data2.Experts.get quiz.Producer
    |> AR.bind (fun exp ->
        exp.AllPackages
        |> List.map Data2.Packages.getDescriptor
        |> Async.Sequential
        |> Async.map (Array.choose (function Ok dsc -> Some (packageRecord dsc) | _ -> None) >> List.ofSeq >> Ok ))

let setPackage quiz req =
    let logic quiz = quiz |> Domain.Quizzes.setPackageId req.PackageId |> Ok

    Data2.Experts.get quiz.Producer
    |> AR.bind (fun exp ->
            match req.PackageId with
            | Some packageId -> Domain.Experts.authorizePackageRead packageId exp
            | None -> Ok ()
            |> AR.fromResult)
    |> AR.next (Data2.Quizzes.update quiz.QuizId logic)
    |> AR.map Admin.quizCard

let getPackageCard quiz req =
    Data2.Experts.get quiz.Producer
    |> AR.bind (Domain.Experts.authorizePackageRead req.PackageId >> AR.fromResult)
    |> AR.next (Data2.Packages.get req.PackageId)
    |> AR.map packageCard

let nextQuestion quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestion
        | None -> Error "Question is empty"

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard

let nextQuestionPart quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestionPart
        | None -> Error "Question is empty"

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard

let startCountDown quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.startCountdown DateTime.UtcNow
        | None -> Error "Question is empty"

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard

let pauseCountDown quiz _ =
    Data2.Quizzes.update quiz.QuizId Domain.Quizzes.pauseCountdown
    |> AR.map Admin.quizCard

let settleTour quiz _ =
    Data2.Quizzes.update quiz.QuizId Domain.Quizzes.settle
    |> AR.side settleAnswers
    |> AR.map Admin.quizCard

type SettleItem = {
    Idx : Domain.QwKey
    Jury : string -> bool
    Points: decimal
    JeopardyPoints: decimal option
    WithChoice: bool
}

let settleAnswers (quiz : Domain.Quiz) =

    let now = DateTime.UtcNow

    let logic (items: SettleItem list) (team : Domain.Team) =
        items
        |> List.fold (fun (team,changed) item ->
            let (team,changedNow) = team |> Domain.Teams.settleAnswer item.Idx item.Jury item.Points item.JeopardyPoints item.WithChoice now
            team, (changed || changedNow))
            (team,false)
        |> (function
            | team,true -> Ok team
            | _, false -> Error "Nothing to change")

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

        let sw = Diagnostics.Stopwatch.StartNew()

        Data2.Teams.getIds quiz.Dsc.QuizId
        |> AR.bind (fun list ->
            list
            |> List.map (fun teamId -> Data2.Teams.update {QuizId = quiz.Dsc.QuizId; TeamId = teamId} (logic items))
            |> Async.ParallelThrottle 20
            //|> Async.Sequential
            |> Async.map (fun _ ->
                sw.Stop()
                Log.Information("{@Op} {@Proc} {@Quiz} {@TeamsCount} {@Duration}", "Settle", "admin", quiz.Dsc.QuizId, list.Length, sw.ElapsedMilliseconds)
                Ok ()))
    | _ -> AR.retn ()

let nextTour quiz _ =
    let logic quiz = quiz |> Domain.Quizzes.next Data2.Packages.provider |> Ok

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard

let getAnswers quiz _ =
    Data2.Quizzes.get quiz.QuizId
    |> AR.bind (fun quiz ->
        Data2.Teams.getAllInQuiz quiz.Dsc.QuizId
        |> AR.map (fun teams -> Admin.AnswersBundle quiz teams))

let updateResults quiz req =
    let logic qwKey res team =
        team
        |> Domain.Teams.updateResult qwKey res DateTime.UtcNow
        |> (function team,true -> Ok team | _,false -> Error "Nothing to change")

    req
    |> List.map (fun r -> Data2.Teams.update {QuizId = quiz.QuizId; TeamId = r.TeamId} (logic (qwKeyToDomain r.QwKey) r.Res))
    |> Async.Sequential
    |> Async.map (fun _ -> Ok ())

let getResults quiz _ =
    Data2.Quizzes.get quiz.QuizId
    |> AR.bind (fun quiz ->
        Data2.Teams.getAllInQuiz quiz.Dsc.QuizId
        |> AR.map (fun teams -> {|Teams = teams |> teamResults true; Questions = questionResults quiz|}))

let getListenToken quiz _ =
    quiz.ListenToken |> AR.retn


let changeStreamUrl quiz url =
    let logic (quiz:Domain.Quiz) =
        {quiz with Dsc = {quiz.Dsc with StreamUrl = if url <> "" then Some url else None}} |> Ok

    Data2.Quizzes.update quiz.QuizId logic
    |> AR.map Admin.quizCard
