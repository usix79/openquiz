module rec AdminService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog


open Shared
open Common
open Env
open Presenter

module AR = AsyncResult

let api env (context:HttpContext) : IAdminApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =

        let ff f = (fun (quizIdStr:string) req ->
            match tryParseInt32 quizIdStr with
            | Some quizId ->
                Data2.Quizzes.getDescriptor env quizId
                |> AsyncResult.bind (fun quiz -> f quiz req)
            | None ->
                Log.Error ("{Api} {Error} {Quiz}", "admin", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
                |> AsyncResult.fromResult
        )

        SecurityService.exec logger proc <| SecurityService.authorizeAdmin secret (ff f)

    let api : IAdminApi = {
        getTeams = ex  "getTeams" <| getTeams env
        createTeam = ex  "createTeam" (createTeam env (Config.getMediaBucketName cfg))
        createTeamBatch = ex  "createTeamBatch" <| createTeamBatch env
        getTeamCard = ex  "getTeamCard" <| getTeamCard env
        updateTeamCard = ex  "updateTeamCard" (updateTeamCard env (Config.getMediaBucketName cfg))
        changeTeamStatus = ex  "changeTeamStatus" <| changeTeamStatus env
        getQuizCard = ex  "getQuizCard" <| getQuizCard env
        changeQuizStatus = ex  "changeQuizStatus" <| changeQuizStatus env
        getPackages = ex "getPackages" <| getPackages env
        setPackage = ex "setPackage" <| setPackage env
        getPackageCard = ex "getPackageCard" <| getPackageCard env
        startCountDown = ex "startCountDown" <| startCountDown env
        pauseCountDown = ex "pauseCountDown" <| pauseCountDown env
        settleTour = ex "settleTour" (settleTour env (Config.getMediaBucketName cfg))
        nextTour = ex "nextTour" <| nextTour env
        nextQuestion = ex "nextQuestion" <| nextQuestion env
        nextQuestionPart = ex "nextQuestionPart" <| nextQuestionPart env
        showQuestion = ex "showQuestion" <| showQuestion env
        getAnswers = ex "getAnswers" <| getAnswers env
        updateResults = ex "updateResults" (updateResults env (Config.getMediaBucketName cfg))
        getListenToken = ex "getListenToken" <| getListenToken env
        changeStreamUrl = ex  "changeStreamUrl" <| changeStreamUrl env
    }

    api

let getTeams env quiz req =
    Data2.Teams.getDescriptors env quiz.QuizId
    |> AR.map (List.map Admin.teamRecord)

let createTeam env bucketName quiz req =
    let teamName = req.TeamName.Trim()

    let creator teamsInQuiz teamId =
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNewAdmin teamId teamName quiz Domain.Admitted  |> Ok

    Data2.Teams.getDescriptors env quiz.QuizId
    |> AR.bind (fun teamsInQuiz ->
        Data2.Teams.create env quiz.QuizId (creator teamsInQuiz)
        |> AR.map (fun team -> {|Record = Admin.teamRecord team.Dsc|}))
    |> AR.side (fun _ -> PublishResults (quiz.QuizId, bucketName) |> (env :> IPublisher).Publish |> AR.retn)

let createTeamBatch env quiz req =

    let creator teamName teamId =
        Domain.Teams.createNewAdmin teamId teamName quiz Domain.Admitted  |> Ok

    req.TeamNames
    |> List.map (fun teamName -> Data2.Teams.create env quiz.QuizId (creator teamName))
    |> List.iter (Async.RunSynchronously >> ignore)

    AR.retn ()

let getTeamCard env quiz req =
    Data2.Teams.getDescriptor env quiz.QuizId req.TeamId
    |> AR.map Admin.teamCard

let updateTeamCard env bucketName quiz req =
    let logic (team : Domain.Team) =
        { team with
            Dsc = {
                team.Dsc with
                    Name = req.TeamName
                    Status = teamStatusToDomain req.TeamStatus
                    EntryToken = req.EntryToken
            }
        } |> Ok

    Data2.Teams.update env {QuizId = quiz.QuizId; TeamId = req.TeamId} logic
    |> AR.map (fun team -> Admin.teamRecord team.Dsc)
    |> AR.side (fun _ -> PublishResults (quiz.QuizId, bucketName) |> env.Publish |> AR.retn)

let changeTeamStatus env quiz req =
    let logic (team : Domain.Team) =
        { team with Dsc = { team.Dsc with Status = teamStatusToDomain req.TeamStatus } } |> Ok

    Data2.Teams.update env {QuizId = quiz.QuizId; TeamId = req.TeamId} logic
    |> AR.map (fun team -> Admin.teamRecord team.Dsc)

let getQuizCard env quiz _ =
    Data2.Quizzes.get env quiz.QuizId
    |> AR.map Admin.quizCard

let changeQuizStatus env quiz req =
    let logic quiz =
        quiz |> Domain.Quizzes.changeStatus (quizStatusToDomain req.QuizStatus) (Data2.Packages.provider env) |> Ok

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let getPackages env quiz _ =
    Data2.Experts.get env quiz.Producer
    |> AR.bind (fun exp ->
        exp.AllPackages
        |> List.map (Data2.Packages.getDescriptor env)
        |> Async.Sequential
        |> Async.map (Array.choose (function Ok dsc -> Some (packageRecord dsc) | _ -> None) >> List.ofSeq >> Ok ))

let setPackage env quiz req =
    let logic quiz = quiz |> Domain.Quizzes.setPackageId req.PackageId |> Ok

    Data2.Experts.get env quiz.Producer
    |> AR.bind (fun exp ->
            match req.PackageId with
            | Some packageId -> Domain.Experts.authorizePackageRead packageId exp
            | None -> Ok ()
            |> AR.fromResult)
    |> AR.next (Data2.Quizzes.update env quiz.QuizId logic)
    |> AR.map Admin.quizCard

let getPackageCard env quiz req =
    Data2.Experts.get env quiz.Producer
    |> AR.bind (Domain.Experts.authorizePackageRead req.PackageId >> AR.fromResult)
    |> AR.next (Data2.Packages.get env req.PackageId)
    |> AR.map packageCard

let nextQuestion env quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestion
        | None -> Error "Question is empty"

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let nextQuestionPart env quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.nextQuestionPart
        | None -> Error "Question is empty"

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let showQuestion env quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.showQuestion
        | None -> Error "Question is empty"

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let startCountDown env quiz req =
    let logic quiz =
        match req.CurrentTour with
        | Some tour ->
            quiz
            |> Domain.Quizzes.update tour.Name tour.Seconds req.PackageSlipIdx tour.QwIdx tour.QwPartIdx (slipToDomain tour.Slip)
            |> Domain.Quizzes.startCountdown DateTime.UtcNow
        | None -> Error "Question is empty"

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let pauseCountDown env quiz _ =
    Data2.Quizzes.update env quiz.QuizId Domain.Quizzes.pauseCountdown
    |> AR.map Admin.quizCard

let settleTour env bucketName quiz _ =
    Data2.Quizzes.update env quiz.QuizId Domain.Quizzes.settle
    |> AR.side (settleAnswers env)
    |> AR.map Admin.quizCard
    |> AR.side (fun _ -> PublishResults (quiz.QuizId, bucketName) |> env.Publish |> AR.retn)

type SettleItem = {
    Idx : Domain.QwKey
    Jury : string -> bool
    Points: decimal
    JeopardyPoints: decimal option
    WithChoice: bool
}

let settleAnswers env (quiz : Domain.Quiz) =

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

    let createItem (slip:Domain.SingleSlip) qwIdx =
        if not (String.IsNullOrWhiteSpace (slip.Answer.ToRawString())) then
            let key = {Domain.QwKey.TourIdx = quiz.CurrentTourIndex; Domain.QwKey.QwIdx = qwIdx}
            Some {Idx = key ; Jury = (Jury.jury (slip.Answer.ToRawString())); Points = slip.Points; JeopardyPoints = slip.JeopardyPoints; WithChoice = slip.WithChoice}
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

        Data2.Teams.getIds env quiz.Dsc.QuizId
        |> AR.bind (fun list ->
            list
            |> List.map (fun teamId -> Data2.Teams.update env {QuizId = quiz.Dsc.QuizId; TeamId = teamId} (logic items))
            |> Async.ParallelThrottle 20
            //|> Async.Sequential
            |> Async.map (fun _ ->
                sw.Stop()
                Log.Information("{@Op} {@Proc} {@Quiz} {@TeamsCount} {@Duration}", "Settle", "admin", quiz.Dsc.QuizId, list.Length, sw.ElapsedMilliseconds)
                Ok ()))
    | _ -> AR.retn ()

let nextTour env quiz _ =
    let logic quiz = quiz |> Domain.Quizzes.next (Data2.Packages.provider env) |> Ok

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard

let getAnswers env quiz range =
    Data2.Quizzes.get env quiz.QuizId
    |> AR.bind (fun quiz ->
        match range with
        | Some range -> Data2.Teams.getRangeInQuiz env range.From range.To quiz.Dsc.QuizId
        | None -> Data2.Teams.getAllInQuiz env quiz.Dsc.QuizId
        |> AR.map (fun teams -> Admin.AnswersBundle quiz teams))

let updateResults env bucketName quiz req  =
    let logic qwKey res team =
        team
        |> Domain.Teams.updateResult qwKey res DateTime.UtcNow
        |> (function team,true -> Ok team | _,false -> Error "Nothing to change")

    req
    |> List.map (fun r -> Data2.Teams.update env {QuizId = quiz.QuizId; TeamId = r.TeamId} (logic (qwKeyToDomain r.QwKey) r.Res))
    |> Async.Sequential
    |> Async.map (fun _ -> Ok ())
    |> AR.side (fun _ -> PublishResults (quiz.QuizId, bucketName) |> env.Publish |> AR.retn)

let getListenToken env quiz _ =
    quiz.ListenToken |> AR.retn


let changeStreamUrl env quiz url =
    let logic (quiz:Domain.Quiz) =
        {quiz with Dsc = {quiz.Dsc with StreamUrl = if url <> "" then Some url else None}} |> Ok

    Data2.Quizzes.update env quiz.QuizId logic
    |> AR.map Admin.quizCard
