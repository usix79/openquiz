module rec AdminService

open System
open System.Text.RegularExpressions
open F23.StringSimilarity
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

        SecurityService.execute logger proc <| SecurityService.authorizeAdmin secret (ff f)

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
        finishQuestion = ex "finishQuestion" finishQuestion
        nextQuestion = ex "nextQuestion" nextQuestion
        getAnswers = ex "getAnswers" getAnswers
        updateResults = ex "updateResults" updateResults
        getResults = ex "getResults" getResults
    }

    api

module Jury =
    let allowedCharacters = Regex ("[^\w\+\-\.]+", RegexOptions.Compiled)

    let private normalizeWords (words: string array) =
        String.Concat(words).ToLower()

    let private normalize txt =
        normalizeWords <| allowedCharacters.Split txt

    let private allowedDistance (words: string list) =
        words
        |> List.fold (fun sum word -> sum + if word.Length > 4 then 1.0 else 0.0) 0.0

    let private splitOnParts (answer : string) =

        let parseBracketsText (txt:string) =
            match txt.Split('/') with
            | res when res.Length = 1 -> [|txt; ""|]
            | res -> res
            |> List.ofSeq

        let mutable isInsideBrackets = false
        let mutable parts = Collections.Generic.List<string list>()
        let mutable txt = Text.StringBuilder()

        for ch in answer do
            match ch with
            | '[' when not isInsideBrackets ->
                if txt.Length > 0 then parts.Add [txt.ToString()]
                txt.Clear() |> ignore
                isInsideBrackets <- true
            | ']' when isInsideBrackets ->
                if txt.Length > 0 then parts.Add (parseBracketsText (txt.ToString()))
                txt.Clear() |> ignore
                isInsideBrackets <- false
            | _ -> txt.Append ch |> ignore

        if txt.Length > 0 then
            match isInsideBrackets with
            | true -> parts.Add (parseBracketsText (txt.ToString()))
            | false -> parts.Add [txt.ToString()]

        parts |> List.ofSeq

    let rec private permutateParts (parts: string list list) : string list list =
        match parts with
        | [head] -> head |> List.map (fun s -> [s])
        | head::tail ->
            head |> List.collect (fun s -> tail |> permutateParts |> List.map (fun t -> s :: t))
        | [] -> []

    let private allSubstitutions (answer: string) =
        splitOnParts answer
        |> permutateParts
        |> List.map String.Concat

    let private allPermutations (words: string list) =
        let rec distribute e = function
          | [] -> [[e]]
          | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

        let rec permute = function
          | [] -> [[]]
          | e::xs -> List.collect (distribute e) (permute xs)

        if words.Length < 5 then
            permute words
        else
            [words] // skip permutation for 5 and more words (5! = 120 cases)

    let private extractCorrectAnswers (answerTxt:string)=
        answerTxt.Split('\n')
        |> List.ofArray
        |> List.collect allSubstitutions
        |> List.map (allowedCharacters.Split >> List.ofArray)
        |> List.collect allPermutations
        |> List.map (fun words -> words |> Array.ofList |> normalizeWords, allowedDistance words)

    let jury answer =
        let answers = extractCorrectAnswers answer
        let mutable knownVersions : Map<string,bool> = Map.empty
        let alg = Damerau()

        fun version ->
            let version = normalize version

            match knownVersions.TryFind version with
            | Some res -> res
            | None ->
                let res = answers |> List.exists (fun (aw, dist) -> alg.Distance(aw, version) <= dist)
                knownVersions <- knownVersions.Add (version,res)
                res

    let createJury (qw:Domain.QuizQuestion) =
        if not (String.IsNullOrWhiteSpace (qw.Answer)) then
            Some (jury qw.Answer)
        else
            None


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

let finishQuestion quiz _ =
    let logic quiz =
        quiz |> Domain.Quizzes.settle

    result{
        let! quiz = CommonService.updateQuiz quiz.QuizId logic

        settleAnswers quiz

        return Admin.quizCard quiz
    }

let settleAnswers (quiz : Domain.Quiz) =
    let logic jury (team : Domain.Team) =
        team |> Domain.Teams.settleAnswer quiz.CurrentQuestionIndex jury DateTime.UtcNow |> Ok

    match quiz.CurrentQuestion with
    | Some qw ->
        match Jury.createJury qw with
        | Some jury ->
            async {
                for teamId in Data.Teams.getIds quiz.Dsc.QuizId do
                    CommonService.updateTeamNoReply {QuizId = quiz.Dsc.QuizId; TeamId = teamId} (logic jury)
            } |> Async.Start
        | None -> ()
    | _ -> ()

let nextQuestion quiz _ =
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
    let logic idx res team =
        team |> Domain.Teams.updateResult idx res DateTime.UtcNow |> Ok

    for r in req do
        CommonService.updateTeamNoReply {QuizId = quiz.QuizId; TeamId = r.TeamId} (logic r.Idx r.Res)

    Ok()

let getResults quiz _ =
    result{
        let! quiz = (Data.Quizzes.get quiz.QuizId, "Quiz not found")
        let teams = Data.Teams.getAllInQuiz quiz.Dsc.QuizId

        return {|Teams = teams |> teamResults true; Questions = questionResults quiz|}
    }

