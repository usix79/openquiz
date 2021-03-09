module rec Presenter

open Shared
open Domain

let quizStatus (status : QuizStatus) : Shared.QuizStatus=
    match status with
    | Setup -> Shared.QuizStatus.Setup
    | Live -> Shared.QuizStatus.Live
    | Finished -> Shared.QuizStatus.Finished

let quizStatusToDomain (status : Shared.QuizStatus) =
    match status with
    | Shared.Setup -> Setup
    | Shared.Live -> Live
    | Shared.Finished -> Finished

let tourStatus (status : QuizTourStatus) : Shared.TourStatus=
    match status with
    | Announcing -> TourStatus.Announcing
    | Countdown -> TourStatus.Countdown
    | Settled -> TourStatus.Settled

let teamStatus (status : TeamStatus) : Shared.TeamStatus=
    match status with
    | New -> Shared.TeamStatus.New
    | Admitted -> Shared.TeamStatus.Admitted
    | Rejected -> Shared.TeamStatus.Rejected

let teamStatusToDomain (status : Shared.TeamStatus) : TeamStatus=
    match status with
    | Shared.New -> TeamStatus.New
    | Shared.Admitted -> TeamStatus.Admitted
    | Shared.Rejected -> TeamStatus.Rejected

let qwKey (qwKey : QwKey) : Shared.QwKey =
    {
        TourIdx = qwKey.TourIdx
        QwIdx = qwKey.QwIdx
    }

let qwKeyToDomain (qwKey : Shared.QwKey) : QwKey =
    {
        TourIdx = qwKey.TourIdx
        QwIdx = qwKey.QwIdx
    }

let packageRecord (package:PackageDescriptor) : PackageRecord =
    {
        PackageId = package.PackageId
        Producer = package.Producer
        Name = package.Name
    }

let packageCard (package: Package) : PackageCard =
    {
        PackageId = package.Dsc.PackageId
        Name = package.Dsc.Name
        Slips = package.Slips |> List.map slip
    }

let question = function
    | Solid qw -> Shared.Solid qw
    | Split list -> Shared.Split list

let questionToDomain = function
    | Shared.Solid qw -> Solid qw
    | Shared.Split list -> Split list

let choiceAnswer (aw:ChoiceAnswer) : Shared.ChoiceAnswer =
    {Text = aw.Text; IsCorrect = aw.IsCorrect}

let choiceAnswerToDomain (aw:Shared.ChoiceAnswer) : ChoiceAnswer =
    {Text = aw.Text; IsCorrect = aw.IsCorrect}

let slipAnswer = function
    | OpenAnswer aw -> Shared.OpenAnswer aw
    | ChoiceAnswer list -> Shared.ChoiceAnswer (list |> List.map choiceAnswer)

let slipAnswerToDomain = function
    | Shared.OpenAnswer aw -> OpenAnswer aw
    | Shared.ChoiceAnswer list -> ChoiceAnswer (list |> List.map choiceAnswerToDomain)

let qwName tour qwIdx =
    match tour.Slip with
    | Single _ -> tour.Name
    | _ -> sprintf "%s.%i" tour.Name (qwIdx + 1)

let mediaType = function
    | Picture -> Shared.Picture
    | Audio -> Shared.Audio
    | Video -> Shared.Video

let mediaTypeToDomain = function
    | Shared.Picture -> Picture
    | Shared.Audio -> Audio
    | Shared.Video -> Video

let mediaDsc (mediaDsc : MediaDsc) : Shared.MediaDsc =
    {Key = mediaDsc.Key; Type = mediaType mediaDsc.Type}

let mediaDscToDomain (mediaDsc : Shared.MediaDsc) : MediaDsc =
    {Key = mediaDsc.Key; Type = mediaTypeToDomain mediaDsc.Type}

let singleSlip (slip:SingleSlip) : Shared.SingleSlip =
    {
        Caption = slip.Caption
        Question = question slip.Question
        QuestionMedia = slip.QuestionMedia |> Option.map mediaDsc
        Answer = slipAnswer slip.Answer
        AnswerMedia = slip.AnswerMedia |> Option.map mediaDsc
        Comment = slip.Comment
        Points = slip.Points
        JeopardyPoints = slip.JeopardyPoints
        WithChoice = slip.WithChoice
        EndOfTour = slip.EndOfTour
    }


let slip (domainSlip : Slip) : Shared.Slip =
    match domainSlip with
    | Single sl -> singleSlip sl |> Shared.Single
    | Multiple (name,slips) -> (name, slips |> List.map singleSlip) |> Shared.Multiple

let singleSlipToDomain (slip:Shared.SingleSlip) =
    {
        Caption = slip.Caption
        Question = questionToDomain slip.Question
        QuestionMedia = slip.QuestionMedia |> Option.map mediaDscToDomain
        Answer = slipAnswerToDomain slip.Answer
        AnswerMedia = slip.AnswerMedia |> Option.map mediaDscToDomain
        Comment = slip.Comment
        Points = slip.Points
        JeopardyPoints = slip.JeopardyPoints
        WithChoice = slip.WithChoice
        EndOfTour = slip.EndOfTour
    } : Domain.SingleSlip
let slipToDomain (slip : Shared.Slip) : Slip =
    match slip with
    | Shared.Single slip -> singleSlipToDomain slip |> Single
    | Shared.Multiple (name, slips) -> (name, slips |> List.map singleSlipToDomain) |> Multiple

let quizChangeEvent (quiz:Quiz) =
    {
        Id = quiz.Dsc.QuizId
        QS = quizStatus quiz.Dsc.Status
        Url = quiz.Dsc.StreamUrl
        T = match quiz.CurrentTour with
             | Some tour -> Some <| tourCard quiz.CurrentTourIndex tour
             | None -> None
    }

let tourCard idx (tour:QuizTour) : TourCard =
    {
        Idx = idx
        Name = tour.Name
        Sec = tour.Seconds
        TS = tourStatus tour.Status
        Slip = slipCard tour.Status tour.QwIdx tour.QwPartIdx tour.IsMediaDisplayed tour.IsQuestionDisplayed tour.Slip
        ST = tour.StartTime
    }

let qwText nextQwPartIdx = function
    | Solid qw -> qw
    | Split list -> list |> List.take (max 0 nextQwPartIdx) |> List.mapi (fun idx qw -> sprintf "%i. %s" (idx + 1) qw) |> String.concat "\n"

let extractChoices = function
    | OpenAnswer _ -> None
    | ChoiceAnswer list -> list |> List.map (fun ach -> ach.Text) |> Some

let slipSingleCard status qwPartIdx showMedia showQuestion (slip:SingleSlip) : SingleSlipCard =
    match status with
    | Announcing when qwPartIdx = 0 && not (showQuestion || showMedia) -> X3
    | Announcing when qwPartIdx = 0 && showQuestion ->
        {   Txt=slip.Question |> qwText qwPartIdx
            Choices = None
            Media = slip.QuestionMedia |> Option.map mediaDsc
            Ch = slip.WithChoice
            Points = slip.Points} |> QW
    | Announcing when qwPartIdx = 0 && showMedia ->
        {   Txt = ""
            Choices = None
            Media = slip.QuestionMedia |> Option.map mediaDsc
            Ch = slip.WithChoice
            Points = slip.Points} |> QW
    | Announcing ->
        {   Txt=slip.Question |> qwText qwPartIdx
            Choices = None
            Media = slip.QuestionMedia |> Option.map mediaDsc
            Ch = slip.WithChoice
            Points = slip.Points} |> QW
    | Countdown ->
        {   Txt=slip.Question |> qwText slip.QuestionsCount
            Choices = extractChoices slip.Answer
            Media = slip.QuestionMedia |> Option.map mediaDsc
            Ch = slip.WithChoice
            Points = slip.Points} |> QW
    | Settled -> {Aw= slipAnswer slip.Answer; Com = slip.Comment;  Media = slip.AnswerMedia |> Option.map mediaDsc; Ch = slip.WithChoice} |> AW

let slipCard status qwIdx qwPartIdx showMedia showQuestion (slip:Slip) : SlipCard =
    match slip with
    | Single slip ->
        let qwPartIdx = if status = Announcing then qwPartIdx else slip.QuestionsCount - 1
        slipSingleCard status qwPartIdx showMedia showQuestion slip |> SS
    | Multiple (name,slips) ->
        let cards =
            if status = Announcing then slips |> List.take qwIdx else slips
            |> List.map (fun s -> s |> slipSingleCard status s.QuestionsCount false false)
        // TODO: add next slip if qwPartIdx > 0
        (name,cards) |> MS

let history (team : Team) =
    team.Answers
    |> Map.toList
    |> List.choose (fun (key,aw) -> aw.Result |> Option.bind (fun res -> Some ((qwKey key),res)))
    |> Map.ofList

module Main =

    let quizRegRecord (quiz:QuizDescriptor) (team: TeamDescriptor option) : MainModels.QuizRegRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Name = quiz.Name
            Status = quizStatus quiz.Status
            Description = quiz.RegText
            Information =
                match team with
                | Some team when team.Status = Admitted -> quiz.InfoText
                | _ -> ""
            ImgKey = quiz.ImgKey
            EventPage = quiz.EventPage
            ListenToken = quiz.ListenToken
            Comp = team |> Option.map expertCompetition
        }

    let quizProdRecord (quiz:QuizDescriptor) : MainModels.QuizProdRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Name = quiz.Name
            Status = quizStatus quiz.Status
            AdminToken = quiz.AdminToken
        }

    let quizProdCard (quiz:Quiz) : MainModels.QuizProdCard =
        {
            QuizId = quiz.Dsc.QuizId
            StartTime = quiz.Dsc.StartTime
            Name = quiz.Dsc.Name
            ImgKey = quiz.Dsc.ImgKey
            ListenToken = quiz.Dsc.ListenToken
            AdminToken = quiz.Dsc.AdminToken
            RegToken = quiz.Dsc.RegToken
            WelcomeText = quiz.Dsc.WelcomeText
            FarewellText = quiz.Dsc.FarewellText
            RegText = quiz.Dsc.RegText
            InfoText = quiz.Dsc.InfoText
            WithPremoderation = quiz.Dsc.WithPremoderation
            EventPage = quiz.Dsc.EventPage
            MixlrCode = quiz.Dsc.MixlrCode
        }

    let expertCompetition (team:TeamDescriptor) : MainModels.ExpertCompetition=
        {QuizId = team.QuizId; TeamId = team.TeamId; TeamName = team.Name; TeamStatus = teamStatus team.Status; EntryToken = team.EntryToken}

    let settingsCard (exp:Expert) : MainModels.SettingsCard =
        {UserId = exp.Id; DefaultImg = exp.DefaultImg; DefaultMixlr = exp.DefaultMixlr}

    let packageCard expertId (expertsProvider:Provider<string,Expert>) (package: Package)  : MainModels.PackageCard =
        {
            PackageId = package.Dsc.PackageId
            Producer = package.Dsc.Producer
            Name = package.Dsc.Name
            TransferToken = if package.Dsc.Producer = expertId then package.TransferToken else ""
            SharedWith = package.SharedWith |> List.choose expertsProvider |> List.map expertRecord
            Slips = package.Slips |> List.map slip
        }

    let expertRecord (exp:Expert) : MainModels.ExpertRecord =
        {Id = exp.Id; Name = exp.Name}

module Admin =

    let teamRecord (team:TeamDescriptor) : AdminModels.TeamRecord =
        {
            TeamId = team.TeamId
            TeamName = team.Name
            TeamStatus = teamStatus team.Status
            EntryToken = team.EntryToken
        }

    let teamCard (team:TeamDescriptor) : AdminModels.TeamCard =
        {
            TeamId = team.TeamId
            TeamName = team.Name
            TeamStatus = teamStatus team.Status
            EntryToken = team.EntryToken
            RegistrationDate = team.RegistrationDate
        }

    let quizCard (quiz: Quiz) : AdminModels.QuizControlCard =
        {
            QuizStatus = quizStatus quiz.Dsc.Status
            PackageId = quiz.Dsc.PkgId
            PackageSlipIdx = quiz.Dsc.PkgSlipIdx
            StreamUrl = quiz.Dsc.StreamUrl
            CurrentTour =
                match quiz.Dsc.Status with
                | Live -> match quiz.CurrentTour with Some tour -> Some <| quizTour tour | None -> None
                | _ -> None
        }

    let quizTour (tour : QuizTour) : AdminModels.TourControlCard =
        {
            Name = tour.Name
            Seconds = tour.Seconds
            Status = tourStatus tour.Status
            StartTime = tour.StartTime
            Slip = slip tour.Slip
            QwIdx = tour.QwIdx
            QwPartIdx = tour.QwPartIdx
            IsMediaDisplayed = tour.IsMediaDisplayed
            IsQuestionDisplayed = tour.IsQuestionDisplayed
        }

    let teamAnswersRecord (team:Team) : AdminModels.TeamAnswersRecord =
        {
            Id = team.Dsc.TeamId
            Nm = team.Dsc.Name
            Awrs =
                team.Answers
                |> Map.toList
                |> List.map (fun (key,aw) ->
                    let key = qwKey key
                    let v =
                        {RT = aw.RecieveTime
                         Txt = aw.Text
                         Jpd = aw.Jeopardy
                         Res = aw.Result
                         IsA = aw.IsAutoResult
                         UT = aw.UpdateTime} : AdminModels.Answer
                    (key,v)
                )|> Map.ofList
        }

    let qwRecord tourIdx tour qwIdx (slip:SingleSlip) : AdminModels.QuestionRecord =
        {
            Key = {TourIdx = tourIdx; QwIdx = qwIdx}
            Nm = qwName tour qwIdx
            Sec = tour.Seconds
            TS = tourStatus tour.Status
            ST = tour.StartTime
            Pt = slip.Points
            JpdPt = slip.JeopardyPoints
            Ch = slip.WithChoice
            Ann = slip.QuestionText 0 |> Common.trimEnd 64 "..."
            Awr = slip.Answer.ToRawString()
        }

    let questionRecords  (quiz:Quiz) : AdminModels.QuestionRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun tourIdx tour ->
            match tour.Slip with
            | Single slip -> [qwRecord tourIdx tour 0 slip]
            | Multiple (_,slips) ->
                slips |> List.mapi (fun idx slip -> qwRecord tourIdx tour idx slip)
        )|> List.concat

    let AnswersBundle (quiz:Quiz) (teams:Team list): AdminModels.AnswersBundle =
        {
            Questions = questionRecords quiz
            Teams  = teams |> List.map teamAnswersRecord
        }

module Teams =

    let quizCard (quiz:Quiz) (team:Team): TeamModels.QuizCard =
        {
            QS = quizStatus quiz.Dsc.Status
            TS = teamStatus team.Dsc.Status
            Img = quiz.Dsc.ImgKey
            Wcm = quiz.Dsc.WelcomeText
            Fwl = quiz.Dsc.FarewellText
            TC =
                match quiz.CurrentTour with
                | Some tour when quiz.Dsc.Status = Live -> Some <| tourCard quiz.CurrentTourIndex tour
                | _ -> None
            Aw =
                match quiz.Dsc.Status with
                | Live -> team.SelectAnswers quiz.CurrentTourIndex |> List.map (fun (key,aw) -> (key.QwIdx,(aw.Text,aw.Jeopardy))) |> Map.ofList
                | _ -> Map.empty

            LT = quiz.Dsc.ListenToken
            Mxlr = quiz.Dsc.MixlrCode
            Url = quiz.Dsc.StreamUrl
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) (team:Team): TeamModels.TeamHistoryRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun idx tour ->
            tour.Slip.Answers
            |> List.mapi (fun awIdx aw ->
                let key = {TourIdx = idx; QwIdx = awIdx}
                let r =
                    {
                        QwKey = qwKey key
                        QwName = qwName tour awIdx
                        QwAw =
                            if idx = quiz.CurrentTourIndex then
                                match tour.Status with
                                | Announcing | Countdown -> ""
                                | Settled -> aw.ToRawString()
                            else
                                aw.ToRawString()
                        AwTxt = None
                        AwJpd = false
                        Result = None
                        Vote = None
                    } :  TeamModels.TeamHistoryRecord

                match team.GetAnswer key with
                | Some aw -> {r with AwTxt = Some aw.Text; AwJpd = aw.Jeopardy; Result = aw.Result; Vote = aw.Vote}
                | None -> r
            )
        ) |> List.concat

module Reg =
    let quizRecord (quiz:QuizDescriptor) : RegModels.QuizRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Name = quiz.Name
            Status = quizStatus quiz.Status
            Description = quiz.RegText
            ImgKey = quiz.ImgKey
            EventPage = quiz.EventPage
        }


module Audience =

    let quizCard (quiz:Quiz) : AudModels.QuizCard =
        {
            QS = quizStatus quiz.Dsc.Status
            QN = quiz.Dsc.Name
            Img = quiz.Dsc.ImgKey
            Wcm = quiz.Dsc.WelcomeText
            Fwl = quiz.Dsc.FarewellText
            TC =
                match quiz.CurrentTour with
                | Some qw when quiz.Dsc.Status = Live -> Some <| tourCard quiz.CurrentTourIndex qw
                | _ -> None
            LT = quiz.Dsc.ListenToken
            Mxlr = quiz.Dsc.MixlrCode
            Url = quiz.Dsc.StreamUrl
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) : AudModels.HistoryRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun idx tour ->
            tour.Slip.Answers
            |> List.mapi (fun awIdx aw ->
                {
                    QwKey = {TourIdx = idx; QwIdx = awIdx}
                    QwName = qwName tour awIdx
                    QwAw =
                        if idx = quiz.CurrentTourIndex then
                            match tour.Status with
                            | Announcing | Countdown -> ""
                            | Settled -> aw.ToRawString()
                        else
                            aw.ToRawString()
                } : AudModels.HistoryRecord
            ) |> List.rev
        ) |> List.concat