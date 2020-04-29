module rec Presenter

open Shared
open Domain

let quizStatus (status : QuizStatus) : Shared.QuizStatus=
    match status with
    | Draft -> Shared.QuizStatus.Draft
    | Published -> Shared.QuizStatus.Published
    | Live -> Shared.QuizStatus.Live
    | QuizStatus.Finished -> Shared.QuizStatus.Finished
    | Archived -> Shared.QuizStatus.Archived

let quizStatusToDomain (status : Shared.QuizStatus) =
    match status with
    | Shared.Draft -> Draft
    | Shared.Published -> Published
    | Shared.Live -> Live
    | Shared.Finished -> Finished
    | Shared.Archived -> Archived

let tourStatus (status : QuizTourStatus) : Shared.TourStatus=
    match status with
    | Announcing -> Shared.TourStatus.Announcing
    | Countdown -> Shared.TourStatus.Countdown
    | Settled -> Shared.TourStatus.Settled

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

let packageRecord (package:PackageDescriptor) : PackageRecord =
    {
        PackageId = package.PackageId
        Name = package.Name
    }

let packageCard (package: Package) : PackageCard =
    {
        PackageId = package.Dsc.PackageId
        Name = package.Dsc.Name
        TransferToken = package.TransferToken
        Slips =
            package.Slips
            |> List.map slip
    }

let slip (slip : Slip) : Shared.Slip =
    match slip with
    | Single sl ->
        Shared.Single {
            Questions = sl.Questions
            ImgKey = sl.ImgKey
            Answer = sl.Answer
            Comment = sl.Comment
            CommentImgKey = sl.CommentImgKey
        }

let slipToDomain (slip : Shared.Slip) : Slip =
    match slip with
    | Shared.Single s ->
        Single {
            Questions = s.Questions
            ImgKey = s.ImgKey
            Answer = s.Answer
            Comment = s.Comment
            CommentImgKey = s.CommentImgKey
        }

let quizChangeEvent (quiz:Quiz) =
    {
        Id = quiz.Dsc.QuizId
        QS = quizStatus quiz.Dsc.Status
        T = match quiz.CurrentTour with
             | Some qw -> Some <| tourCard quiz.CurrentTourIndex qw
             | None -> None
    }

let tourCard idx (tour:QuizTour) : TourCard =
    {
        Idx = idx
        Cap = tour.Name
        Sec = tour.Seconds
        TS = tourStatus tour.Status
        Slip = slipCard tour.Status tour.NextQwIdx tour.Slip
        ST = tour.StartTime
    }

let qwText totalCount qwList =
    if totalCount = 1 then qwList |> String.concat "\n"
    else  qwList |> List.mapi (fun idx qw -> sprintf "%i. %s" (idx + 1) qw) |> String.concat "\n"

let slipCard status nextQwIdx (slip:Slip) : SlipCard =
    match slip with
    | Single s ->
        Shared.SingleSlipCard {
            Txt =
                match status with
                | Announcing -> s.Questions |> List.take nextQwIdx |> qwText s.Questions.Length
                | Countdown -> s.Questions |> qwText s.Questions.Length
                | Settled -> s.Answer
            Img =
                match status with
                | Announcing -> ""
                | Countdown -> s.ImgKey
                | Settled -> s.CommentImgKey
            Com =
                match status with
                | Settled -> s.Comment
                | _ -> ""

        }

let teamResults withHistory (teams: Team list) : TeamResult list =
    let mutable currentPlace = 1
    [for (points,teams) in
        teams
        |> List.filter (fun t -> t.Dsc.Status = Admitted)
        |> List.groupBy (fun t -> t.Points)
        |> List.sortByDescending (fun (points, _) -> points) do
            let len = teams.Length

            for team in teams do
                {TeamId = team.Dsc.TeamId; TeamName = team.Dsc.Name; Points = points;
                    PlaceFrom = currentPlace; PlaceTo = currentPlace + len - 1;
                        History = if withHistory then history team else Map.empty}

            currentPlace <- currentPlace + len
    ]

let questionResults (quiz:Quiz) : QuestionResult list =
    quiz.Tours
    |> List.rev
    |> List.mapi (fun idx qw -> {Idx = idx + 1; Name = qw.Name})

let history (team : Team) =
    team.Answers
    |> Map.filter (fun _  aw -> aw.Result.IsSome)
    |> Map.map (fun idx  aw -> aw.Result.Value)

module Main =

    let quizPubRecord (quiz:QuizDescriptor) : MainModels.QuizPubRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Brand = quiz.Brand
            Name = quiz.Name
            Status = quizStatus quiz.Status
            Description =
                match quiz.Status with
                | Draft | Published | Live -> quiz.WelcomeText
                | Finished | Archived -> quiz.FarewellText
            ImgKey = quiz.ImgKey
            EventPage = quiz.EventPage
        }

    let quizProdRecord (quiz:QuizDescriptor) : MainModels.QuizProdRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Brand = quiz.Brand
            Name = quiz.Name
            Status = quizStatus quiz.Status
            AdminToken = quiz.AdminToken
        }

    let quizProdCard (quiz:Quiz) : MainModels.QuizProdCard =
        {
            QuizId = quiz.Dsc.QuizId
            StartTime = quiz.Dsc.StartTime
            Brand = quiz.Dsc.Brand
            Name = quiz.Dsc.Name
            Status = quizStatus quiz.Dsc.Status
            ImgKey = quiz.Dsc.ImgKey
            ListenToken = quiz.Dsc.ListenToken
            AdminToken = quiz.Dsc.AdminToken
            RegToken = quiz.Dsc.RegToken
            WelcomeText = quiz.Dsc.WelcomeText
            FarewellText = quiz.Dsc.FarewellText
            IsPrivate = quiz.Dsc.IsPrivate
            WithPremoderation = quiz.Dsc.WithPremoderation
            EventPage = quiz.Dsc.EventPage
            MixlrCode = quiz.Dsc.MixlrCode
        }

    let expertCompetition (team:TeamDescriptor) : MainModels.ExpertCompetition=
        {QuizId = team.QuizId; TeamId = team.TeamId; TeamName = team.Name; TeamStatus = teamStatus team.Status; EntryToken = team.EntryToken}


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
            NextQwIdx = tour.NextQwIdx
        }

    let teamAnswersRecord (team:Team) : AdminModels.TeamAnswersRecord =
        {
            Id = team.Dsc.TeamId
            Nm = team.Dsc.Name
            Awrs =
                team.Answers
                |> Map.map (fun idx aw ->
                                {
                                    Txt = aw.Text
                                    RT = aw.RecieveTime
                                    Res = aw.Result
                                    IsA = aw.IsAutoResult
                                    UT = aw.UpdateTime
                                }
                            )
        }

    let questionRecords  (quiz:Quiz) : AdminModels.QuestionRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun idx qw ->
                        {
                            Idx = idx + 1
                            Nm = qw.Name
                            Sec = qw.Seconds
                            QQS = tourStatus qw.Status
                            ST = qw.StartTime
                        }
        )


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
                | Some qw when quiz.Dsc.Status = Live -> Some <| tourCard quiz.CurrentTourIndex qw
                | _ -> None
            Aw =
                match team.GetAnswer quiz.CurrentTourIndex with
                | Some aw when quiz.Dsc.Status = Live -> Some aw.Text
                | _ -> None
            LT = quiz.Dsc.ListenToken
            Mxlr = quiz.Dsc.MixlrCode
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) (team:Team): TeamModels.TeamHistoryRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun idx tour ->
            let idx = idx + 1
            tour.Slip.Answers
            |> List.mapi (fun awIdx aw ->
                let awIdx = idx + awIdx
                let r =
                    {
                        QwIdx = idx
                        QwName = tour.Name
                        QwAw =
                            if idx = quiz.CurrentTourIndex then
                                match tour.Status with
                                | Announcing | Countdown -> ""
                                | Settled -> aw
                            else
                                aw
                        AwTxt = None
                        Result = None
                    } :  TeamModels.TeamHistoryRecord

                match team.GetAnswer awIdx with
                | Some aw -> {r with AwTxt = Some aw.Text; Result = aw.Result}
                | None -> r
            )
        ) |> List.concat


module Reg =
    let quizRecord (quiz:QuizDescriptor) : RegModels.QuizRecord =
        {
            QuizId = quiz.QuizId
            StartTime = quiz.StartTime
            Brand = quiz.Brand
            Name = quiz.Name
            Status = quizStatus quiz.Status
            Description =
                match quiz.Status with
                | Draft | Published | Live -> quiz.WelcomeText
                | Finished | Archived -> quiz.FarewellText
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
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) : AudModels.HistoryRecord list =
        quiz.Tours
        |> List.rev
        |> List.mapi (fun idx tour ->
            let idx = idx + 1
            tour.Slip.Answers
            |> List.mapi (fun awIdx aw ->
                {
                    QwIdx = idx + awIdx
                    QwName = tour.Name
                    QwAw =
                        if idx = quiz.CurrentTourIndex then
                            match tour.Status with
                            | Announcing | Countdown -> ""
                            | Settled -> aw
                        else
                            aw
                } : AudModels.HistoryRecord
            )
        ) |> List.concat