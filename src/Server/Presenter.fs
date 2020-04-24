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

let quizQuestionStatus (status : QuizQuestionStatus) : Shared.QuizQuestionStatus=
    match status with
    | Announcing -> Shared.QuizQuestionStatus.Announcing
    | Countdown -> Shared.QuizQuestionStatus.Countdown
    | Settled -> Shared.QuizQuestionStatus.Settled

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
        Questions =
            package.Questions
            |> List.map packageQw
    }

let packageQw (packageQw : PackageQuestion) : Shared.PackageQuestion =
    {
        Text = packageQw.Text
        ImgKey = packageQw.ImgKey
        Answer = packageQw.Answer
        Comment = packageQw.Comment
        CommentImgKey = packageQw.CommentImgKey
    }

let packageQwToDomain (packageProdQw : Shared.PackageQuestion) : PackageQuestion =
    {
        Text = packageProdQw.Text
        ImgKey = packageProdQw.ImgKey
        Answer = packageProdQw.Answer
        Comment = packageProdQw.Comment
        CommentImgKey = packageProdQw.CommentImgKey
    }

let quizChangeEvent (quiz:Quiz) =
    {
        Id = quiz.Dsc.QuizId
        QS = Presenter.quizStatus quiz.Dsc.Status
        Qw = match quiz.CurrentQuestion with
             | Some qw -> Some <| questionCard quiz.CurrentQuestionIndex qw
             | None -> None
    }

let questionCard idx (qw:QuizQuestion) : QuestionCard =
    {
        Idx = idx
        Cap = qw.Name
        Sec = qw.Seconds
        QQS = quizQuestionStatus qw.Status
        Txt =
            match qw.Status with
            | Announcing -> ""
            | Countdown -> qw.Text
            | Settled -> qw.Answer
        Img =
            match qw.Status with
            | Announcing -> ""
            | Countdown -> qw.ImgKey
            | Settled -> qw.CommentImgKey
        Com =
            match qw.Status with
            | Settled -> qw.Comment
            | _ -> ""

        ST = qw.StartTime
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
    quiz.Questions
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
            PackageQwIdx = quiz.Dsc.PkgQwIdx
            CurrentQw =
                match quiz.Dsc.Status with
                | Live -> match quiz.CurrentQuestion with Some qw -> Some <| quizQuestion qw | None -> None
                | _ -> None
        }

    let quizQuestion (qw : QuizQuestion) : AdminModels.QuizQuestion =
        {
            Name = qw.Name
            Seconds = qw.Seconds
            Status = quizQuestionStatus qw.Status
            Text = qw.Text
            ImgKey = qw.ImgKey
            Answer = qw.Answer
            Comment = qw.Comment
            CommentImgKey = qw.CommentImgKey
            StartTime = qw.StartTime
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
        quiz.Questions
        |> List.rev
        |> List.mapi (fun idx qw ->
                        {
                            Idx = idx + 1
                            Nm = qw.Name
                            Sec = qw.Seconds
                            QQS = quizQuestionStatus qw.Status
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
            Qw =
                match quiz.CurrentQuestion with
                | Some qw when quiz.Dsc.Status = Live -> Some <| questionCard quiz.CurrentQuestionIndex qw
                | _ -> None
            Aw =
                match team.GetAnswer quiz.CurrentQuestionIndex with
                | Some aw when quiz.Dsc.Status = Live -> Some aw.Text
                | _ -> None
            LT = quiz.Dsc.ListenToken
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) (team:Team): TeamModels.TeamHistoryRecord list =
        quiz.Questions
        |> List.rev
        |> List.mapi (fun idx qw ->
            let idx = idx + 1
            let r =
                {
                    QwIdx = idx
                    QwName = qw.Name
                    QwAw =
                        if idx = quiz.CurrentQuestionIndex then
                            match qw.Status with
                            | Announcing | Countdown -> ""
                            | Settled -> qw.Answer
                        else
                            qw.Answer
                    AwTxt = None
                    Result = None
                } :  TeamModels.TeamHistoryRecord

            match team.GetAnswer idx with
            | Some aw -> {r with AwTxt = Some aw.Text; Result = aw.Result}
            | None -> r
        )

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
            Qw =
                match quiz.CurrentQuestion with
                | Some qw when quiz.Dsc.Status = Live -> Some <| questionCard quiz.CurrentQuestionIndex qw
                | _ -> None
            LT = quiz.Dsc.ListenToken
            V = quiz.Version
        }

    let quizHistory (quiz:Quiz) : AudModels.HistoryRecord list =
        quiz.Questions
        |> List.rev
        |> List.mapi (fun idx qw ->
            let idx = idx + 1
            {
                QwIdx = idx
                QwName = qw.Name
                QwAw =
                    if idx = quiz.CurrentQuestionIndex then
                        match qw.Status with
                        | Announcing | Countdown -> ""
                        | Settled -> qw.Answer
                    else
                        qw.Answer
            }
        )
