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