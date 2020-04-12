module rec Presenter

open Shared
open Domain

let quizStatus (status : QuizStatus) =
    match status with
    | Draft -> SharedModels.QuizStatus.Draft
    | Published -> SharedModels.QuizStatus.Published
    | Live -> SharedModels.QuizStatus.Live
    | QuizStatus.Finished -> SharedModels.QuizStatus.Finished
    | Archived -> SharedModels.QuizStatus.Archived

let quizStatusToDmain (status : SharedModels.QuizStatus) =
    match status with
    | SharedModels.QuizStatus.Draft -> Draft
    | SharedModels.QuizStatus.Published -> Published
    | SharedModels.QuizStatus.Live -> Live
    | SharedModels.QuizStatus.Finished -> QuizStatus.Finished
    | SharedModels.QuizStatus.Archived -> Archived

let teamStatus (status : TeamStatus) =
    match status with
    | New -> SharedModels.TeamStatus.New
    | Admitted -> SharedModels.TeamStatus.Admitted
    | Rejected -> SharedModels.TeamStatus.Rejected

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
        }

    let quizProdCard (quiz:Quiz) : MainModels.QuizProdCard =
        {
            QuizId = quiz.Dsc.QuizId
            StartTime = quiz.Dsc.StartTime
            Brand = quiz.Dsc.Brand
            Name = quiz.Dsc.Name
            Status = quizStatus quiz.Dsc.Status
            ImgKey = quiz.Dsc.ImgKey
            ListenToken = quiz.ListenToken
            AdminToken = quiz.AdminToken
            RegToken = quiz.RegToken
            WelcomeText = quiz.Dsc.WelcomeText
            FarewellText = quiz.Dsc.FarewellText
            IsPrivate = quiz.Dsc.IsPrivate
            WithPremoderation = quiz.Dsc.WithPremoderation
        }

    let expertCompetition (team:TeamDescriptor) : MainModels.ExpertCompetition=
        {QuizId = team.QuizId; TeamId = team.TeamId; TeamName = team.Name; TeamStatus = teamStatus team.Status; EntryToken = team.EntryToken}

    let packageProdRecord (package:PackageDescriptor) : MainModels.PackageProdRecord =
        {
            PackageId = package.PackageId
            Name = package.Name
        }

    let packageProdCard (package: Package) : MainModels.PackageProdCard =
        {
            PackageId = package.Dsc.PackageId
            Name = package.Dsc.Name
            Questions =
                package.Questions
                |> List.map packageProdQw
        }

    let packageProdQw (packageQw : PackageQuestion) : MainModels.PackageProdQuestion =
        {
            Text = packageQw.Text
            ImgKey = packageQw.ImgKey
            Answer = packageQw.Answer
            Comment = packageQw.Comment
            CommentImgKey = packageQw.CommentImgKey
        }

    let packageQw (packageProdQw : MainModels.PackageProdQuestion) : PackageQuestion =
        {
            Text = packageProdQw.Text
            ImgKey = packageProdQw.ImgKey
            Answer = packageProdQw.Answer
            Comment = packageProdQw.Comment
            CommentImgKey = packageProdQw.CommentImgKey
        }