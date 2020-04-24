module rec MainService

open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let api (context:HttpContext) : IMainApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =
        let ff f = (fun expertId username (quizIdStr:string) req ->
            let quizId = match System.Int32.TryParse quizIdStr with true, id -> Some id | _ -> None
            f expertId username quizId req
        )

        SecurityService.execute logger proc <| SecurityService.authorizeExpertCheckPrivateQuiz secret (ff f)

    let exPublisher proc f =

        let ff f = (fun expertId username req ->
            match Data.Experts.get expertId with
            | Some expert ->
                f expert req
            | None ->
                Log.Error ("{Op} {Error} {ExpertId}", "main", "Wrong Publisher Id", expertId)
                Error "Wrong Publisher Id"
        )

        SecurityService.execute logger proc <| SecurityService.authorizeExpert secret (ff f)

    let api : IMainApi = {
        becomeProducer = ex  "becomeProducer" becomeProducer
        getPubModel = ex "getPubModel" getPubModel
        registerTeam = ex "registerTeam" registerTeam
        createQuiz = exPublisher  "createQuiz" createQuiz
        getProdQuizzes = exPublisher "getProdQuizzes" getProdQuizzes
        getProdQuizCard = exPublisher "getProdQuizCard" getProdQuizCard
        updateProdQuizCard = exPublisher "updateProdQuizCard" updateProdQuizCard
        uploadFile = exPublisher "uploadFile" <| uploadFile (Config.getFilesAccessPoint cfg)
        getProdPackages = exPublisher "getProdPackages" getProdPackages
        getProdPackageCard = exPublisher  "getProdPackageCard" getProdPackageCard
        createPackage = exPublisher  "createPackage" createPackage
        updateProdPackageCard = exPublisher  "updateProdPackageCard" updateProdPackageCard
        aquirePackage = exPublisher "aquirePackage" aquirePackage

    }

    api

let becomeProducer expertId username _ _ =
    let creator _ = Domain.Experts.createNew expertId username |> Ok
    let logic expert = expert |> Domain.Experts.becomeProducer |> Ok

    CommonService.updateOrCreateExpert expertId creator logic |> ignore

    Ok ()

let createQuiz expert _ =
    let creator quizId =
        Ok <| Domain.Quizzes.createNew quizId expert.Id


    result{
        let! quiz = CommonService.createQuiz creator

        let logic expert = expert |> Domain.Experts.addQuiz quiz.Dsc.QuizId |> Ok
        CommonService.updateExpert expert.Id logic |> ignore

        return {|Record = quiz.Dsc |> Main.quizProdRecord; Card = Main.quizProdCard quiz; |}
    }

let getProdQuizzes expert _ =
    expert.Quizes
    |> List.map Data.Quizzes.getDescriptor
    |> List.filter (fun q -> q.IsSome)
    |> List.map (fun q -> Main.quizProdRecord q.Value)
    |> Ok

let getProdQuizCard expert (req : {|QuizId : int|}) =
    match Data.Quizzes.get req.QuizId with
    | Some quiz when quiz.Dsc.Producer <> expert.Id -> Error "Quiz is produced by someone else"
    | None -> Error "Quiz not found"
    | Some quiz -> Ok <| Main.quizProdCard quiz

let updateProdQuizCard expert card =
    let logic (quiz : Domain.Quiz) =
        match quiz with
        | _ when quiz.Dsc.Producer <> expert.Id -> Error "Quiz is produced by someone else"
        | _ ->
            let dsc = { quiz.Dsc with Brand = card.Brand; Name = card.Name; StartTime = card.StartTime; Status = quizStatusToDomain card.Status;
                                    ImgKey = card.ImgKey; WelcomeText = card.WelcomeText; FarewellText = card.FarewellText; IsPrivate = card.IsPrivate;
                                    WithPremoderation = card.WithPremoderation; EventPage = card.EventPage; MixlrCode = card.MixlrCode}

            Ok { quiz with Dsc = dsc}

    result{
        let! quiz = CommonService.updateQuiz card.QuizId logic

        return quiz.Dsc |> Main.quizProdRecord
    }


let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody

let getPubModel expId username quizId _ =
    let comps =
        match Data.Experts.get expId with
        | Some exp -> exp.Competitions
        | None -> Map.empty

    let quizzes =
        match quizId with
        | Some id ->
            match Data.Quizzes.getDescriptor id with
            | Some quiz -> [quiz]
            | None -> []
        | None ->
            Data.Quizzes.getDescriptors()
            |> List.filter Domain.Quizzes.isPubQuiz
        |> List.map Presenter.Main.quizPubRecord

    let competitions =
        quizzes
        |> List.map (fun q -> (comps.TryGetValue q.QuizId), q.QuizId)
        |> List.filter (fun ((found, _), _) -> found)
        |> List.map (fun ((_,teamId),quizId) -> quizId, Data.Teams.getDescriptor quizId teamId)
        |> List.filter (fun (_,dsc) -> dsc.IsSome)
        |> List.map (fun (quizId,dsc) -> quizId,Presenter.Main.expertCompetition dsc.Value)
        |> Map.ofList

    Ok {|Profile = {Competitions = competitions}; Quizzes = quizzes|}

let registerTeam expId username quizId req =
    let expCreator _ = Domain.Experts.createNew expId username |> Ok

    result {

        let! exp = CommonService.updateOrCreateExpert expId expCreator Ok

        let! quiz = ((Data.Quizzes.getDescriptor req.QuizId), "Quiz not found")

        do! match quizId with
            | Some id when id <> quiz.QuizId -> Error "Wrong quiz id"
            | None when quiz.IsPrivate -> Error "Public registration is now allowed"
            | _ -> Ok ()

        let teamName = req.TeamName.Trim()
        let! team =
            match Domain.Experts.getComp req.QuizId exp with
            | Some teamId ->
                match Data.Teams.get req.QuizId teamId with
                | Some team -> updateTeam quiz team teamName
                | None -> createTeam exp quiz teamName
            | None -> createTeam exp quiz teamName

        return Presenter.Main.expertCompetition team.Dsc
    }

let private createTeam exp quiz teamName : Result<Domain.Team,string> =

    let creator (key : Domain.TeamKey) =
        let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew key.TeamId teamName quiz |> Ok

    result {
        let! team = CommonService.createTeam quiz.QuizId creator

        let logic expert = expert |> Domain.Experts.addComp team.Dsc.QuizId team.Dsc.TeamId |> Ok
        CommonService.updateExpert exp.Id logic |> ignore

        return team
    }

let private updateTeam quiz team teamName : Result<Domain.Team,string> =
    match team.Dsc.Name = teamName with
    | true -> Ok team
    | false ->
        let logic (team : Domain.Team) =
            result {
                let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId

                do! match Domain.Teams.validateTeamUpdate false teamName teamsInQuiz quiz with Some txt -> Error txt | _ -> Ok()

                return team |> Domain.Teams.changeName teamName
            }

        CommonService.updateTeam team.Key logic

let getProdPackages expert _ =
    expert.Packages
    |> List.map Data.Packages.getDescriptor
    |> List.filter (fun p -> p.IsSome)
    |> List.map (fun p -> packageRecord p.Value)
    |> Ok

let getProdPackageCard expert (req : {|PackageId : int|}) =
    match Data.Packages.get req.PackageId with
    | Some package when package.Dsc.Producer <> expert.Id -> Error "Package belongs to another producer"
    | None -> Error "Package not found"
    | Some package -> Ok <| packageCard package

let createPackage expert _ =
    let creator = fun id ->
        Domain.Packages.createNew id expert.Id |> Ok

    result {
        let! package = CommonService.createPackage creator

        CommonService.updateExpertNoReply expert.Id (Domain.Experts.addPackage package.Dsc.PackageId)

        return {|Record = package.Dsc |> packageRecord; Card = packageCard package; |}
    }

let updateProdPackageCard expert card =
    let logic (pkg:Domain.Package) =
        { pkg with
            Dsc = { pkg.Dsc with Name = card.Name}
            TransferToken = card.TransferToken
            Questions = card.Questions |> List.map packageQwToDomain
        } |> Ok

    result {
        let! _ = (expert.Packages |> List.tryFind ((=) card.PackageId), "Package belongs to another producer")
        let! package = CommonService.updatePackage card.PackageId logic
        return package.Dsc |> packageRecord
    }

let aquirePackage expert res =
    result{
        let! origPackageDsc = (Data.Packages.getDescriptor res.PackageId, "Invalid Transfer Token")
        let! updatedPackage = CommonService.updatePackage res.PackageId (Domain.Packages.transfer expert.Id res.TransferToken)

        CommonService.updateExpertNoReply origPackageDsc.Producer (Domain.Experts.removePackage origPackageDsc.PackageId)
        CommonService.updateExpertNoReply expert.Id (Domain.Experts.addPackage updatedPackage.Dsc.PackageId)

        return updatedPackage.Dsc |> packageRecord
    }