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
        let ff f = (fun expertId usersysname username (quizIdStr:string) req ->
            let quizId = match System.Int32.TryParse quizIdStr with true, id -> Some id | _ -> None
            f expertId usersysname username quizId req
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
        becomeProducer = ex "becomeProducer" becomeProducer
        getRegModel = ex "getRegModel" getRegModel
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
        deleteQuiz = exPublisher "deleteQuiz" deleteQuiz
        deletePackage = exPublisher "deletePackage" deletePackage
        getSettings = exPublisher "getSettings" getSettings
        updateSettings = exPublisher "updateSettings" updateSettings
        sharePackage = exPublisher "sharePackage" sharePackage
        removePackageShare = exPublisher "removePackageShare" removePackageShare
    }

    api

let becomeProducer expertId usersysname name _ _ =
    let creator _ = Domain.Experts.createNew expertId usersysname name |> Ok
    let logic expert = expert |> Domain.Experts.becomeProducer |> Ok

    CommonService.updateOrCreateExpert expertId creator logic |> ignore

    Ok ()

let createQuiz expert _ =
    let creator quizId =
        Ok <| Domain.Quizzes.createNew quizId expert.Id expert.DefaultImg expert.DefaultMixlr

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
            let dsc = { quiz.Dsc with Name = card.Name; StartTime = card.StartTime;
                                    ImgKey = card.ImgKey; WelcomeText = card.WelcomeText; FarewellText = card.FarewellText;
                                    WithPremoderation = card.WithPremoderation; EventPage = card.EventPage; MixlrCode = card.MixlrCode}

            Ok { quiz with Dsc = dsc}

    result{
        let! quiz = CommonService.updateQuiz card.QuizId logic

        return quiz.Dsc |> Main.quizProdRecord
    }

let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody

let getRegModel expId username name quizId _ =
    result{
        let! quizId = quizId, "Quiz not defined"
        let! quiz = (Data.Quizzes.getDescriptor quizId), "Quiz not found"
        let team =
            Data.Experts.get expId
            |> Option.bind (fun exp -> exp.Competitions.TryFind quizId)
            |> Option.bind (Data.Teams.getDescriptor quizId)

        return Main.quizRegRecord quiz team
    }

let registerTeam expId username name quizId req =
    let expCreator _ = Domain.Experts.createNew expId username name |> Ok

    result {
        let! quizId = quizId, "Quiz not defined"
        let! quiz = (Data.Quizzes.getDescriptor quizId), "Quiz not found"

        let! exp = CommonService.updateOrCreateExpert expId expCreator Ok

        let teamName = req.TeamName.Trim()
        let! team =
            match Domain.Experts.getComp quizId exp with
            | Some teamId ->
                match Data.Teams.get quizId teamId with
                | Some team -> updateTeam quiz team teamName
                | None -> createTeam exp quiz teamName
            | None -> createTeam exp quiz teamName

        return Main.quizRegRecord quiz (Some team.Dsc)
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
    expert.AllPackages
    |> List.map Data.Packages.getDescriptor
    |> List.filter (fun p -> p.IsSome)
    |> List.map (fun p -> packageRecord p.Value)
    |> Ok

let getProdPackageCard expert (req : {|PackageId : int|}) =
    if expert |> Domain.Experts.isAuthorizedForPackage req.PackageId then
        match Data.Packages.get req.PackageId with
        | Some package -> Main.packageCard expert.Id package CommonService.expertLoader |> Ok
        | None -> Error "Package not found"
    else  Error "You are not authorized to load the package"

let createPackage expert _ =
    let creator = fun id ->
        Domain.Packages.createNew id expert.Id |> Ok

    result {
        let! package = CommonService.createPackage creator

        CommonService.updateExpertNoReply expert.Id (Domain.Experts.addPackage package.Dsc.PackageId)

        return {|Record = package.Dsc |> packageRecord; Card = Main.packageCard expert.Id package CommonService.expertLoader; |}
    }

let updateProdPackageCard expert card =
    let logic (pkg:Domain.Package) =
        { pkg with
            Dsc = { pkg.Dsc with Name = card.Name}
            TransferToken = card.TransferToken
            Slips = card.Slips |> List.map slipToDomain
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

let deleteQuiz expert req =
    result {
        let! _ = (expert.Quizes |> List.tryFind ((=) req.QuizId), "Quiz belongs to another producer")

        Data.Teams.getIds req.QuizId
        |> List.iter (fun teamId -> Data.Teams.delete req.QuizId teamId)

        Data.Quizzes.delete req.QuizId

        CommonService.updateExpertNoReply expert.Id (Domain.Experts.removeQuiz req.QuizId)

        return ()
    }

let deletePackage expert req =
    result {
        let! _ = (expert.Packages |> List.tryFind ((=) req.PackageId), "Package belongs to another producer")

        let! pkg = Data.Packages.get req.PackageId, "Package not found"

        for userId in pkg.SharedWith do
            CommonService.updateExpertNoReply userId (Domain.Experts.removeSharedPackage req.PackageId)

        Data.Packages.delete req.PackageId
        CommonService.updateExpertNoReply expert.Id (Domain.Experts.removePackage req.PackageId)

        return ()
    }

let getSettings expert req =
    expert |> Main.settingsCard |> Ok

let updateSettings expert req =
    let logic (exp:Domain.Expert) =
        {exp with DefaultImg = req.DefaultImg; DefaultMixlr = req.DefaultMixlr} |> Ok

    result {
        let! exp = CommonService.updateExpert expert.Id logic
        return exp |> Main.settingsCard
    }

let sharePackage expert req =
    result{
        let! _ = (expert.Packages |> List.tryFind ((=) req.PackageId), "Package belongs to another producer")
        let! exp = Data.Experts.get req.UserId, "User not found"

        let! pgk = CommonService.updatePackage req.PackageId (Domain.Packages.shareWith req.UserId)
        CommonService.updateExpertNoReply req.UserId (Domain.Experts.addSharedPackage req.PackageId)

        return exp |> Main.expertRecord
    }

let removePackageShare expert req =
    result{
        let! _ = (expert.Packages |> List.tryFind ((=) req.PackageId), "Package belongs to another producer")
        let! exp = Data.Experts.get req.UserId, "User not found"

        let! pgk = CommonService.updatePackage req.PackageId (Domain.Packages.removeShareWith req.UserId)
        CommonService.updateExpertNoReply req.UserId (Domain.Experts.removeSharedPackage req.PackageId)

        return ()
    }
