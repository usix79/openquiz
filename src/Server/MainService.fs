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

        SecurityService.exec logger proc <| SecurityService.authorizeExpertCheckPrivateQuiz secret (ff f)

    let exPublisher proc f =

        let ff f = (fun expertId username req ->
            async{
                match! Data2.Experts.get expertId with
                | Ok expert ->
                    return! f expert req
                | Error _ ->
                    Log.Error ("{Op} {Error} {ExpertId}", "main", "Wrong Publisher Id", expertId)
                    return Error "Wrong Publisher Id"
            }
        )

        SecurityService.exec logger proc <| SecurityService.authorizeExpert secret (ff f)

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

let private checkPackageOwner (expert:Domain.Expert) packageId f =
    async {
        match expert.Packages |> List.tryFind ((=) packageId) with
        | Some _ -> return! f ()
        | None -> return Error "Package belongs to another producer"
    }

let becomeProducer expertId usersysname name _ _ =
    let creator = fun () -> Domain.Experts.createNew expertId usersysname name
    let logic expert = expert |> Domain.Experts.becomeProducer |> Ok

    Data2.Experts.updateOrCreate expertId logic creator
    |> AsyncResult.map ignore

let createQuiz expert _ =
    let creator quizId =
        Ok <| Domain.Quizzes.createNew quizId expert.Id expert.DefaultImg expert.DefaultMixlr

    result{
        let! quiz = CommonService.createQuiz creator

        let logic expert = expert |> Domain.Experts.addQuiz quiz.Dsc.QuizId |> Ok
        Data2.Experts.update expert.Id logic |> Async.RunSynchronously |> ignore

        return {|Record = quiz.Dsc |> Main.quizProdRecord; Card = Main.quizProdCard quiz; |}
    } |> Async.retn

let getProdQuizzes expert _ =
    expert.Quizzes
    |> List.map Data.Quizzes.getDescriptor
    |> List.filter (fun q -> q.IsSome)
    |> List.map (fun q -> Main.quizProdRecord q.Value)
    |> AsyncResult.retn

let getProdQuizCard expert (req : {|QuizId : int|}) =
    match Data.Quizzes.get req.QuizId with
    | Some quiz when quiz.Dsc.Producer <> expert.Id -> Error "Quiz is produced by someone else"
    | None -> Error "Quiz not found"
    | Some quiz -> Ok <| Main.quizProdCard quiz
    |> Async.retn

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
    |> Async.retn

let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody
    |> Async.retn

let getRegModel expId username name quizId _ =

    let tryFindTeam quizId =
        async {
            match! Data2.Experts.get expId with
            | Ok exp ->
                return
                    exp.Competitions.TryFind quizId
                    |> Option.bind (Data.Teams.getDescriptor quizId)
            | _ -> return None
        }

    async{
        match quizId with
        | Some quizId ->
            let! team = tryFindTeam quizId
            return
                match Data.Quizzes.getDescriptor quizId with
                | Some quiz -> Main.quizRegRecord quiz team |> Ok
                | None -> Error  "Quiz not found"
        | None -> return Error "Quiz not defined"
    }

let registerTeam expId username name quizId req =
    let expCreator () = Domain.Experts.createNew expId username name

    result {
        let! quizId = quizId, "Quiz not defined"
        let! quiz = (Data.Quizzes.getDescriptor quizId), "Quiz not found"

        let! exp = Data2.Experts.updateOrCreate expId Ok expCreator  |> Async.RunSynchronously

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
    |> Async.retn

let private createTeam exp quiz teamName : Result<Domain.Team,string> =

    let creator (key : Domain.TeamKey) =
        let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew key.TeamId teamName quiz |> Ok

    result {
        let! team = CommonService.createTeam quiz.QuizId creator

        let logic expert = expert |> Domain.Experts.addComp team.Dsc.QuizId team.Dsc.TeamId |> Ok
        Data2.Experts.update exp.Id logic |> Async.RunSynchronously |> ignore

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
    async{
        let! list =
            expert.AllPackages
            |> List.map Data2.Packages.getDescriptor
            |> Async.Sequential

        return
            list
            |> List.ofArray
            |> List.choose (function Ok res -> res |> packageRecord |> Some | _ -> None)
            |> Ok
    }

let getProdPackageCard expert (req : {|PackageId : int|}) =
    expert
    |> Domain.Experts.authorizePackage req.PackageId
    |> AsyncResult.fromResult
    |> AsyncResult.bind (fun _ -> Data2.Packages.get req.PackageId)
    |> AsyncResult.map (Main.packageCard expert.Id Data2.Experts.provider)

let createPackage expert _ =
    let creator = fun id ->
        Domain.Packages.createNew id expert.Id |> Ok

    result {
        let! package = CommonService.createPackage creator

        Data2.Experts.update expert.Id (Domain.Experts.addPackage package.Dsc.PackageId) |> Async.RunSynchronously |> ignore

        return {|Record = package.Dsc |> packageRecord; Card = Main.packageCard expert.Id Data2.Experts.provider package ; |}
    }
    |> Async.retn

let updateProdPackageCard expert card =
    let logic (pkg:Domain.Package) =
        { pkg with
            Dsc = { pkg.Dsc with Name = card.Name}
            TransferToken = card.TransferToken
            Slips = card.Slips |> List.map slipToDomain
        } |> Ok

    checkPackageOwner expert card.PackageId (fun () ->
        Data2.Packages.update card.PackageId logic
        |> AsyncResult.map (fun pkg -> pkg.Dsc |> packageRecord)
    )

let aquirePackage expert res =
    result{
        let! origPackageDsc = (Data.Packages.getDescriptor res.PackageId, "Invalid Transfer Token")
        let! updatedPackage = CommonService.updatePackage res.PackageId (Domain.Packages.transfer expert.Id res.TransferToken)

        Data2.Experts.update origPackageDsc.Producer (Domain.Experts.removePackage origPackageDsc.PackageId) |> Async.RunSynchronously |> ignore
        Data2.Experts.update expert.Id (Domain.Experts.addPackage updatedPackage.Dsc.PackageId) |> Async.RunSynchronously |> ignore

        return updatedPackage.Dsc |> packageRecord
    }
    |> Async.retn

let deleteQuiz expert req =
    result {
        let! _ = (expert.Quizzes |> List.tryFind ((=) req.QuizId), "Quiz belongs to another producer")

        Data.Teams.getIds req.QuizId
        |> List.iter (fun teamId -> Data.Teams.delete req.QuizId teamId)

        Data.Quizzes.delete req.QuizId

        Data2.Experts.update expert.Id (Domain.Experts.removeQuiz req.QuizId) |> Async.RunSynchronously |> ignore

        return ()
    }
    |> Async.retn

let deletePackage expert req =
    result {
        let! _ = (expert.Packages |> List.tryFind ((=) req.PackageId), "Package belongs to another producer")

        let! pkg = Data.Packages.get req.PackageId, "Package not found"

        for userId in pkg.SharedWith do
            Data2.Experts.update  userId (Domain.Experts.removeSharedPackage req.PackageId) |> Async.RunSynchronously |> ignore

        Data.Packages.delete req.PackageId
        Data2.Experts.update  expert.Id (Domain.Experts.removePackage req.PackageId) |> Async.RunSynchronously |> ignore

        return ()
    }
    |> Async.retn

let getSettings expert req =
    expert |> Main.settingsCard |> Ok |> AsyncResult.fromResult

let updateSettings expert req =
    let logic (exp:Domain.Expert) =
        {exp with DefaultImg = req.DefaultImg; DefaultMixlr = req.DefaultMixlr} |> Ok

    Data2.Experts.update expert.Id logic
    |> AsyncResult.map Main.settingsCard


let sharePackage expert req =
    checkPackageOwner expert req.PackageId (fun () ->
        Data2.Experts.update req.UserId (Domain.Experts.addSharedPackage req.PackageId)
        |> AsyncResult.bind (fun exp ->
            CommonService.updatePackage req.PackageId (Domain.Packages.shareWith req.UserId)
            |> Result.map (fun _ -> exp |> Main.expertRecord)
            |> AsyncResult.fromResult
        )
    )

let removePackageShare expert req =
    checkPackageOwner expert req.PackageId (fun () ->
        Data2.Experts.update req.UserId (Domain.Experts.removeSharedPackage req.PackageId)
        |> AsyncResult.bind (fun _ ->
            CommonService.updatePackage req.PackageId (Domain.Packages.removeShareWith req.UserId)
            |> Result.map ignore
            |> AsyncResult.fromResult
        )
    )
