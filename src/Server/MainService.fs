module rec MainService

open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

module AR = AsyncResult

let api (context:HttpContext) : IMainApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =
        let ff f = (fun expertId usersysname username (quizIdStr:string) req ->
            f expertId usersysname username (tryParseInt32 quizIdStr) req
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
        registerTeam = ex "registerTeam" (registerTeam (Config.getMediaBucketName cfg))
        createQuiz = exPublisher  "createQuiz" createQuiz
        getProdQuizzes = exPublisher "getProdQuizzes" getProdQuizzes
        getProdQuizCard = exPublisher "getProdQuizCard" getProdQuizCard
        updateProdQuizCard = exPublisher "updateProdQuizCard" updateProdQuizCard
        getProdPackages = exPublisher "getProdPackages" getProdPackages
        getProdPackageCard = exPublisher  "getProdPackageCard" getProdPackageCard
        createPackage = exPublisher  "createPackage" createPackage
        updateProdPackageCard = exPublisher  "updateProdPackageCard" updateProdPackageCard
        aquirePackage = exPublisher "aquirePackage" aquirePackage
        deleteQuiz = exPublisher "deleteQuiz" <| deleteQuiz (Config.getMediaBucketName cfg)
        deletePackage = exPublisher "deletePackage" deletePackage
        getSettings = exPublisher "getSettings" getSettings
        updateSettings = exPublisher "updateSettings" updateSettings
        sharePackage = exPublisher "sharePackage" sharePackage
        removePackageShare = exPublisher "removePackageShare" removePackageShare
        getUploadUrl = exPublisher "getUploadUrl" <| getUploadUrl (Config.getMediaBucketName cfg)
    }

    api

let becomeProducer expertId usersysname name _ _ =
    let creator = fun () -> Domain.Experts.createNew expertId usersysname name
    let logic expert = expert |> Domain.Experts.becomeProducer |> Ok

    Data2.Experts.updateOrCreate expertId logic creator
    |> AR.map ignore

let createQuiz expert _ =
    let creator quizId =
        Domain.Quizzes.createNew quizId expert.Id expert.DefaultImg expert.DefaultMixlr

    let logic (quiz:Domain.Quiz) expert  = expert |> Domain.Experts.addQuiz quiz.Dsc.QuizId |> Ok

    Data2.Quizzes.create creator
    |> AR.side (fun quiz -> Data2.Experts.update expert.Id (logic quiz))
    |> AR.map (fun quiz -> {|Record = quiz.Dsc |> Main.quizProdRecord; Card = Main.quizProdCard quiz; |})

let getProdQuizzes expert _ =
    expert.Quizzes
    |> List.map Data2.Quizzes.getDescriptor
    |> Async.Sequential
    |> Async.map (Array.choose (function Ok r -> Some (Main.quizProdRecord r) | _ -> None))
    |> Async.map (List.ofArray >> Ok)

let getProdQuizCard expert (req : {|QuizId : int|}) =
    Data2.Quizzes.get req.QuizId
    |> AR.sideRes (fun quiz -> Domain.Quizzes.authorize expert.Id quiz.Dsc)
    |> AR.map Main.quizProdCard

let updateProdQuizCard expert card =
    let logic (quiz : Domain.Quiz) =
        Domain.Quizzes.authorize expert.Id quiz.Dsc
        |> Result.map (fun _ ->
            let dsc = { quiz.Dsc with Name = card.Name; StartTime = card.StartTime;
                                    ImgKey = card.ImgKey; WelcomeText = card.WelcomeText; FarewellText = card.FarewellText; InfoText = card.InfoText;
                                    WithPremoderation = card.WithPremoderation; EventPage = card.EventPage; MixlrCode = card.MixlrCode}

            { quiz with Dsc = dsc}
        )

    Data2.Quizzes.update card.QuizId logic
    |> AR.map (fun quiz -> quiz.Dsc |> Main.quizProdRecord)

let deleteQuiz bucket expert req =
    Data2.Quizzes.getDescriptor req.QuizId
    |> AR.sideRes (Domain.Quizzes.authorize expert.Id)
    |> AR.side ( fun _ ->
        Data2.Teams.getIds req.QuizId
        |> AR.bind ( fun list ->
            list
            |> List.map (Data2.Teams.delete req.QuizId)
            |> Async.Sequential
            |> Async.map (fun _ -> Ok ())))
    |> AR.side ( fun quiz ->
        Bucket.deleteFile bucket (Bucket.getResultsKey quiz.QuizId quiz.ListenToken)
        |> Async.map (fun _ -> Ok ()))
    |> AR.next (Data2.Quizzes.delete req.QuizId)
    |> AR.next (Data2.Experts.update expert.Id (Domain.Experts.removeQuiz req.QuizId))
    |> AR.map ignore

let getRegModel expId username name quizId _ =
    quizId |> Result.fromOption "Quiz not defined"
    |> AR.fromResult
    |> AR.bind (fun quizId ->
        Data2.Experts.get expId
        |> AR.bind (fun exp ->
            match exp.Competitions.TryFind quizId with
            | Some teamId ->
                Data2.Teams.getDescriptor quizId teamId
                |> AR.map Some
                |> AR.ifError (fun _ -> None)
            | None -> AR.retn None)
        |> AR.ifError (fun _ -> None)
        |> AR.bind (fun team ->
            Data2.Quizzes.getDescriptor quizId
            |> AR.map (fun quiz -> Main.quizRegRecord quiz team)))

let registerTeam bucketName expId username name quizId req =
    let expCreator () = Domain.Experts.createNew expId username name

    quizId |> Result.fromOption "Quiz not defined"
    |> AR.fromResult
    |> AR.bind Data2.Quizzes.getDescriptor
    |> AR.bind (fun quiz ->
        Data2.Experts.updateOrCreate expId Ok expCreator
        |> AR.bind (fun exp ->
            let teamName = req.TeamName.Trim()
            match Domain.Experts.getComp quiz.QuizId exp with
            | Some teamId ->
                Data2.Teams.check quiz.QuizId teamId
                |> AR.bind (fun exist ->
                    match exist with
                    | true ->
                        Data2.Teams.get {QuizId = quiz.QuizId; TeamId = teamId}
                        |> AR.bind (fun team -> updateTeam quiz team teamName)
                    | false -> createTeam exp quiz teamName
                )
            | None -> createTeam exp quiz teamName
            |> AR.map (fun (team:Domain.Team) -> Main.quizRegRecord quiz (Some team.Dsc))))
    |> AR.side (fun qr -> Agents.PublishResults (qr.QuizId, bucketName) |> Agents.publish |> AR.retn)

let private createTeam exp quiz teamName  =

    let creator teamsInQuiz teamId =
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew teamId teamName quiz |> Ok

    Data2.Teams.getDescriptors quiz.QuizId
    |> AR.bind (fun teamsInQuiz ->
        Data2.Teams.create quiz.QuizId (creator teamsInQuiz)
        |> AR.side (fun team ->
            let logic expert = expert |> Domain.Experts.addComp team.Dsc.QuizId team.Dsc.TeamId |> Ok
            Data2.Experts.update exp.Id logic))

let private updateTeam quiz team teamName  =
    match team.Dsc.Name = teamName with
    | true -> Ok team |> AsyncResult.fromResult
    | false ->
        let logic teamsInQuiz (team : Domain.Team) =
            result {
                do! match Domain.Teams.validateTeamUpdate false teamName teamsInQuiz quiz with Some txt -> Error txt | _ -> Ok()
                return team |> Domain.Teams.changeName teamName
            }
        Data2.Teams.getDescriptors quiz.QuizId
        |> AR.bind (fun teamsInQuiz ->
            Data2.Teams.update team.Key (logic teamsInQuiz))

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
    |> Domain.Experts.authorizePackageRead req.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.get req.PackageId)
    |> AR.map (Main.packageCard expert.Id Data2.Experts.provider)

let createPackage expert _ =
    Data2.Packages.create (Domain.Packages.createNew expert.Id)
    |> AR.side (fun pkg -> Data2.Experts.update expert.Id (Domain.Experts.addPackage pkg.Dsc.PackageId))
    |> AR.map (fun pkg -> {|Record = pkg.Dsc |> packageRecord; Card = Main.packageCard expert.Id Data2.Experts.provider pkg ; |})

let updateProdPackageCard expert card =
    let logic (pkg:Domain.Package) =
        { pkg with
            Dsc = { pkg.Dsc with Name = card.Name}
            TransferToken = card.TransferToken
            Slips = card.Slips |> List.map slipToDomain
        } |> Ok

    expert
    |> Domain.Experts.authorizePackageWrite card.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.update card.PackageId logic)
    |> AR.map (fun pkg -> pkg.Dsc |> packageRecord)

let aquirePackage expert req =
    Data2.Packages.getDescriptor req.PackageId
    |> AR.bind (fun origPkg ->
        Data2.Packages.update origPkg.PackageId (Domain.Packages.transfer expert.Id req.TransferToken)
        |> AR.side (fun _ -> Data2.Experts.update origPkg.Producer (Domain.Experts.removePackage origPkg.PackageId)))
    |> AR.side (fun pkg -> Data2.Experts.update expert.Id (Domain.Experts.addPackage pkg.Dsc.PackageId))
    |> AR.map (fun pkg -> packageRecord pkg.Dsc)

let deletePackage expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.get req.PackageId)
    |> AR.side (fun pkg ->
        pkg.SharedWith
        |> List.map (fun expId -> Data2.Experts.update expId (Domain.Experts.removeSharedPackage req.PackageId))
        |> Async.Sequential
        |> Async.map (fun _ -> Ok ()))
    |> AR.next (Data2.Packages.delete req.PackageId)
    |> AR.next (Data2.Experts.update expert.Id (Domain.Experts.removePackage req.PackageId))
    |> AR.map ignore

let getSettings expert req =
    expert |> Main.settingsCard |> Ok |> AR.fromResult

let updateSettings expert req =
    let logic (exp:Domain.Expert) =
        {exp with DefaultImg = req.DefaultImg; DefaultMixlr = req.DefaultMixlr} |> Ok

    Data2.Experts.update expert.Id logic
    |> AR.map Main.settingsCard

let sharePackage expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.side (fun _ -> Data2.Packages.update req.PackageId (Domain.Packages.shareWith req.UserId))
    |> AR.next (Data2.Experts.update req.UserId (Domain.Experts.addSharedPackage req.PackageId))
    |> AR.map Main.expertRecord

let removePackageShare expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.side (fun _ -> Data2.Packages.update req.PackageId (Domain.Packages.removeShareWith req.UserId))
    |> AR.next (Data2.Experts.update req.UserId (Domain.Experts.removeSharedPackage req.PackageId))
    |> AR.map ignore

let getUploadUrl bucketName expert req  =
    let key,url = Bucket.getSignedUrl bucketName req.Cat
    AR.retn  {|Url = url; BucketKey = key|}