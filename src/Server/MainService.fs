module rec MainService

open Microsoft.AspNetCore.Http

open Shared
open Common
open Env
open Presenter

module AR = AsyncResult

let api env (context:HttpContext) : IMainApi =
    let secret =  (env :> ICfg).Configurer.JwtSecret

    let ex proc f =
        let ff f = (fun expertId usersysname username (quizIdStr:string) req ->
            f expertId usersysname username (tryParseInt32 quizIdStr) req
        )

        SecurityService.exec (env :> ILog).Logger proc <| SecurityService.authorizeExpertCheckPrivateQuiz secret (ff f)

    let exPublisher proc f =

        let ff f = (fun expertId username req ->
            async{
                match! Data2.Experts.get env expertId with
                | Ok expert ->
                    return! f expert req
                | Error _ ->
                    env.Logger.Error ("{Op} {Error} {ExpertId}", "main", "Wrong Publisher Id", expertId)
                    return Error "Wrong Publisher Id"
            }
        )

        SecurityService.exec env.Logger proc <| SecurityService.authorizeExpert secret (ff f)

    let api : IMainApi = {
        becomeProducer = ex "becomeProducer" <| becomeProducer env
        getRegModel = ex "getRegModel" <| getRegModel env
        registerTeam = ex "registerTeam" <| registerTeam env
        createQuiz = exPublisher  "createQuiz" <| createQuiz env
        getProdQuizzes = exPublisher "getProdQuizzes" <| getProdQuizzes env
        getProdQuizCard = exPublisher "getProdQuizCard" <| getProdQuizCard env
        updateProdQuizCard = exPublisher "updateProdQuizCard" <| updateProdQuizCard env
        getProdPackages = exPublisher "getProdPackages" <| getProdPackages env
        getProdPackageCard = exPublisher  "getProdPackageCard" <| getProdPackageCard env
        createPackage = exPublisher  "createPackage" <| createPackage env
        updateProdPackageCard = exPublisher  "updateProdPackageCard" <| updateProdPackageCard env
        aquirePackage = exPublisher "aquirePackage" <| aquirePackage env
        deleteQuiz = exPublisher "deleteQuiz" <| deleteQuiz env
        deletePackage = exPublisher "deletePackage" <| deletePackage env
        getSettings = exPublisher "getSettings" <| getSettings
        updateSettings = exPublisher "updateSettings" <| updateSettings env
        sharePackage = exPublisher "sharePackage" <| sharePackage env
        removePackageShare = exPublisher "removePackageShare" <| removePackageShare env
        getUploadUrl = exPublisher "getUploadUrl" <| getUploadUrl env
    }

    api

let becomeProducer env expertId usersysname name _ _ =
    let creator = fun () -> Domain.Experts.createNew expertId usersysname name
    let logic expert = expert |> Domain.Experts.becomeProducer |> Ok

    Data2.Experts.updateOrCreate env expertId logic creator
    |> AR.map ignore

let createQuiz env expert _ =
    let creator quizId =
        Domain.Quizzes.createNew quizId expert.Id expert.DefaultImg expert.DefaultMixlr

    let logic (quiz:Domain.Quiz) expert  = expert |> Domain.Experts.addQuiz quiz.Dsc.QuizId |> Ok

    Data2.Quizzes.create env creator
    |> AR.side (fun quiz -> Data2.Experts.update env expert.Id (logic quiz))
    |> AR.map (fun quiz -> {|Record = quiz.Dsc |> Main.quizProdRecord; Card = Main.quizProdCard quiz; |})

let getProdQuizzes env expert _ =
    expert.Quizzes
    |> List.map (Data2.Quizzes.getDescriptor env)
    |> Async.Sequential
    |> Async.map (Array.choose (function Ok r -> Some (Main.quizProdRecord r) | _ -> None))
    |> Async.map (List.ofArray >> Ok)

let getProdQuizCard env expert (req : {|QuizId : int|}) =
    Data2.Quizzes.get env req.QuizId
    |> AR.sideRes (fun quiz -> Domain.Quizzes.authorize expert.Id quiz.Dsc)
    |> AR.map Main.quizProdCard

let updateProdQuizCard env expert card =
    let logic (quiz : Domain.Quiz) =
        Domain.Quizzes.authorize expert.Id quiz.Dsc
        |> Result.map (fun _ ->
            let dsc = { quiz.Dsc with Name = card.Name; StartTime = card.StartTime;
                                    ImgKey = card.ImgKey; WelcomeText = card.WelcomeText; FarewellText = card.FarewellText; RegText = card.RegText; InfoText = card.InfoText;
                                    WithPremoderation = card.WithPremoderation; EventPage = card.EventPage; MixlrCode = card.MixlrCode}

            { quiz with Dsc = dsc}
        )

    Data2.Quizzes.update env card.QuizId logic
    |> AR.map (fun quiz -> quiz.Dsc |> Main.quizProdRecord)

let deleteQuiz env expert req =
    Data2.Quizzes.getDescriptor env req.QuizId
    |> AR.sideRes (Domain.Quizzes.authorize expert.Id)
    |> AR.side ( fun _ ->
        Data2.Teams.getIds env req.QuizId
        |> AR.bind ( fun list ->
            list
            |> List.map (Data2.Teams.delete env req.QuizId)
            |> Async.Sequential
            |> Async.map (fun _ -> Ok ())))
    |> AR.side ( fun quiz ->
        Aws.deleteFile env (Aws.getResultsKey quiz.QuizId quiz.ListenToken)
        |> Async.map (fun _ -> Ok ()))
    |> AR.next (Data2.Quizzes.delete env req.QuizId)
    |> AR.next (Data2.Experts.update env expert.Id (Domain.Experts.removeQuiz req.QuizId))
    |> AR.map ignore

let getRegModel env expId username name quizId _ =
    quizId |> Result.fromOption "Quiz not defined"
    |> AR.fromResult
    |> AR.bind (fun quizId ->
        Data2.Experts.get env expId
        |> AR.bind (fun exp ->
            match exp.Competitions.TryFind quizId with
            | Some teamId ->
                Data2.Teams.getDescriptor env quizId teamId
                |> AR.map Some
                |> AR.ifError (fun _ -> None)
            | None -> AR.retn None)
        |> AR.ifError (fun _ -> None)
        |> AR.bind (fun team ->
            Data2.Quizzes.getDescriptor env quizId
            |> AR.map (fun quiz -> Main.quizRegRecord quiz team)))

let registerTeam env expId username name quizId req =
    let expCreator () = Domain.Experts.createNew expId username name

    quizId |> Result.fromOption "Quiz not defined"
    |> AR.fromResult
    |> AR.bind (Data2.Quizzes.getDescriptor env)
    |> AR.bind (fun quiz ->
        Data2.Experts.updateOrCreate env expId Ok expCreator
        |> AR.bind (fun exp ->
            let teamName = req.TeamName.Trim()
            match Domain.Experts.getComp quiz.QuizId exp with
            | Some teamId ->
                Data2.Teams.check env quiz.QuizId teamId
                |> AR.bind (fun exist ->
                    match exist with
                    | true ->
                        Data2.Teams.get env {QuizId = quiz.QuizId; TeamId = teamId}
                        |> AR.bind (fun team -> updateTeam env quiz team teamName)
                    | false -> createTeam env exp quiz teamName
                )
            | None -> createTeam env exp quiz teamName
            |> AR.map (fun (team:Domain.Team) -> Main.quizRegRecord quiz (Some team.Dsc))))
    |> AR.side (fun qr -> PublishResults qr.QuizId |> (env :> IPublisher).Publish |> AR.retn)

let private createTeam env exp quiz teamName  =

    let creator teamsInQuiz teamId =
        match Domain.Teams.validateTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew teamId teamName quiz |> Ok

    Data2.Teams.getDescriptors env quiz.QuizId
    |> AR.bind (fun teamsInQuiz ->
        Data2.Teams.create env quiz.QuizId (creator teamsInQuiz)
        |> AR.side (fun team ->
            let logic expert = expert |> Domain.Experts.addComp team.Dsc.QuizId team.Dsc.TeamId |> Ok
            Data2.Experts.update env exp.Id logic))

let private updateTeam env quiz team teamName  =
    match team.Dsc.Name = teamName with
    | true -> Ok team |> AsyncResult.fromResult
    | false ->
        let logic teamsInQuiz (team : Domain.Team) =
            result {
                do! match Domain.Teams.validateTeamUpdate false teamName teamsInQuiz quiz with Some txt -> Error txt | _ -> Ok()
                return team |> Domain.Teams.changeName teamName
            }
        Data2.Teams.getDescriptors env quiz.QuizId
        |> AR.bind (fun teamsInQuiz ->
            Data2.Teams.update env team.Key (logic teamsInQuiz))

let getProdPackages env expert _ =
    async{
        let! list =
            expert.AllPackages
            |> List.map (Data2.Packages.getDescriptor env)
            |> Async.Sequential

        return
            list
            |> List.ofArray
            |> List.choose (function Ok res -> res |> packageRecord |> Some | _ -> None)
            |> Ok
    }

let getProdPackageCard env expert (req : {|PackageId : int|}) =
    expert
    |> Domain.Experts.authorizePackageRead req.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.get env req.PackageId)
    |> AR.map (Main.packageCard expert.Id (Data2.Experts.provider env))

let createPackage env expert _ =
    Data2.Packages.create env (Domain.Packages.createNew expert.Id)
    |> AR.side (fun pkg -> Data2.Experts.update env expert.Id (Domain.Experts.addPackage pkg.Dsc.PackageId))
    |> AR.map (fun pkg -> {|Record = pkg.Dsc |> packageRecord; Card = Main.packageCard expert.Id (Data2.Experts.provider env) pkg ; |})

let updateProdPackageCard env expert card =
    let logic (pkg:Domain.Package) =
        { pkg with
            Dsc = { pkg.Dsc with Name = card.Name}
            TransferToken = card.TransferToken
            Slips = card.Slips |> List.map slipToDomain
        } |> Ok

    expert
    |> Domain.Experts.authorizePackageWrite card.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.update env card.PackageId logic)
    |> AR.map (fun pkg -> pkg.Dsc |> packageRecord)

let aquirePackage env expert req =
    Data2.Packages.getDescriptor env req.PackageId
    |> AR.bind (fun origPkg ->
        Data2.Packages.update env origPkg.PackageId (Domain.Packages.transfer expert.Id req.TransferToken)
        |> AR.side (fun _ -> Data2.Experts.update env origPkg.Producer (Domain.Experts.removePackage origPkg.PackageId)))
    |> AR.side (fun pkg -> Data2.Experts.update env expert.Id (Domain.Experts.addPackage pkg.Dsc.PackageId))
    |> AR.map (fun pkg -> packageRecord pkg.Dsc)

let deletePackage env expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.next (Data2.Packages.get env req.PackageId)
    |> AR.side (fun pkg ->
        pkg.SharedWith
        |> List.map (fun expId -> Data2.Experts.update env expId (Domain.Experts.removeSharedPackage req.PackageId))
        |> Async.Sequential
        |> Async.map (fun _ -> Ok ()))
    |> AR.next (Data2.Packages.delete env req.PackageId)
    |> AR.next (Data2.Experts.update env expert.Id (Domain.Experts.removePackage req.PackageId))
    |> AR.map ignore

let getSettings expert req =
    expert |> Main.settingsCard |> Ok |> AR.fromResult

let updateSettings env expert req =
    let logic (exp:Domain.Expert) =
        {exp with DefaultImg = req.DefaultImg; DefaultMixlr = req.DefaultMixlr} |> Ok

    Data2.Experts.update env expert.Id logic
    |> AR.map Main.settingsCard

let sharePackage env expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.side (fun _ -> Data2.Packages.update env req.PackageId (Domain.Packages.shareWith req.UserId))
    |> AR.next (Data2.Experts.update env req.UserId (Domain.Experts.addSharedPackage req.PackageId))
    |> AR.map Main.expertRecord

let removePackageShare env expert req =
    expert
    |> Domain.Experts.authorizePackageWrite req.PackageId
    |> AR.fromResult
    |> AR.side (fun _ -> Data2.Packages.update env req.PackageId (Domain.Packages.removeShareWith req.UserId))
    |> AR.next (Data2.Experts.update env req.UserId (Domain.Experts.removeSharedPackage req.PackageId))
    |> AR.map ignore

let getUploadUrl env expert req  =
    let key,url = Aws.getSignedUrl env.Configurer.BucketName req.Cat
    AR.retn  {|Url = url; BucketKey = key|}