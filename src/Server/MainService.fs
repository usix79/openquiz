module rec MainService

open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let private _packagesLockObj = System.Object()
let private _quizzesLockObj = System.Object()

let api (context:HttpContext) : IMainApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =
        Security.execute logger proc <| Security.authorizeExpert secret f

    let exPublisher proc f =

        let ff f = (fun expertId req ->
            match Data.Experts.get expertId with
            | Some expert ->
                f expert req
            | None ->
                Log.Error ("{Op} {Error} {ExpertId}", "main", "Wrong Publisher Id", expertId)
                Error "Wrong Publisher Id"
        )

        Security.execute logger proc <| Security.authorizeExpert secret (ff f)

    let api : IMainApi = {
        becomeProducer = ex  "becomeProducer" <| becomeProducer
        getPubModel = ex "getPubModel"<| getPubModel
        registerTeam = ex "registerTeam" <| registerTeam
        createQuiz = exPublisher  "createQuiz" <| createQuiz
        getProdQuizzes = exPublisher "getProdQuizzes"<| getProdQuizzes
        getProdQuizCard = exPublisher "getProdQuizCard" <| getProdQuizCard
        updateProdQuizCard = exPublisher "updateProdQuizCard" <| updateProdQuizCard
        uploadFile = exPublisher "uploadFile" <| uploadFile (Config.getFilesAccessPoint cfg)
        getProdPackages = exPublisher "getProdPackages"<| getProdPackages
        getProdPackageCard = exPublisher  "getProdPackageCard"<| getProdPackageCard
        createPackage = exPublisher  "createPackage" <| createPackage
        updateProdPackageCard = exPublisher  "updateProdPackageCard" <| updateProdPackageCard
    }

    api

let becomeProducer expertId _ =
    defaultArg (Data.Experts.get expertId) (Domain.Experts.createNew expertId)
    |> Domain.Experts.becomeProducer
    |> Data.Experts.update
    |> ignore

    Ok ()

let createQuiz expert _ =
    let transaction = fun () ->
        let quizId = Data.Quizzes.getMaxId() + 1
        let quiz = Domain.Quizzes.createNew quizId expert.Id
        Data.Quizzes.update quiz

    let quiz = lock _quizzesLockObj transaction

    expert
    |> Domain.Experts.addQuiz quiz.Dsc.QuizId
    |> Data.Experts.update
    |> ignore

    Ok {|Record = quiz.Dsc |> Main.quizProdRecord; Card = Main.quizProdCard quiz; |}

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
    match Data.Quizzes.get card.QuizId with
    | Some quiz when quiz.Dsc.Producer <> expert.Id -> Error "Quiz is produced by someone else"
    | None -> Error "Quiz not found"
    | Some quiz ->
        let dsc = { quiz.Dsc with Brand = card.Brand; Name = card.Name; StartTime = card.StartTime; Status = quizStatusToDmain card.Status;
                                ImgKey = card.ImgKey; WelcomeText = card.WelcomeText; FarewellText = card.FarewellText; IsPrivate = card.IsPrivate;
                                WithPremoderation = card.WithPremoderation}

        let updatedQuiz = { quiz with Dsc = dsc} |> Data.Quizzes.update

        updatedQuiz.Dsc |> Main.quizProdRecord |> Ok

let uploadFile bucketName _ req =
    Bucket.uploadFile  bucketName req.Cat req.FileType req.FileBody

let getPubModel expId _ =
    let comps =
        match Data.Experts.get expId with
        | Some exp -> exp.Competitions
        | None -> Map.empty

    let quizzes =
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

let registerTeam expId req =
    result {
        let! quiz = ((Data.Quizzes.getDescriptor req.QuizId), "Quiz not found")

        let exp = defaultArg (Data.Experts.get expId) (Domain.Experts.createNew expId)

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
    let teamName = teamName.Trim()

    let creator (key : Domain.TeamKey) =
        let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId
        match Domain.Teams.validatePublicTeamUpdate true teamName teamsInQuiz quiz with
        | Some txt -> Error txt
        | None -> Domain.Teams.createNew key.TeamId teamName quiz |> Ok

    result {
        let! team = CommonService.createTeam quiz.QuizId creator

        exp
        |> Domain.Experts.addComp team.Dsc.QuizId team.Dsc.TeamId
        |> Data.Experts.update
        |> ignore

        return team
    }

let private updateTeam quiz team teamName : Result<Domain.Team,string> =
    match team.Dsc.Name = teamName with
    | true -> Ok team
    | false ->
        let logic (team : Domain.Team) =
            result {
                let teamsInQuiz = Data.Teams.getDescriptors quiz.QuizId

                do! match Domain.Teams.validatePublicTeamUpdate false teamName teamsInQuiz quiz with Some txt -> Error txt | _ -> Ok()

                return team |> Domain.Teams.changeName teamName
            }

        CommonService.updateTeam team.Key logic

let getProdPackages expert _ =
    expert.Packages
    |> List.map Data.Packages.getDescriptor
    |> List.filter (fun p -> p.IsSome)
    |> List.map (fun p -> Main.packageProdRecord p.Value)
    |> Ok

let getProdPackageCard expert (req : {|PackageId : int|}) =
    match Data.Packages.get req.PackageId with
    | Some package when package.Dsc.Producer <> expert.Id -> Error "Package is produced by someone else"
    | None -> Error "Package not found"
    | Some package -> Ok <| Main.packageProdCard package

let createPackage expert _ =
    let transaction = fun () ->
        let packageId = Data.Packages.getMaxId() + 1
        let package = Domain.Packages.createNew packageId expert.Id
        Data.Packages.update package

    let package = lock _packagesLockObj transaction

    expert
    |> Domain.Experts.addPackage package.Dsc.PackageId
    |> Data.Experts.update
    |> ignore

    Ok {|Record = package.Dsc |> Main.packageProdRecord; Card = Main.packageProdCard package; |}

let updateProdPackageCard expert card =
    match Data.Packages.get card.PackageId with
    | Some pkg when pkg.Dsc.Producer <> expert.Id -> Error "Package is produced by someone else"
    | None -> Error "Package not found"
    | Some pkg ->
        let updatedPkg =
            { pkg with
                Dsc = { pkg.Dsc with Name = card.Name}
                Questions = card.Questions |> List.map Main.packageQw
            }
            |> Data.Packages.update

        updatedPkg.Dsc |> Main.packageProdRecord |> Ok
