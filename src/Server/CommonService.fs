module CommonService

open Serilog

open Common
open Domain

type Generator<'PKey,'TKey> = 'PKey -> 'TKey
type Creator<'TKey, 'T> = 'TKey -> Result<'T, string>
type Logic<'T> = 'T -> Result<'T, string>
type Loader<'TKey, 'T> = 'TKey -> 'T option
type Saver<'T> = 'T -> 'T
type Broker<'T> = 'T -> unit

type LoaderAsync<'TKey, 'T> = 'TKey -> Async<'T option>
type SaverAsync<'T> = 'T -> Async<'T>

module DomainEvents =

    let mutable private subscribers = []

    let subscribeOnQuizChanges (f: Shared.QuizChangedEvent -> unit) =
        subscribers <- f :: subscribers

    let quizChanged (quiz : Quiz) =
        let evt = Presenter.quizChangeEvent quiz

        subscribers
        |> List.iter (fun f -> f evt)

type private UpdateCommand<'PKey,'TKey, 'T> =
    | Create of 'PKey*Creator<'TKey,'T> * AsyncReplyChannel<Result<'T, string>>
    | Update of 'TKey * Logic<'T> * AsyncReplyChannel<Result<'T, string>>
    | UpdateWithoutReply of 'TKey * Logic<'T>
    | UpdateOrCreate of 'PKey*'TKey * Creator<'TKey,'T>*Logic<'T> * AsyncReplyChannel<Result<'T, string>>

let private createAgent<'PKey,'TKey,'T>
    (generator : Generator<'PKey,'TKey>)
    (loader : Loader<'TKey,'T>)
    (saver : Saver<'T>)
    (broker : Broker<'T>) =
    MailboxProcessor<UpdateCommand<'PKey,'TKey,'T>>.Start (fun inbox ->

        let create' parentKey creator =
            match generator parentKey |> creator with
            | Ok entity -> saver entity |> Ok
            | Error txt -> Error txt

        let update' logic entity =
            match logic entity with
            | Ok entity ->
                let savedEntity = saver entity
                broker savedEntity
                savedEntity |> Ok
            | Error txt -> Error txt

        let updateOrCreate pkey key creator logic =
            try
                match loader key with
                | Some entity -> update' logic entity
                | None ->
                    match create' pkey creator with
                    | Ok entity -> update' logic entity
                    | Error txt -> Error txt
            with
            | ex ->
                Log.Error ("{Op} {Exeption}", "updateOrCreateAgent", ex)
                Error ex.Message

        let update key logic =
            try
                match loader key with
                | Some entity -> update' logic entity
                | None -> Error "Entity Not Found"
            with
            | ex ->
                Log.Error ("{Op} {Exeption}", "updateAgent", ex)
                Error ex.Message

        let rec loop () =
            async {
                try
                    let! msg = inbox.Receive()
                    match msg with
                    | Create (pkey,creator,rch) -> create' pkey creator |> rch.Reply
                    | Update (key,logic, rch) -> update key logic |> rch.Reply
                    | UpdateWithoutReply (key,logic) -> update key logic |> ignore
                    | UpdateOrCreate (pkey,key,creator,logic, rch) -> updateOrCreate pkey key creator logic |> rch.Reply
                with
                | ex -> Log.Error ("{Op} {Exeption}", "updateAgentMainLoop!!!", ex)

                return! loop()
            }
        loop())

let private createAsyncAgent<'PKey,'TKey,'T>
    (generator : Generator<'PKey,'TKey>)
    (loader : LoaderAsync<'TKey,'T>)
    (saver : SaverAsync<'T>)
    (broker : Broker<'T>) =
    MailboxProcessor<UpdateCommand<'PKey,'TKey,'T>>.Start (fun inbox ->

        let create' parentKey creator =
            async{
                match generator parentKey |> creator with
                | Ok entity ->
                    let! en = saver entity
                    return en |> Ok
                | Error txt -> return Error txt

            }

        let update' logic entity =
            async{
                match logic entity with
                | Ok entity ->
                    let! savedEntity = saver entity
                    broker savedEntity
                    return savedEntity |> Ok
                | Error txt -> return Error txt
            }

        let updateOrCreate pkey key creator logic =
            async{
                try
                    match! loader key with
                    | Some entity -> return! update' logic entity
                    | None ->
                        match! create' pkey creator with
                        | Ok entity -> return! update' logic entity
                        | Error txt -> return Error txt
                with
                | ex ->
                    Log.Error ("{Op} {Exeption}", "updateOrCreateAgent", ex)
                    return Error ex.Message

            }

        let update key logic =
            async{
                try
                    printfn "[%i] loading %A" (System.Threading.Thread.CurrentThread.ManagedThreadId) key
                    match! loader key with
                    | Some entity ->
                        printfn "[%i] update %A" (System.Threading.Thread.CurrentThread.ManagedThreadId) key
                        let! x = update' logic entity
                        printfn "[%i] update %A DONE" (System.Threading.Thread.CurrentThread.ManagedThreadId) key
                        return x
                    | None -> return Error "Entity Not Found"
                with
                | ex ->
                    Log.Error ("{Op} {Exeption}", "updateAgent", ex)
                    return Error ex.Message

            }

        let rec loop () =
            async {
                try
                    let! msg = inbox.Receive()
                    match msg with
                    | Create (pkey,creator,rch) -> create' pkey creator |> Async.RunSynchronously |> rch.Reply
                    | Update (key,logic, rch) -> update key logic |> Async.RunSynchronously |> rch.Reply
                    | UpdateWithoutReply (key,logic) ->
                        printfn "Update Team Agent %A Threads %A" key (System.Threading.ThreadPool.GetAvailableThreads())

                        let! _ = update key logic
                        ()
                    | UpdateOrCreate (pkey,key,creator,logic, rch) -> updateOrCreate pkey key creator logic |> Async.RunSynchronously |> rch.Reply
                with
                | ex -> Log.Error ("{Op} {Exeption}", "updateAgentMainLoop!!!", ex)

                return! loop()
            }
        loop())


let mutable private _teamAgents : Map<TeamKey, MailboxProcessor<UpdateCommand<int,TeamKey,Team>>> = Map.empty

let teamLoader (key:TeamKey) = Data.Teams.get key.QuizId key.TeamId
let private teamSaver team = Data.Teams.update team
let private teamGenerator (quizId:int) : TeamKey =
    {QuizId = quizId; TeamId = (Data.Teams.getMaxId quizId) + 1}

let teamLoaderAsync (key:TeamKey) =
    printfn "LoadAsync %A" key
    let t = Data.Teams.getAsync key.QuizId key.TeamId
    printfn "LoadAsync %A DONE" key
    t

let private teamSaverAsync team =
    printfn "AupdateAsync %A" (team.Dsc.Key)
    let t = Data.Teams.updateAsync team
    printfn "AupdateAsync %A DONE" (team.Dsc.Key)
    t

let private getOrCreateTeamAgent key =
    let trans () =
        match Map.tryFind key _teamAgents with
        | Some agent -> agent
        | None ->
            let agent = createAsyncAgent teamGenerator teamLoaderAsync teamSaverAsync ignore
            _teamAgents <- _teamAgents.Add (key, agent)
            agent

    lock _teamAgents trans

let createTeam quizId (creator:Creator<TeamKey,Team>) =
    let agent = getOrCreateTeamAgent {QuizId = quizId; TeamId = -1}
    agent.PostAndReply (fun rch -> Create (quizId, creator, rch))

let updateTeam (key : TeamKey) (logic : Logic<Team>) : Result<Domain.Team,string> =
    let agent = getOrCreateTeamAgent key
    agent.PostAndReply (fun rch -> Update (key, logic, rch))

let updateTeamNoReply (key : TeamKey) (logic : Logic<Team>) =
    let agent = getOrCreateTeamAgent key
    agent.Post (UpdateWithoutReply (key, logic))

let mutable private _quizAgents : Map<int, MailboxProcessor<UpdateCommand<unit,int,Quiz>>> = Map.empty

let  quizLoader (quizId : int) = Data.Quizzes.get quizId
let private quizGenerator () = Data.Quizzes.getMaxId () + 1
let private quizSaver quiz = Data.Quizzes.update quiz
let private quizBroker quiz = DomainEvents.quizChanged quiz

let private getOrCreateQuizAgent (quizId:int) =
    let trans () =
        match Map.tryFind quizId _quizAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent quizGenerator quizLoader quizSaver quizBroker
            _quizAgents <- _quizAgents.Add (quizId, agent)
            agent

    lock _quizAgents trans

let createQuiz (creator:Creator<int,Quiz>) =
    let agent = getOrCreateQuizAgent -1
    agent.PostAndReply (fun rch -> Create ((), creator, rch))

let updateQuiz (quizId : int) (logic : Logic<Quiz>) : Result<Domain.Quiz,string> =
    let agent = getOrCreateQuizAgent quizId
    agent.PostAndReply (fun rch -> Update (quizId, logic, rch))

let updateQuizNoReply (quizId : int) (logic : Logic<Quiz>) =
    let agent = getOrCreateQuizAgent quizId
    agent.Post (UpdateWithoutReply (quizId, logic))

let mutable private _expertAgents : Map<string, MailboxProcessor<UpdateCommand<unit,string,Expert>>> = Map.empty

let expertLoader (sub:string) = Data.Experts.get sub
let private expertGenerator () = ""
let private expertSaver expert = Data.Experts.update expert

let private getOrCreateExpertAgent key =
    let trans () =
        match Map.tryFind key _expertAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent expertGenerator expertLoader expertSaver ignore
            _expertAgents <- _expertAgents.Add (key, agent)
            agent

    lock _expertAgents trans

let updateExpert (sub : string) (logic : Logic<Expert>) : Result<Domain.Expert,string> =
    let agent = getOrCreateExpertAgent sub
    agent.PostAndReply (fun rch -> Update (sub, logic, rch))

let updateExpertNoReply (sub : string) (logic : Logic<Expert>) =
    let agent = getOrCreateExpertAgent sub
    agent.Post (UpdateWithoutReply (sub, logic))

let updateOrCreateExpert (sub : string) (creator:Creator<string,Expert>) (logic : Logic<Expert>) =
    let agent = getOrCreateExpertAgent sub
    agent.PostAndReply (fun rch -> UpdateOrCreate ((), sub, creator, logic, rch))

let mutable private _pkgAgents : Map<int, MailboxProcessor<UpdateCommand<unit,int,Package>>> = Map.empty

let packageLoader (id:int) = Data.Packages.get id
let private packageGenerator () = Data.Packages.getMaxId () + 1
let private packageSaver team = Data.Packages.update team

let private getOrCreatePackageAgent id =
    let trans () =
        match Map.tryFind id _pkgAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent packageGenerator packageLoader packageSaver ignore
            _pkgAgents <- _pkgAgents.Add (id, agent)
            agent

    lock _pkgAgents trans

let createPackage (creator:Creator<int,Package>) =
    let agent = getOrCreatePackageAgent -1
    agent.PostAndReply (fun rch -> Create ((), creator, rch))

let updatePackage (id : int) (logic : Logic<Package>) : Result<Domain.Package,string> =
    let agent = getOrCreatePackageAgent id
    agent.PostAndReply (fun rch -> Update (id, logic, rch))

let updatePackageNoReply (id : int) (logic : Logic<Package>) =
    let agent = getOrCreatePackageAgent id
    agent.Post (UpdateWithoutReply (id, logic))