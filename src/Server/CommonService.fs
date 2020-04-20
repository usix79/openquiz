module CommonService

open Serilog

open Common
open Domain

type Creator<'TKey, 'T> = 'TKey -> Result<'T, string>
type Logic<'T> = 'T -> Result<'T, string>
type Loader<'TKey, 'T> = 'TKey -> 'T option
type Saver<'T> = 'T -> 'T
type Broker<'T> = 'T -> unit

module DomainEvents =

    let mutable private subscribers = []

    let subscribeOnQuizChanges (f: Shared.QuizChangedEvent -> unit) =
        subscribers <- f :: subscribers

    let quizChanged (quiz : Quiz) =
        let evt = Presenter.quizChangeEvent quiz

        subscribers
        |> List.iter (fun f -> f evt)

type private UpdateCommand<'TKey, 'T> =
    | Update of 'TKey * Logic<'T>
    | UpdateAndReply of 'TKey * Logic<'T> * AsyncReplyChannel<Result<'T, string>>

let private createAgent<'TKey,'T> (loader : Loader<'TKey,'T>) (saver : Saver<'T>) (broker : Broker<'T>) =
    MailboxProcessor<UpdateCommand<'TKey,'T>>.Start (fun inbox ->

        let update key logic =
            try
                match loader key with
                | Some entity ->
                    match logic entity with
                    | Ok entity ->
                        let savedEntity = saver entity
                        broker savedEntity
                        savedEntity |> Ok
                    | Error txt -> Error txt
                | None -> Error (sprintf "Not Found %A" key)
            with
            | ex ->
                Log.Error ("{Op} {Exeption}", "updateAgent", ex)
                Error ex.Message

        let rec loop () =
            async {
                try
                    let! msg = inbox.Receive()
                    match msg with
                    | Update (key,logic) -> update key logic |> ignore
                    | UpdateAndReply (key,logic, rch) -> update key logic |> rch.Reply
                with
                | ex -> Log.Error ("{Op} {Exeption}", "updateAgentMainLoop!!!", ex)

                return! loop()
            }
        loop())


let private _teamsLockObj = System.Object()
let mutable private _teamAgents : Map<TeamKey, MailboxProcessor<UpdateCommand<TeamKey,Team>>> = Map.empty

let teamLoader (key:TeamKey) = Data.Teams.get key.QuizId key.TeamId
let private teamSaver team = Data.Teams.update team

let private getOrCreateTeamAgent key =
    let trans () =
        match Map.tryFind key _teamAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent teamLoader teamSaver ignore
            _teamAgents <- _teamAgents.Add (key, agent)
            agent

    lock _teamsLockObj trans

let createTeam quizId (creator : Creator<TeamKey,Team>) : Result<Domain.Team,string> =

    let trans () =
        let teamId = (Data.Teams.getMaxId quizId) + 1

        match creator {QuizId = quizId; TeamId = teamId} with
        | Ok t -> Data.Teams.update t |> Ok
        | Error txt -> Error txt

    lock _teamsLockObj trans

let updateTeam (key : TeamKey) (logic : Logic<Team>) : Result<Domain.Team,string> =
    let agent = getOrCreateTeamAgent key
    agent.PostAndReply (fun rch -> UpdateAndReply (key, logic, rch))

let updateTeamNoReply (key : TeamKey) (logic : Logic<Team>) =
    let agent = getOrCreateTeamAgent key
    agent.Post (Update (key, logic))

let private _quizzesLockObj = System.Object()
let mutable private _quizAgents : Map<int, MailboxProcessor<UpdateCommand<int,Quiz>>> = Map.empty

let  quizLoader (quizId : int) = Data.Quizzes.get quizId
let private quizSaver quiz = Data.Quizzes.update quiz
let private quizBroker quiz = DomainEvents.quizChanged quiz

let private getOrCreateQuizAgent (quizId:int) =
    let trans () =
        match Map.tryFind quizId _quizAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent quizLoader quizSaver quizBroker
            _quizAgents <- _quizAgents.Add (quizId, agent)
            agent

    lock _teamsLockObj trans

let createQuiz (creator : Creator<int,Quiz>) : Result<Domain.Quiz,string> =
    let trans () =
        let quizId = Data.Quizzes.getMaxId () + 1

        match creator quizId with
        | Ok x -> Data.Quizzes.update x |> Ok
        | Error txt -> Error txt

    lock _teamsLockObj trans

let updateQuiz (quizId : int) (logic : Logic<Quiz>) : Result<Domain.Quiz,string> =
    let agent = getOrCreateQuizAgent quizId
    agent.PostAndReply (fun rch -> UpdateAndReply (quizId, logic, rch))

let updateQuizNoReply (quizId : int) (logic : Logic<Quiz>) =
    let agent = getOrCreateQuizAgent quizId
    agent.Post (Update (quizId, logic))

let  packageLoader (id : int) = Data.Packages.get id

let private _expertsLockObj = System.Object()
let mutable private _expertAgents : Map<string, MailboxProcessor<UpdateCommand<string,Expert>>> = Map.empty

let expertLoader (sub:string) = Data.Experts.get sub
let private expertSaver expert = Data.Experts.update expert

let private getOrCreateExpertAgent key =
    let trans () =
        match Map.tryFind key _expertAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent expertLoader expertSaver ignore
            _expertAgents <- _expertAgents.Add (key, agent)
            agent

    lock _teamsLockObj trans

let updateExpert (sub : string) (logic : Logic<Expert>) : Result<Domain.Expert,string> =
    let agent = getOrCreateExpertAgent sub
    agent.PostAndReply (fun rch -> UpdateAndReply (sub, logic, rch))

let updateExpertNoReply (sub : string) (logic : Logic<Expert>) =
    let agent = getOrCreateExpertAgent sub
    agent.Post (Update (sub, logic))
