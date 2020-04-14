module CommonService

open Serilog

open Common
open Domain

type Creator<'TKey, 'T> = 'TKey -> Result<'T, string>
type Logic<'T> = 'T -> Result<'T, string>
type Loader<'TKey, 'T> = 'TKey -> 'T option
type Saver<'T> = 'T -> 'T


type private UpdateCommand<'TKey, 'T> =
    | Update of 'TKey * Logic<'T>
    | UpdateAndReply of 'TKey * Logic<'T> * AsyncReplyChannel<Result<'T, string>>

let private createAgent<'TKey,'T> (loader : Loader<'TKey,'T>) (saver : Saver<'T>) =
    MailboxProcessor<UpdateCommand<'TKey,'T>>.Start (fun inbox ->

        let update key logic =
            try
                match loader key with
                | Some entity ->
                    match logic entity with
                    | Ok entity -> saver entity |> Ok
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

let private teamLoader (key:TeamKey) = Data.Teams.get key.QuizId key.TeamId
let private teamSaver team = Data.Teams.update team

let private getOrCreateTeamAgent key =
    let trans () =
        match Map.tryFind key _teamAgents with
        | Some agent -> agent
        | None ->
            let agent = createAgent teamLoader teamSaver
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