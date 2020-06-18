module Common

open System
open Fable.Remoting.DotnetClient

open Shared

type SetupOptions = {
    TeamsCount : int
}

type TeamsOptions = {
    FirstTeamId : int
    LastTeamId : int
}

type AudienceOptions = {
    AudienceCount : int
}

type TestModeOptions=
    | Setup of SetupOptions
    | Teams of TeamsOptions
    | Audience of AudienceOptions

type TestOptions = {
    Server : string
    QuizId : int
    Token : string
    Mode : TestModeOptions option }

let nextIdGenerator () =
    let mutable id = 0
    fun () -> Threading.Interlocked.Increment(&id)

let gen = nextIdGenerator()

let randomNames category =
    Seq.initInfinite (fun _ -> sprintf "%s-test-%s-%i" category (DateTime.UtcNow.ToString("dd.MM.yy HH:mm:ss")) (gen()))
    |> Seq.distinct

let serverRequest<'Arg> token (arg:'Arg) =
    {Token = token; Arg = arg}

let timeDiff serverTime: TimeSpan = (serverTime - DateTime.UtcNow)

let compareDates (d1:DateTime) (d2:DateTime) =
    ((d1 - d2):TimeSpan).TotalMilliseconds

let serverTime timeDiff =
    System.DateTime.UtcNow.Add timeDiff

let createProxy<'TProxy> server =
    let routes = sprintf "%s/app/api/%s/%s" server
    Proxy.create<'TProxy> routes

let apiCall
    (inbox : MailboxProcessor<'msg>)
    (f:'arg -> Async<Result<RESP<'res>,exn>>)
    (a:'arg)
    (ofSuccess : RESP<'res> -> 'msg) (ofError: string -> 'msg) =
    async {
        let! resp = f a

        match resp with
        | Ok r -> inbox.Post <| ofSuccess r
        | Error ex -> inbox.Post <| ofError ex.Message
    } |> Async.Start

type SecurityFacade (server) =
    let proxy = createProxy server

    member x.Login req =
        async {
            let req = serverRequest "" req
            let! resp = proxy.call <@ fun server -> server.login req @>

            return
                match resp.Value with
                | Ok v -> v
                | Error txt -> failwithf "LOGIN ERROR: %s" txt
        }

type AdminFacade (server, token) =
    let mutable token = token
    let proxy = createProxy<IAdminApi> server

    member x.CreateTeam teamName =
        async {
            let req = serverRequest token {|TeamName = teamName|}
            let! resp = proxy.call <@ fun server -> server.createTeam req @>
            return
                match resp.Value with
                | Ok res -> res
                | Error txt -> failwithf "CreateTeam error %s" txt
        }

    member x.GetTeams () =
        async {
            let req = serverRequest token ()
            let! resp = proxy.call <@ fun server -> server.getTeams req @>

            return
                match resp.Value with
                | Ok v -> v
                | Error txt -> failwithf "GetTeams ERROR: %s" txt
        }

    member x.GetListenToken () =
        async {
            let req = serverRequest token ()
            let! resp = proxy.call <@ fun server -> server.getListenToken req @>

            return
                match resp.Value with
                | Ok v -> v
                | Error txt -> failwithf "GetListenToken ERROR: %s" txt
        }

type TeamFacade (server, token) =

    let mutable token = token
    let proxy = createProxy<ITeamApi> server

    member x.TakeActiveSession () =
        async {
            let req = serverRequest token ()
            return! proxy.callSafely <@ fun server -> server.takeActiveSession req @>
        }

    member x.GetState () =
        async {
            let req = serverRequest token ()
            return! proxy.callSafely <@ fun server -> server.getState req @>
        }

    member x.Answer qwKey answer =
        async {
            let req = serverRequest token ([qwKey, answer] |> Map.ofList)
            return! proxy.callSafely <@ fun server -> server.answers req @>
        }

type AudienceFacade (server, token) =

    let mutable token = token
    let proxy = createProxy<IAudApi> server

    member x.GetQuiz () =
        async {
            let req = serverRequest token ()
            return! proxy.callSafely <@ fun server -> server.getQuiz req @>
        }