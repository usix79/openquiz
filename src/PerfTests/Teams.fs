module Teams

open System
open System.Threading.Tasks
open FSharpx.Control

open Shared
open Common

type Model = {
    QuizStatus : QuizStatus
    CurrentTour : TourCard option
    CurrentTourIndex : int
    IsAnswerSent : bool
    TimeDiff: TimeSpan
}

type Msg =
    | QuizChanged of QuizChangedEvent
    | AnswerResponse of RESP<unit>
    | CountdownTick of {|TourIdx: int|}
    | Exn of string

let applyChanges sendAnswer (inbox : MailboxProcessor<Msg>) (model : Model) =
    match model.CurrentTour with
    | Some t ->
        match t.TS with
        | _ when model.CurrentTourIndex <> t.Idx && not model.IsAnswerSent ->
            sendAnswer model.CurrentTourIndex "AGA, but missed countdown"
            {model with IsAnswerSent = false; CurrentTourIndex = t.Idx}
        | _ when model.CurrentTourIndex <> t.Idx ->
            {model with IsAnswerSent = false; CurrentTourIndex = t.Idx}
        | Countdown when model.QuizStatus = Live && t.IsCountdownActive (serverTime model.TimeDiff) ->
            let secondsLeft = t.SecondsLeft (serverTime model.TimeDiff)
            Task.Delay(secondsLeft * 1000).ContinueWith (fun _ -> inbox.Post (CountdownTick {|TourIdx = t.Idx|})) |> ignore
            model
        | Countdown | Settled when not model.IsAnswerSent ->
            sendAnswer model.CurrentTourIndex "AGA"
            {model with IsAnswerSent = true}
        | _ -> model
    | None -> model

let startAgent (team:AdminModels.TeamRecord) (teamFacade:TeamFacade) (initModel:Model) =

    printfn "Sarting agent for teamId = %i" team.TeamId

    MailboxProcessor.Start (fun inbox ->

        let sendAnswer (tourIdx:int) (answer:string) =
            printfn "team %i sending answer %i" team.TeamId tourIdx
            apiCall inbox (teamFacade.Answer {TourIdx = tourIdx; QwIdx = 0}) (answer,false) AnswerResponse Exn

        let rec loop model =
            async {

                let! msg = inbox.Receive()
                printfn "team %i msg=%A" team.TeamId msg

                let newModel =
                    match msg with
                    | QuizChanged evt ->
                        {model with QuizStatus = evt.QS; CurrentTour = evt.T} |> applyChanges sendAnswer inbox
                    | CountdownTick v when model.CurrentTour.IsSome && v.TourIdx = model.CurrentTour.Value.Idx ->
                        model |> applyChanges sendAnswer inbox
                    | AnswerResponse _ -> model
                    | Exn ex ->
                        printfn "EXCEPTION %A" ex
                        model
                    | _ -> model

                return! loop newModel
            }

        loop initModel
    )

let teams server quizId opts (securytyFacade:SecurityFacade) (adminFacade: AdminFacade)  =
    let teams = adminFacade.GetTeams () |> Async.RunSynchronously

    let hch = new Net.Http.HttpClientHandler();
    hch.Proxy <- null;
    hch.UseProxy <- false;

    let httpClient = new Net.Http.HttpClient(hch)
    httpClient.Timeout <- TimeSpan.FromMinutes(10.0)

    Net.ServicePointManager.DefaultConnectionLimit <- 1024
    Net.ServicePointManager.ReusePort <- true

    teams
    |> List.filter (fun team -> team.TeamId >= opts.FirstTeamId && team.TeamId <= opts.LastTeamId)
    |> List.map (
        fun team ->
            async {
                try
                    printfn "initializing team %i" team.TeamId

                    let req = Shared.TeamUser {|QuizId=quizId; TeamId = team.TeamId; Token=team.EntryToken|}
                    let! loginResp = securytyFacade.Login req

                    match loginResp.User with
                    | TeamUser u ->

                        let teamFacade = TeamFacade (server, loginResp.Token)

                        let! _ = teamFacade.TakeActiveSession ()

                        //do! Task.Delay(1000) |> Async.AwaitTask // wait for session to be applyed

                        let! resp = teamFacade.GetState ()

                        match resp with
                        | Ok {Value = Ok quiz; ST = serverTime} ->
                            let initModel = {
                                QuizStatus = quiz.QS
                                CurrentTour = quiz.TC
                                CurrentTourIndex = if quiz.TC.IsSome then quiz.TC.Value.Idx else 0;
                                TimeDiff = timeDiff serverTime;
                                IsAnswerSent = quiz.Aw.Count > 0}

                            let agent = startAgent team teamFacade initModel

                            let appSync = AppSyncFacade(u.AppSyncCfg)
                            appSync.Subscribe quizId  quiz.LT (QuizChanged >> agent.Post)

                        | Ok {Value = Result.Error txt} -> failwithf "Init agent error for team %i %s" team.TeamId txt
                        | Result.Error exn -> failwithf "Init agent exception for team %i %s" team.TeamId exn.Message
                    | _ -> printfn "ERROR: wrong user type"
                with
                | ex -> printfn "EXCEPTION: %s %A" ex.Message ex
            }
    )  |> Async.ParallelCatchWithThrottle 20
    |> Async.RunSynchronously
    |> ignore

    Console.WriteLine "Press ENTER key to stop test"
    Console.ReadLine () |> ignore