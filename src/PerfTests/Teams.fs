module Teams

open System
open System.Threading.Tasks

open FSharpx.Control
open FSharp.SSEClient
open Fable.Remoting.Json
open Newtonsoft.Json
open AWS.AppSync.Client

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
                    let! token = securytyFacade.Login req

                    let teamFacade = TeamFacade (server, token)

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

                        let converter = FableJsonConverter()

                        let appSyncClient = AppSyncClient("https://7bhbyvhbfzezhj7jrxmnllg7ca.appsync-api.eu-central-1.amazonaws.com/graphql", AuthOptions(APIKey = "da2-z2u7n2jhhneejchbkfnipdungq"))
                        let guid = Guid.NewGuid()

                        let query = sprintf """
                              subscription pqm {
                                onQuizMessage(quizId: %i, token: \"%s\") {
                                  body
                                }
                              }
                            """
                        let opt = QueryOptions(Query = (query quizId quiz.LT), SubscriptionId = guid)

                        let onMessage = Action<obj>(fun obj -> printfn "MESSAGE: %A" obj)
                        let onError = Action<exn>(fun exn -> printfn "EXCEPTION: %A" exn)

                        let res = appSyncClient.CreateSubscriptionAsync<obj>(opt, onMessage, onError) |> Async.AwaitTask

                        ()

                        // printfn "sse initializing for team %i" team.TeamId

                        // let url = sprintf "%s%s" server (Infra.sseUrl quizId quiz.V quiz.LT)
                        // let! s = httpClient.GetStreamAsync (url) |> Async.AwaitTask

                        // printfn "sse established for team %i" team.TeamId

                        // Connection.Receive (fun () -> s)
                        // //let s = Http.RequestStream( url )
                        // //Connection.Receive (fun () -> s.ResponseStream)
                        // |> Observable.subscribe ( fun evt ->
                        //     match evt.EventName, evt.Data with
                        //     | Some "message", Some (SSEData json) ->
                        //         try
                        //             let evt = JsonConvert.DeserializeObject<QuizChangedEvent>(json, converter)
                        //             agent.Post (QuizChanged evt)
                        //         with
                        //         | ex -> printfn "EXCEPTION: %s" ex.Message
                        //     | _ -> ()
                        // ) |> ignore

                    | Ok {Value = Result.Error txt} -> failwithf "Init agent error for team %i %s" team.TeamId txt
                    | Result.Error exn -> failwithf "Init agent exception for team %i %s" team.TeamId exn.Message
                with
                | ex -> printfn "EXCEPTION: %s %A" ex.Message ex
            }
    )  |> Async.ParallelCatchWithThrottle 20
    |> Async.RunSynchronously
    |> ignore

    Console.WriteLine "Press ENTER key to stop test"
    Console.ReadLine () |> ignore