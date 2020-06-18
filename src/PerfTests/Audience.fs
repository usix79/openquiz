module rec Audience

open System
open System.Threading.Tasks

open FSharpx.Control
open FSharp.SSEClient
open Fable.Remoting.Json
open Newtonsoft.Json

open Shared
open Common

let audience server quizId opts (securytyFacade:SecurityFacade)  (adminFacade: AdminFacade) =
    let listenToken = adminFacade.GetListenToken() |> Async.RunSynchronously

    let hch = new Net.Http.HttpClientHandler();
    hch.Proxy <- null;
    hch.UseProxy <- false;

    let httpClient = new Net.Http.HttpClient(hch)
    httpClient.Timeout <- TimeSpan.FromMinutes(10.0)

    Net.ServicePointManager.DefaultConnectionLimit <- 1024
    Net.ServicePointManager.ReusePort <- true

    [ for idx in 0 .. opts.AudienceCount ->
        async {
            try
                printfn "initializing audience %i" idx

                let req = Shared.AudUser {|QuizId=quizId; Token=listenToken|}
                let! loginResp = securytyFacade.Login req

                let audFacade = AudienceFacade (server, loginResp.Token)
                let! resp = audFacade.GetQuiz ()

                match resp with
                | Ok {Value = Ok quiz; ST = st} ->

                    let converter = FableJsonConverter()

                    printfn "sse initializing for audience  %i" idx

                    let url = sprintf "%s%s" server (Infra.sseUrl quizId quiz.V quiz.LT)
                    let! s = httpClient.GetStreamAsync (url) |> Async.AwaitTask

                    printfn "sse established for audience %i" idx

                    Connection.Receive (fun () -> s)
                    //let s = Http.RequestStream( url )
                    //Connection.Receive (fun () -> s.ResponseStream)
                    |> Observable.subscribe ( fun evt ->
                        match evt.EventName, evt.Data with
                        | Some "message", Some (SSEData json) ->
                            try
                                let evt = JsonConvert.DeserializeObject<QuizChangedEvent>(json, converter)
                                printfn "%i get evt v%i" idx evt.Id
                            with
                            | ex -> printfn "EXCEPTION: %s" ex.Message
                        | _ -> ()
                    ) |> ignore

                | Ok {Value = Result.Error txt} -> failwithf "Init agent error for audience %i %s" idx txt
                | Result.Error exn -> failwithf "Init agent exception for audience  %i %s" idx exn.Message

            with
            | ex -> printfn "EXCEPTION for %i %s %A" idx ex.Message ex
        }
    ]
    |> Async.ParallelCatchWithThrottle 10
    |> Async.RunSynchronously
    |> ignore


    Console.WriteLine "Press ENTER key to stop test"
    Console.ReadLine () |> ignore