module rec Audience

open System
open FSharpx.Control

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

                match loginResp.User with
                | AudUser u ->
                    let audFacade = AudienceFacade (server, loginResp.Token)
                    let! resp = audFacade.GetQuiz ()

                    match resp with
                    | Ok {Value = Ok quiz; ST = st} ->
                        let appSync = AppSyncFacade(u.AppSyncCfg)
                        appSync.Subscribe quizId  quiz.LT (ignore)
                    | Ok {Value = Result.Error txt} -> failwithf "Init agent error for audience %i %s" idx txt
                    | Result.Error exn -> failwithf "Init agent exception for audience  %i %s" idx exn.Message

                | _ -> printfn "ERROR: wrong user type"
            with
            | ex -> printfn "EXCEPTION for %i %s %A" idx ex.Message ex
        }
    ]
    |> Async.ParallelCatchWithThrottle 10
    |> Async.RunSynchronously
    |> ignore

    Console.WriteLine "Press ENTER key to stop test"
    Console.ReadLine () |> ignore