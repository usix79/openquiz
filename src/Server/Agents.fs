module Agents

open System
open Newtonsoft.Json
open Serilog

open Common
open Env

module AR = AsyncResult

type PublisherCommand =
    | PublishResults of IDb * quizId : int * bucketName : string

let private uploadFile bucket (quiz:Domain.Quiz) results =
    let key = Bucket.getResultsKey quiz.Dsc.QuizId quiz.Dsc.ListenToken
    let body =
        JsonConvert.SerializeObject(results, Common.fableConverter)
        |> Text.UTF8Encoding.UTF8.GetBytes
    Bucket.uploadFile bucket key "application/json" body

let private publishResuts env bucket quizId =
    Data2.Quizzes.get env quizId
    |> AR.bind (fun quiz ->
        Data2.Teams.getAllInQuiz env (quiz.Dsc.QuizId)
        |> AR.bind (fun teams ->
            Domain.Results.results quiz teams
            |> uploadFile bucket quiz))
    |> Async.map ignore

let private publisherAgent = MailboxProcessor<PublisherCommand>.Start(fun inbox ->
    let rec loop() =
        let rec readAll msgs =
            async{
                match! inbox.TryReceive 0 with
                | Some msg ->
                    return!
                        if not <| List.contains msg msgs then msgs @ [msg] else msgs
                        |> readAll
                | None -> return msgs
            }

        async {
            try
                let! msg = inbox.Receive()
                let! msgs = readAll [msg]

                for msg in msgs do
                    match msg with
                    | PublishResults (env,quizId,bucket) ->
                        do! publishResuts env bucket quizId
            with
            | ex -> Log.Error ("{Op} {Exeption}", "publishAgentLoop", ex)

            return! loop()
        }
    loop()
)

let publish cmd =
    printfn "PUBLISH: %A" cmd
    publisherAgent.Post cmd