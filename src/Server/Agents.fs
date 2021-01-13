module Agents

open System
open Newtonsoft.Json
open Serilog

open Common
open Env

module AR = AsyncResult

let private uploadFile env bucket (quiz:Domain.Quiz) results =
    let key = Bucket.getResultsKey quiz.Dsc.QuizId quiz.Dsc.ListenToken
    let body =
        JsonConvert.SerializeObject(results, Common.fableConverter)
        |> Text.UTF8Encoding.UTF8.GetBytes
    Bucket.uploadFile env bucket key "application/json" body

let private publishResuts env bucket quizId =
    Data2.Quizzes.get env quizId
    |> AR.bind (fun quiz ->
        Data2.Teams.getAllInQuiz env (quiz.Dsc.QuizId)
        |> AR.bind (fun teams ->
            Domain.Results.results quiz teams
            |> uploadFile env bucket quiz))
    |> Async.map ignore

let publisherAgent env =
    MailboxProcessor<PublisherCommand>.Start(fun inbox ->
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
                    | PublishResults (quizId,bucket) ->
                        do! publishResuts env bucket quizId
            with
            | ex -> Log.Error ("{Op} {Exeption}", "publishAgentLoop", ex)

            return! loop()
        }
    loop()
)