module Agents

open System
open Newtonsoft.Json

open Common

module AR = AsyncResult

let private uploadFile env (quiz:Domain.Quiz) results =
    let key = Aws.getResultsKey quiz.Dsc.QuizId quiz.Dsc.ListenToken
    let body =
        JsonConvert.SerializeObject(results, Common.fableConverter)
        |> Text.UTF8Encoding.UTF8.GetBytes
    Aws.uploadFile env key "application/json" body

let private publishResuts env quizId =
    Data2.Quizzes.get env quizId
    |> AR.bind (fun quiz ->
        Data2.Teams.getAllInQuiz env (quiz.Dsc.QuizId)
        |> AR.bind (fun teams ->
            Domain.Results.results quiz teams
            |> uploadFile env quiz))
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
                    | PublishResults quizId ->
                        do! publishResuts env quizId
            with
            | ex -> env.Logger.Error ("{Op} {Exeption}", "publishAgentLoop", ex)

            return! loop()
        }
    loop()
)