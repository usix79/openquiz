module OpenQuiz.Agents

open System
open Newtonsoft.Json

open Common
open Env

module AR = AsyncResult

let private uploadFile env (quiz:Domain.Quiz) results =
    let key = Aws.getResultsKey quiz.Dsc.QuizId quiz.Dsc.ResultsToken
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
            async {
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
    loop())


let answersAgent env   =
    MailboxProcessor<AnswersCommand>.Start(fun inbox ->
    let rec loop() =
        let rec readAll cmds =
            async {
                match! inbox.TryReceive 0 with
                | Some cmd -> return! readAll (cmd :: cmds)
                | None -> return List.rev cmds
            }

        async {
            try
                let! cmd = inbox.Receive()
                let! cmds = readAll [cmd]

                (env :> ILog).Logger.Information ("{Op} {Step} {Count}", "AnswersAgent", "Registering", (cmds.Length))

                let! results =
                    cmds
                    |> List.choose (function RegisterAnswer logic -> Some logic)
                    |> Async.ParallelThrottle 16

                for result in results do
                    match result with
                    | Error (quizId,teamId,txt) -> env.Logger.Error("{Op} {Quiz} {Team} {Error}", "AnswersAgent", quizId, teamId, txt)
                    | _ -> ()

                (env :> ILog).Logger.Information ("{Op} {Step}", "AnswersAgent", "Done")

            with
            | ex -> (env :> ILog).Logger.Error ("{Op} {Exeption}", "AnswersAgent", ex)

            return! loop()
        }
    loop())