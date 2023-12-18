module Setup

open OpenQuiz.Shared
open Common

let setup opts (adminFacade:AdminFacade)  =

    randomNames "team"
    |> Seq.take opts.TeamsCount
    |> List.ofSeq
    |> adminFacade.CreateTeamBatch
    |> Async.RunSynchronously

    System.Threading.Thread.Sleep(1000)