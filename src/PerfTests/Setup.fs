module Setup

open Shared
open Common

let setup opts (adminFacade:AdminFacade)  =

    randomNames "team"
    |> Seq.take opts.TeamsCount
    |> Array.ofSeq
    |> Array.Parallel.map (adminFacade.CreateTeam >> Async.RunSynchronously)
    |> ignore

    System.Threading.Thread.Sleep(1000)