module rec RegService

open System
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open Shared
open Common
open Presenter

let api (context:HttpContext) : IRegApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let ex proc f =
        let ff f = (fun (quizIdStr:string) req ->
            match Int32.TryParse quizIdStr with
            | true, quizId ->
                match Data.Quizzes.getDescriptor quizId with
                | Some quiz -> f quiz req
                | None ->
                    Log.Error ("{Api} {Error} {Quiz}", "admin", "Quiz Not Found", quizId)
                    Error "Quiz not found"
            | _ ->
                Log.Error ("{Api} {Error} {Quiz}", "admin", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
        )

        SecurityService.exec logger proc <| SecurityService.authorizePrivateReg secret (ff f)

    let api : IRegApi = {
        getRecord = ex "getRecord" getRecord
    }

    api

let getRecord quiz _ =
    quiz |> Reg.quizRecord |> Ok