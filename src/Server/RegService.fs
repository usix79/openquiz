module rec OpenQuiz.RegService

open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Serilog

open OpenQuiz.Shared
open Common
open Env
open Presenter

let api env (context:HttpContext) : IRegApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret =  (env :> ICfg).Configurer.JwtSecret

    let ex proc f =
        let ff f = (fun (quizIdStr:string) req ->
            match tryParseInt32 quizIdStr with
            | Some quizId ->
                Data2.Quizzes.getDescriptor env quizId
                |> AsyncResult.bind (fun quiz -> f quiz req)
            | None ->
                env.Logger.Error ("{Api} {Error} {Quiz}", "reg", "Wrong quiz Id", quizIdStr)
                Error "Wrong Quiz Id"
                |> AsyncResult.fromResult
        )

        SecurityService.exec logger proc <| SecurityService.authorizePrivateReg secret (ff f)

    let api : IRegApi = {
        getRecord = ex "getRecord" getRecord
    }

    api

let getRecord quiz _ =
    quiz |> Reg.quizRecord |> AsyncResult.retn