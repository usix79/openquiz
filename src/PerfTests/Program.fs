open System

open Common
open Setup
open Teams
open Audience

let parseArgs argv =
    let defaultOptions = {Server = ""; Token = ""; QuizId = -1; Mode = None}

    let rec parseSetupOptions args optionsSoFar =
        match args with
        | [] -> optionsSoFar
        | "-tc" :: countTxt :: xs -> parseSetupOptions xs {optionsSoFar with TeamsCount = Int32.Parse(countTxt)}
        | _ -> failwithf "Wrong setup arguments %A" args

    let rec parseTeamsOptions args optionsSoFar : TeamsOptions=
        match args with
        | [] -> optionsSoFar
        | "-fst" :: id :: xs -> parseTeamsOptions xs {optionsSoFar with FirstTeamId = Int32.Parse(id)}
        | "-lst" :: id :: xs -> parseTeamsOptions xs {optionsSoFar with LastTeamId = Int32.Parse(id)}
        | _ -> failwithf "Wrong setup arguments %A" args

    let rec parseAudienceOptions args optionsSoFar : AudienceOptions=
        match args with
        | [] -> optionsSoFar
        | "-count" :: id :: xs -> parseAudienceOptions xs {optionsSoFar with AudienceCount = Int32.Parse(id)}
        | _ -> failwithf "Wrong setup arguments %A" args

    let rec parseCommandLine args optionsSoFar =
        match args with
        | [] -> optionsSoFar
        | "-srv" :: server :: xs -> parseCommandLine xs {optionsSoFar with Server = server}
        | "-token" :: token :: xs -> parseCommandLine xs {optionsSoFar with Token = token}
        | "-quiz" :: id :: xs -> parseCommandLine xs {optionsSoFar with QuizId = Int32.Parse(id)}
        | "setup" :: xs -> {optionsSoFar with Mode = Some (Setup (parseSetupOptions xs {TeamsCount = 0}))}
        | "teams" :: xs -> {optionsSoFar with Mode = Some (Teams (parseTeamsOptions xs {FirstTeamId = 1; LastTeamId = 1}))}
        | "audience" :: xs -> {optionsSoFar with Mode = Some (Audience (parseAudienceOptions xs {AudienceCount = 1}))}
        | _ -> failwithf "Wrong arguments %A" args

    parseCommandLine (List.ofSeq argv) defaultOptions

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn """Open Quiz Performanse Tests Utility
        USAGE:
            perftests.dll -srv <url> -quiz <quizId> -token <admin token> MODE
            Possible MODEs:
                setup -tc <teams count>
                teams -fst <startId> -lst <lastId>
                audience -count <audience count>
        """
    else
        let options = parseArgs argv
        printfn "Starting Open-Quiz Performanse Tests with such options: %A" options

        let securityFacade = SecurityFacade options.Server
        let req = Shared.AdminUser {|QuizId=options.QuizId; Token=options.Token|}
        let loginResp = securityFacade.Login req |> Async.RunSynchronously
        let adminFacade = AdminFacade (options.Server, loginResp.Token)

        match options.Mode with
        | Some (Setup opt) -> setup opt adminFacade
        | Some (Teams opt) -> teams options.Server options.QuizId opt securityFacade adminFacade
        | Some (Audience opt) -> audience options.Server options.QuizId opt securityFacade adminFacade
        | _ -> failwith "Not supported mode"
        ()

    0 // return an integer exit code
