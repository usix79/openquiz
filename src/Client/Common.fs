module rec Common

open Elmish
open Fulma
open Fable.React
open Fable.Remoting.Client
open Browser
open Fable.Core.DynamicExtensions
open Fable.Import
open Fable.Core
open JsInterop
open Fable.SimpleJson
open Microsoft.FSharp.Reflection

open Shared

let noCmd model =
    model, Cmd.none

let batchCmd (cmd1:Cmd<'msg>) (model:'model, cmd2:Cmd<'msg>) =
    model, Cmd.batch [cmd1; cmd2]

let apiCmd proc arg ofSucccess ofExn model =
    model, Cmd.OfAsync.either proc (Infra.REQ arg) ofSucccess ofExn

let apiCmd' proc arg ofSucccess ofExn =
    Cmd.OfAsync.either proc (Infra.REQ arg) ofSucccess ofExn


let inline fromString<'a> (s:string) =
    match Reflection.FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(Reflection.FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

let urlForTeam quizId teamId token =
     sprintf "%s?who=team&quiz=%d&team=%d&token=%s" (Infra.locationFullPath ()) quizId teamId token

let urlForAdmin quizId token =
     sprintf "%s?who=admin&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let urlForReg quizId token =
     sprintf "%s?who=reg&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let urlForPub quizId token =
     sprintf "%s?who=pub&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let fileOnChange tag callback (ev:Types.Event) =
    let files : Types.FileList = !!ev.target.["files"]
    let file = files.[0]

    let reader = Browser.Dom.FileReader.Create()
    reader.onload <- fun _ ->
        let b : JS.ArrayBuffer = !!reader.result
        let a = JS.Constructors.Uint8Array.Create(buffer=b)
        let ba : byte[] = unbox a
        let t : string = !!file.["type"]
        callback {|Type=t; Body=ba; Tag=tag|}

    reader.readAsArrayBuffer file

type TRESP<'T, 'P> = {
    Tag : 'T
    Rsp : RESP<'P>
}

let taggedMsg msg tag =
    fun rsp ->
        msg {Tag = tag; Rsp = rsp}

let ofInt (value : string option) : int option =
    match value with
    | Some v ->
        match System.Int32.TryParse(v) with
        | true, i -> Some i
        | _ -> None
    | None -> None

let inline (|Err|_|) (msg:'msg) : string option =

    let pi = FSharpType.GetRecordFields typeof<RESP<_>> |> Array.tryFind (fun pi -> pi.Name = "Value")

    match pi with
    | Some pi' ->

        let msgType = msg.GetType()
        if Reflection.FSharpType.IsUnion msgType then
            let (_, objArray) = FSharpValue.GetUnionFields (msg, msgType)
            if objArray.Length = 1 then
                try
                    let res = FSharpValue.GetRecordField (objArray.[0], pi')

                    let (caseInfo, objArray) = FSharpValue.GetUnionFields (res, typedefof<Result<_,_>>)
                    if caseInfo.Name = "Error" then
                        let txt:string = unbox objArray.[0]
                        printfn "TXT: %s" txt
                        Some txt
                    else
                        None
                with
                | _ -> None
            else
                None
        else
            None
    | None -> None

module Infra =

    [<Emit("window.location.pathname")>]
    let virtualPath : string = jsNative

    [<Emit("window.location.search")>]
    let locationSearch : string = jsNative

    [<Emit("window.location.origin")>]
    let locationOrign : string = jsNative

    let locationFullPath () =
        locationOrign + virtualPath

    let redirect url =
        window.location.replace(url)

    let clearQueryString () =

        history.pushState(null, "", (window.location.href.Split([|'?'|]).[0]));


    let saveToSessionStorage key value =
        sessionStorage.setItem (key, Json.stringify value)

    let inline loadFromSessionStorage<'t>  key : 't option =
        let str = sessionStorage.getItem key

        match Json.tryParseAs<'t> str with
        | Ok data -> Some data
        | Error _ -> None

    let saveUser user serverTime =
        saveToSessionStorage "USER" user
        saveToSessionStorage "START" serverTime

    let clearUserAndRedirect url =
        sessionStorage.removeItem "USER"
        sessionStorage.removeItem "START"
        redirect url

    let currentQueryString () =
        let toKeyValuePair (segment: string) =
            match segment.Split([|'='|], 2) with
            | [| key; value |] -> Some (key, value)
            | _ -> None

        let parseParams (querystring: string) =
            if querystring.Length > 1 then
                querystring.Substring(1).Split('&')
                |> Seq.map toKeyValuePair
                |> Seq.choose id
                |> Map.ofSeq
            else Map.empty

        parseParams locationSearch

    let REQ<'Arg> (arg:'Arg) =
        {Token = ""; Arg = arg}

    type ApiFactory (switchToLoginAction: unit -> unit) =

        let o = sessionStorage.getItem "TOKEN"
        let mutable token = if isNull o then "" else o

        let o = sessionStorage.getItem "REFRESH_TOKEN"
        let mutable refreshToken = if isNull o then "" else o

        let securityApi =
            Remoting.createApi()
            |> Remoting.withBaseUrl "/"
            |> Remoting.withRouteBuilder (Infra.routeBuilder "")
            |> Remoting.buildProxy<ISecurityApi>

        let mainApi =
            Remoting.createApi()
            |> Remoting.withBaseUrl "/"
            |> Remoting.withRouteBuilder (Infra.routeBuilder "")
            |> Remoting.buildProxy<IMainApi>

        member x.RefreshTokenAndProceed<'Req, 'Resp> failedToken =
            async{
                if failedToken <> token then  // token has been already refreshed
                    return Some token
                else
                    let req = {Token = token; Arg = {|RefreshToken = refreshToken|}}
                    let! resp = securityApi.refreshToken req

                    match resp.Status, resp.Value with
                    | Executed, Ok res ->
                        x.UpdateTokens res.Token res.RefreshToken
                        return Some token
                    | _ ->
                        return None
            }
        member private x.Rpc (f : REQ<'Req> -> ARESP<'Resp>) req =
            async {
                let! r = f req |> Async.Catch
                match r with
                | Choice1Of2 resp -> return resp
                | Choice2Of2 ex -> return raise ex
            }

        member x.Wrap<'Req, 'Resp> (f : REQ<'Req> -> ARESP<'Resp>) req =
            async {
                let req = {req with Token = token}

                let! resp = x.Rpc f req

                let mutable realR = resp

                match resp.Status with
                | SecurityTokenInvalid -> switchToLoginAction()
                | SecurityTokenExpired ->
                    let! newToken = x.RefreshTokenAndProceed req.Token

                    match newToken with
                    | Some token ->
                        let! newResp = x.Rpc f {req with Token = token}

                        match newResp.Status with
                        | SecurityTokenInvalid
                        | SecurityTokenExpired -> switchToLoginAction()
                        | _ -> ()

                        realR <- newResp

                    | None -> switchToLoginAction()

                | _ -> ()

                match realR.Value with
                | Error txt when txt = Errors.SessionIsNotActive -> return failwith Errors.SessionIsNotActive
                | _ -> return realR
            }

        member x.UpdateTokens newToken newRefreshToken =
            token <- newToken
            refreshToken <- newRefreshToken

            sessionStorage.setItem ("TOKEN", token)
            sessionStorage.setItem ("REFRESH_TOKEN", refreshToken)

        member x.CreateSecurityApi () =
            {
                login = x.Wrap securityApi.login
                refreshToken = x.Wrap securityApi.refreshToken
            }

        member x.CreateMainApi () =
            {
                becomeProducer = x.Wrap mainApi.becomeProducer
                createQuiz = x.Wrap mainApi.createQuiz
                getProdQuizzes = x.Wrap mainApi.getProdQuizzes
                getProdQuizCard = x.Wrap mainApi.getProdQuizCard
                updateProdQuizCard = x.Wrap mainApi.updateProdQuizCard
                uploadFile = x.Wrap mainApi.uploadFile
                getPubModel = x.Wrap mainApi.getPubModel
                registerTeam = x.Wrap mainApi.registerTeam
                getProdPackages = x.Wrap mainApi.getProdPackages
                getProdPackageCard = x.Wrap mainApi.getProdPackageCard
                createPackage = x.Wrap mainApi.createPackage
                updateProdPackageCard = x.Wrap mainApi.updateProdPackageCard
            }

        member x.CreateAdminApi () =
            {
                login = x.Wrap securityApi.login
                refreshToken = x.Wrap securityApi.refreshToken
            }
