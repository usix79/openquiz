module rec Common

open Elmish
open Browser
open Fable.Core
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop
open Fable.Import
open Fable.React
open Fable.Remoting.Client
open Fable.SimpleJson
open Microsoft.FSharp.Reflection

open OpenQuiz.Shared

let timeDiff serverTime : System.TimeSpan = (serverTime - System.DateTime.UtcNow)

let serverTime timeDiff = System.DateTime.UtcNow.Add timeDiff

let compareDates (d1: System.DateTime) (d2: System.DateTime) =
    ((d1 - d2): System.TimeSpan).TotalMilliseconds

let noCmd model = model, Cmd.none

let batchCmd (cmd1: Cmd<'msg>) (model: 'model, cmd2: Cmd<'msg>) = model, Cmd.batch [ cmd1; cmd2 ]

let apiCmd proc arg ofSucccess ofExn model =
    model, Cmd.OfAsync.either proc (Infra.REQ arg) ofSucccess ofExn

let apiCmd' proc arg ofSucccess ofExn =
    Cmd.OfAsync.either proc (Infra.REQ arg) ofSucccess ofExn

let timeoutCmd msg timeout =
    let mutable isTickScheduled = false

    let sub dispatch =
        if not isTickScheduled then
            isTickScheduled <- true

            window.setTimeout (
                (fun _ ->
                    isTickScheduled <- false
                    dispatch msg),
                timeout
            )
            |> ignore

    Cmd.ofEffect sub

let uploadFileToS3Cmd (getUrlMethod: GetUrlMethod) cat (file: Types.File) onSuccess onError =
    let action () =
        async {
            let! resp = getUrlMethod { Token = ""; Arg = {| Cat = cat |} }

            match resp with
            | { Value = Ok res } ->
                let! _ = uploadFileToS3 file res.Url
                return res.BucketKey
            | { Value = Error txt } -> return raise (exn txt)
        }

    Cmd.OfAsync.either action () onSuccess onError

let mediaTypeFromMiME (mime: string) =
    printfn "MIME: %s" mime
    let mime = mime.ToLower()

    if mime.StartsWith("audio") then Audio
    else if mime.StartsWith("video") then Video
    else Picture

let inline fromString<'a> (s: string) =
    match
        Reflection.FSharpType.GetUnionCases typeof<'a>
        |> Array.filter (fun case -> case.Name = s)
    with
    | [| case |] -> Some(Reflection.FSharpValue.MakeUnion(case, [||]) :?> 'a)
    | _ -> None

let urlForTeam quizId teamId token =
    sprintf "%s?who=team&quiz=%d&team=%d&token=%s" (Infra.locationFullPath ()) quizId teamId token

let urlForAdmin quizId token =
    sprintf "%s?who=admin&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let urlForReg quizId token =
    sprintf "%s?who=reg&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let urlForAud quizId token =
    sprintf "%s?who=aud&quiz=%d&token=%s" (Infra.locationFullPath ()) quizId token

let urlForResults quizId quizName quizImgKey resultsToken mediaHost =
    sprintf
        "/results.html?who=res&quiz=%d&quizName=%s&quizImg=%s&token=%s&url=%s"
        quizId
        quizName
        quizImgKey
        resultsToken
        mediaHost

let urlForResultsIframe quizId resultsToken teamId mediaHost =
    let url =
        sprintf "/results.html?who=emb&quiz=%d&token=%s&url=%s" quizId resultsToken mediaHost

    match teamId with
    | Some id -> url + (sprintf "&teamId=%d" id)
    | None -> url

let splitByLines (txt: string) =
    [ for l in txt.Split([| '\n' |]) do
          str l
          br [] ]

let fileOnChangeAsText tag callback (ev: Types.Event) =
    let files: Types.FileList = !!ev.target["files"]
    let file = files[0]

    let reader = Browser.Dom.FileReader.Create()

    reader.onload <-
        fun _ ->
            callback
                {| Name = unbox<string> file.name
                   Body = unbox<string> reader.result
                   Tag = tag |}

    reader.readAsText file

let fileOnChangeForS3 tag callback (ev: Types.Event) =
    let files: Types.FileList = !!ev.target["files"]
    callback {| File = files[0]; Tag = tag |}

let uploadFileToS3 (file: Types.File) presignedUrl =
    Async.FromContinuations
    <| fun (resolve, _, _) ->

        let xhr = XMLHttpRequest.Create()
        xhr.``open`` ("PUT", presignedUrl)
        //xhr.setRequestHeader("Content-Type", !!file.["type"])
        //printfn "Content-Type: %s" !!file.["type"]
        xhr.onreadystatechange <-
            fun _ ->
                match xhr.readyState with
                | Types.ReadyState.Done when xhr.status = 200 -> resolve (xhr.responseText)
                | Types.ReadyState.Done -> raise <| exn xhr.responseText
                | _ -> ignore ()

        xhr.send (file)

let download (fileName: string) (contentType: string) (content: string) =
    let anchor = document.createElement "a"
    let encodedContent = $"data:{contentType};charset=utf-8,{content}" |> JS.encodeURI
    anchor.setAttribute ("href", encodedContent)
    anchor.setAttribute ("download", fileName)
    anchor.click ()

type TRESP<'T, 'P> = { Tag: 'T; Rsp: RESP<'P> }

let taggedMsg msg tag = fun rsp -> msg { Tag = tag; Rsp = rsp }

let inline saveToSessionStorage<'t> key (value: 't) =
    sessionStorage.setItem (key, Json.serialize<'t> value)

let inline loadFromSessionStorage<'t> key : 't option =
    let str = sessionStorage.getItem key

    match Json.tryParseAs<'t> str with
    | Ok data -> Some data
    | Error _ -> None

let ofInt (value: string option) : int option =
    match value with
    | Some v ->
        match System.Int32.TryParse(v) with
        | true, i -> Some i
        | _ -> None
    | None -> None

let ofDecimal (value: string option) : decimal option =
    match value with
    | Some v ->
        match System.Decimal.TryParse(v) with
        | true, i -> Some i
        | _ -> None
    | None -> None

let trimEnd n suffix (str: string) =
    if str.Length < n then str else str.Substring(0, n) + suffix

let trimMiddle n suffix (str: string) =
    if str.Length < n then
        str
    else
        str.Substring(0, n / 2) + suffix + str.Substring(str.Length - (n / 2), n / 2)

let inline (|Err|_|) (msg: 'msg) : string option =

    let pi =
        FSharpType.GetRecordFields typeof<RESP<_>>
        |> Array.tryFind (fun pi -> pi.Name = "Value")

    match pi with
    | Some pi' ->

        let msgType = msg.GetType()

        if Reflection.FSharpType.IsUnion msgType then
            let (_, objArray) = FSharpValue.GetUnionFields(msg, msgType)

            if objArray.Length = 1 then
                try
                    let res = FSharpValue.GetRecordField(objArray.[0], pi')

                    let (caseInfo, objArray) = FSharpValue.GetUnionFields(res, typedefof<Result<_, _>>)

                    if caseInfo.Name = "Error" then
                        let txt: string = unbox objArray.[0]
                        Some txt
                    else
                        None
                with _ ->
                    None
            else
                None
        else
            None
    | None -> None

type DeleteForm =
    { ConfirmText: string
      FormError: string
      IsSending: bool }

    static member CreateEmpty() =
        { ConfirmText = ""
          FormError = ""
          IsSending = false }

    static member Toggle(form: DeleteForm option) =
        match form with
        | Some _ -> None
        | None -> DeleteForm.CreateEmpty() |> Some

module Infra =

    [<Emit("window.location.href")>]
    let locationHref: string = jsNative

    [<Emit("window.location.hash")>]
    let locationHash: string = jsNative

    [<Emit("window.location.pathname")>]
    let virtualPath: string = jsNative

    [<Emit("window.location.search")>]
    let locationSearch: string = jsNative

    [<Emit("window.location.origin")>]
    let locationOrign: string = jsNative

    [<Import("play_safe", "./infra.js")>]
    let play (fileName: string) = jsNative

    [<Emit("window.navigator.languages")>]
    let languages () : string array = jsNative

    let locationFullPath () = locationOrign + virtualPath

    let getPreferableLangugage () =
        let tryLang (lang: string) =
            let lang = lang.ToLowerInvariant()

            if lang.StartsWith("en") then Some L10n.English
            else if lang.StartsWith("ru") then Some L10n.Russian
            else if lang.StartsWith("uk") then Some L10n.Ukrainian
            else if lang.StartsWith("az") then Some L10n.Azerbaijanian
            else if lang.StartsWith("uz") then Some L10n.Uzbek
            else None

        let rec check (langs: string list) =
            match langs with
            | head :: tail ->
                match tryLang head with
                | Some lang -> lang
                | None -> check tail
            | [] -> L10n.English

        languages () |> List.ofArray |> check

    let getUrlHashParts () =
        let hash = window.location.hash

        if System.String.IsNullOrEmpty hash then
            ("", "")
        else
            let idxOfCol = hash.IndexOf(':')

            if idxOfCol = -1 then
                (hash, "")
            else
                (hash.Substring(0, idxOfCol), hash.Substring(idxOfCol))

    let urlWithNewHash hash =
        let (hashTag, _) = getUrlHashParts ()

        if hashTag <> hash then
            (window.location.href).Replace(window.location.hash, hash)
        else
            window.location.href

    let urlWithNewHashQuery query =
        let (hashTag, _) = getUrlHashParts ()

        if hashTag <> "" then
            (window.location.href).Replace(window.location.hash, (hashTag + query))
        else
            window.location.href

    let redirect url = window.location.replace (url)

    let clearQueryString () =

        history.pushState (null, "", (window.location.href.Split([| '?' |]).[0]))

    let inline saveToSessionStorage<'t> key (value: 't) =
        sessionStorage.setItem (key, Json.serialize<'t> value)

    let inline loadFromSessionStorage<'t> key : 't option =
        let str = sessionStorage.getItem key

        match Json.tryParseAs<'t> str with
        | Ok data -> Some data
        | Error _ -> None

    let inline saveToLocalStorage<'t> key (value: 't) =
        localStorage.setItem (key, Json.serialize<'t> value)

    let inline loadFromLocalStorage<'t> key : 't option =
        let str = localStorage.getItem key

        match Json.tryParseAs<'t> str with
        | Ok data -> Some data
        | Error _ -> None

    let removeFromLocalStorage key = localStorage.removeItem key

    let saveUser (user: User) = saveToSessionStorage "USER" user

    let loadUser () = loadFromSessionStorage<User> "USER"

    let saveSettings (settings: Settings) =
        saveToSessionStorage "SETTINGS" settings

    let loadSettings () =
        loadFromSessionStorage<Settings> "SETTINGS"

    let clearUserAndSettingsAndRedirect url =
        sessionStorage.removeItem "USER"
        sessionStorage.removeItem "SETTINGS"
        redirect url

    let currentQueryString () =
        let toKeyValuePair (segment: string) =
            match segment.Split([| '=' |], 2) with
            | [| key; value |] -> Some(key, value)
            | _ -> None

        let parseParams (querystring: string) =
            if querystring.Length > 1 then
                querystring.Substring(1).Split('&')
                |> Seq.map toKeyValuePair
                |> Seq.choose id
                |> Map.ofSeq
            else
                Map.empty

        parseParams locationSearch

    let REQ<'Arg> (arg: 'Arg) = { Token = ""; Arg = arg }

    let inline createApi<'I> () : 'I =
        Remoting.createApi ()
        |> Remoting.withBaseUrl "/"
        |> Remoting.withRouteBuilder (Infra.routeBuilder "")
        |> Remoting.buildProxy<'I>

    type ApiFactory(switchToLoginAction: unit -> unit) =

        let o = sessionStorage.getItem "TOKEN"
        let mutable token = if isNull o then "" else o

        let o = sessionStorage.getItem "REFRESH_TOKEN"
        let mutable refreshToken = if isNull o then "" else o

        let securityApi = createApi<ISecurityApi> ()
        let mainApi = createApi<IMainApi> ()
        let adminApi = createApi<IAdminApi> ()
        let teamApi = createApi<ITeamApi> ()
        let regApi = createApi<IRegApi> ()
        let audApi = createApi<IAudApi> ()

        member x.RefreshTokenAndProceed<'Req, 'Resp> failedToken =
            async {
                if failedToken <> token then // token has been already refreshed
                    return Some token
                else
                    let req =
                        { Token = token
                          Arg = {| RefreshToken = refreshToken |} }

                    let! resp = securityApi.refreshToken req

                    match resp.Status, resp.Value with
                    | Executed, Ok res ->
                        x.UpdateTokens res.Token res.RefreshToken
                        return Some token
                    | _ -> return None
            }

        member private x.Rpc (f: REQ<'Req> -> ARESP<'Resp>) req =
            async {
                let! r = f req |> Async.Catch

                match r with
                | Choice1Of2 resp -> return resp
                | Choice2Of2 ex -> return raise ex
            }

        member x.Wrap<'Req, 'Resp> (f: REQ<'Req> -> ARESP<'Resp>) req =
            async {
                let req = { req with Token = token }

                let! resp = x.Rpc f req

                let mutable realR = resp

                match resp.Status with
                | SecurityTokenInvalid -> switchToLoginAction ()
                | SecurityTokenExpired ->
                    let! newToken = x.RefreshTokenAndProceed req.Token

                    match newToken with
                    | Some token ->
                        let! newResp = x.Rpc f { req with Token = token }

                        match newResp.Status with
                        | SecurityTokenInvalid
                        | SecurityTokenExpired -> switchToLoginAction ()
                        | _ -> ()

                        realR <- newResp

                    | None -> switchToLoginAction ()

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

        member x.CreateSecurityApi() =
            { login = x.Wrap securityApi.login
              refreshToken = x.Wrap securityApi.refreshToken }

        member x.CreateMainApi() =
            { becomeProducer = x.Wrap mainApi.becomeProducer
              createQuiz = x.Wrap mainApi.createQuiz
              getProdQuizzes = x.Wrap mainApi.getProdQuizzes
              getProdQuizCard = x.Wrap mainApi.getProdQuizCard
              updateProdQuizCard = x.Wrap mainApi.updateProdQuizCard
              getRegModel = x.Wrap mainApi.getRegModel
              registerTeam = x.Wrap mainApi.registerTeam
              getProdPackages = x.Wrap mainApi.getProdPackages
              getProdPackageCard = x.Wrap mainApi.getProdPackageCard
              createPackage = x.Wrap mainApi.createPackage
              updateProdPackageCard = x.Wrap mainApi.updateProdPackageCard
              aquirePackage = x.Wrap mainApi.aquirePackage
              deleteQuiz = x.Wrap mainApi.deleteQuiz
              deletePackage = x.Wrap mainApi.deletePackage
              getSettings = x.Wrap mainApi.getSettings
              updateSettings = x.Wrap mainApi.updateSettings
              sharePackage = x.Wrap mainApi.sharePackage
              removePackageShare = x.Wrap mainApi.removePackageShare
              getUploadUrl = x.Wrap mainApi.getUploadUrl }

        member x.CreateAdminApi() =
            { getTeams = x.Wrap adminApi.getTeams
              createTeam = x.Wrap adminApi.createTeam
              createTeamBatch = x.Wrap adminApi.createTeamBatch
              getTeamCard = x.Wrap adminApi.getTeamCard
              updateTeamCard = x.Wrap adminApi.updateTeamCard
              changeTeamStatus = x.Wrap adminApi.changeTeamStatus
              getQuizCard = x.Wrap adminApi.getQuizCard
              changeQuizStatus = x.Wrap adminApi.changeQuizStatus
              changeStreamUrl = x.Wrap adminApi.changeStreamUrl
              getPackages = x.Wrap adminApi.getPackages
              setPackage = x.Wrap adminApi.setPackage
              getPackageCard = x.Wrap adminApi.getPackageCard
              startCountDown = x.Wrap adminApi.startCountDown
              pauseCountDown = x.Wrap adminApi.pauseCountDown
              settleTour = x.Wrap adminApi.settleTour
              nextTour = x.Wrap adminApi.nextTour
              nextQuestion = x.Wrap adminApi.nextQuestion
              nextQuestionPart = x.Wrap adminApi.nextQuestionPart
              showMedia = x.Wrap adminApi.showMedia
              showQuestion = x.Wrap adminApi.showQuestion
              getAnswers = x.Wrap adminApi.getAnswers
              updateResults = x.Wrap adminApi.updateResults
              updateResultsWithoutAnswer = x.Wrap adminApi.updateResultsWithoutAnswer
              getListenToken = x.Wrap adminApi.getListenToken }

        member x.CreateTeamApi() =
            { getState = x.Wrap teamApi.getState
              takeActiveSession = x.Wrap teamApi.takeActiveSession
              answers = x.Wrap teamApi.answers
              getHistory = x.Wrap teamApi.getHistory
              vote = x.Wrap teamApi.vote }

        member x.CreateRegApi() = { getRecord = x.Wrap regApi.getRecord }

        member x.CreateAudApi() =
            { getQuiz = x.Wrap audApi.getQuiz
              getHistory = x.Wrap audApi.getHistory }