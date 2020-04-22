module rec AdminCP

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Elmish.React

open Shared
open Shared.AdminModels
open Common

type Msg =
    | QuizCardResp of RESP<QuizControlCard>
    | DeleteError of string
    | Exn of exn
    | ChangeStatus of string
    | PackagesClick
    | PackagesResp of RESP<PackageRecord list>
    | SelectPackage of string
    | PackagesCardResp of RESP<PackageCard option>
    | SelectQwIdx of string
    | UpdateQwName of string
    | UpdateQwSeconds of string
    | UpdateQwText of string
    | UpdateQwAnswer of string
    | UpdateQwComment of string
    | QwImgChanged of {|Type:string; Body:byte[]; Tag:unit|}
    | QwImgClear of unit
    | UploadQwImgResp of RESP<{|BucketKey:string|}>
    | QwCommentImgChanged of {|Type:string; Body:byte[]; Tag:unit|}
    | QwCommentImgClear of unit
    | UploadQwCommentImgResp of RESP<{|BucketKey:string|}>
    | Start
    | Tick
    | Pause
    | Finish
    | Next

type Model = {
    Errors : Map<string, string>
    Quiz : QuizControlCard option
    Package : PackageCard option
    AvailablePackages : PackageRecord list option
    IsLoading : bool
    TimeDiff: TimeSpan
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let loading model =
    {model with IsLoading = true}

let editing model =
    {model with IsLoading = false}

let uploadPackages api model =
    match model.AvailablePackages with
    | Some pkgs -> model |> noCmd
    | None -> model |> loading |> apiCmd api.getPackages () PackagesResp Exn

let setPackage api txt model =
    match Int32.TryParse txt with
    | true, id -> model |> loading |> apiCmd api.setPackage {|PackageId = if id <> -1 then Some id else None|} QuizCardResp Exn
    | _ -> model |> noCmd

let setQwIdx txt model =
    match Int32.TryParse txt with
    | true, id ->
        match model.Package with
        | Some pkg ->
            model
            |> updateCard (fun q -> {q with PackageQwIdx = if id <> -1 then Some id else None})
            |> (fun model ->
                    match pkg.GetQuestion id with
                    | Some qw ->
                        model
                        |> updateQw (
                            fun q -> {q with Text = qw.Text; ImgKey = qw.ImgKey;
                                                        Answer = qw.Answer; Comment = qw.Comment; CommentImgKey = qw.CommentImgKey})
                    | None -> model
            )
        | None -> model
    | _ -> model

let setCard api card st model =
    let model = {model with Quiz = Some card; TimeDiff = timeDiff st}
    match model.Package, card.PackageId with
    | None, Some id -> model |> loading |> apiCmd api.getPackageCard {|PackageId = id|} PackagesCardResp Exn
    | Some pkg, Some id when pkg.PackageId = id -> model |> editing |> noCmd
    | Some _, Some id -> {model with Package = None } |> loading |> apiCmd api.getPackageCard {|PackageId = id|} PackagesCardResp Exn
    | Some _, None -> {model with Package = None } |> editing |> noCmd
    | None, None -> model  |> editing |> noCmd

let updateCard (f : QuizControlCard -> QuizControlCard) model =
    match model.Quiz with
    | Some quiz -> {model with Quiz = Some <| f quiz}
    | _ -> model

let updateQw (f : QuizQuestion -> QuizQuestion) model =
    match model.Quiz with
    | Some quiz ->
        match quiz.CurrentQw with
        | Some qw -> {model with Quiz = Some {quiz with CurrentQw = Some <| f qw}}
        | None -> model
    | _ -> model

let uploadFile api respMsg fileType body model =
    if Array.length body > (1024*128) then
        model |> addError "max image size is 128K" |> noCmd
    else
        model |> loading |> apiCmd api.uploadFile {|Cat = Question; FileType=fileType; FileBody=body|} respMsg Exn

let scheduleTick (model : Model,  cmd : Cmd<Msg>) =
    match model.Quiz with
    | Some quiz when quiz.QuizStatus = Live ->
        match quiz.CurrentQw with
        | Some qw when qw.IsCoundownActive (serverTime model.TimeDiff) -> model, Cmd.batch[cmd; timeoutCmd Tick 1000]
        | _ -> model, cmd
    | _ -> model, cmd

let init (api:IAdminApi) user : Model*Cmd<Msg> =
    {Errors = Map.empty; Quiz = None; Package = None; IsLoading = true; AvailablePackages = None; TimeDiff = TimeSpan.Zero} |> apiCmd api.getQuizCard () QuizCardResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | DeleteError id -> cm |> delError id |> noCmd
    | QuizCardResp {Value = Ok res; ST = st } -> cm |> setCard api res st |> scheduleTick
    | ChangeStatus txt -> cm |> loading |> apiCmd api.changeQuizStatus {|QuizStatus = (defaultArg (fromString txt) Draft)|} QuizCardResp Exn
    | PackagesClick -> cm |> uploadPackages api
    | PackagesResp {Value = Ok pkgs} -> {cm with AvailablePackages = Some pkgs} |> editing |> noCmd
    | SelectPackage txt -> cm |> setPackage api txt
    | SelectQwIdx txt -> cm |> setQwIdx txt |> noCmd
    | PackagesCardResp {Value = Ok res} -> {cm with Package = res} |> editing |> noCmd
    | UpdateQwName txt -> cm |> updateQw (fun qw -> {qw with Name = txt}) |> noCmd
    | UpdateQwSeconds txt -> cm |> updateQw (fun qw -> {qw with Seconds = Int32.Parse txt}) |> noCmd
    | UpdateQwText txt -> cm |> updateQw (fun qw -> {qw with Text = txt}) |> noCmd
    | UpdateQwAnswer txt -> cm |> updateQw (fun qw -> {qw with Answer = txt}) |> noCmd
    | UpdateQwComment txt -> cm |> updateQw (fun qw -> {qw with Comment = txt}) |> noCmd
    | QwImgClear _ -> cm |> updateQw (fun qw -> {qw with ImgKey = ""}) |> noCmd
    | QwImgChanged res -> cm |> uploadFile api UploadQwImgResp res.Type res.Body
    | UploadQwImgResp {Value = Ok res} -> cm |> updateQw (fun qw -> {qw with ImgKey = res.BucketKey}) |> editing |> noCmd
    | QwCommentImgClear _ -> cm |> updateQw (fun qw -> {qw with CommentImgKey = ""}) |> noCmd
    | QwCommentImgChanged res -> cm |> uploadFile api UploadQwCommentImgResp res.Type res.Body
    | UploadQwCommentImgResp {Value = Ok res} -> cm |> updateQw (fun qw -> {qw with CommentImgKey = res.BucketKey}) |> editing |> noCmd
    | Start -> cm |> loading |> apiCmd api.startCountDown cm.Quiz.Value QuizCardResp Exn
    | Tick -> cm |> noCmd |> scheduleTick
    | Pause -> cm |> loading |> apiCmd api.pauseCountDown () QuizCardResp Exn
    | Finish -> cm |> loading |> apiCmd api.finishQuestion () QuizCardResp Exn
    | Next -> cm |> loading |> apiCmd api.nextQuestion () QuizCardResp Exn
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    div [Class "columns"][
        div [Class "column"][
            if model.IsLoading then
                button [Class "button is-loading is-large is-fullwidth is-light"][]
        ]
        div [Class "column"][
            div [][
                match model.Quiz with
                | Some quiz -> yield quizView dispatch user model quiz
                | None -> ()
            ]
        ]
        div [Class "column"][
            for error in model.Errors do
                div [Class "notification is-danger is-light"][
                    button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                    str error.Value
                ]
        ]
    ]

let quizView (dispatch : Msg -> unit) (user:AdminUser) (model : Model) (quiz : QuizControlCard) =

    let changesNotAllowed =
        match quiz.CurrentQw with
        | Some qw when qw.Status = Countdown || qw.Status = Settled -> true
        | _ -> false

    let isCountdown =
        match quiz.CurrentQw with
        | Some qw when qw.Status = Countdown -> true
        | _ -> false

    let isReadOnly = changesNotAllowed || model.IsLoading

    div [][
        div [Class "field"][
            div [Class "control"][
                label [Class "label"][str "Status"]
                div [Class "select"][
                    select[Disabled (model.IsLoading || isCountdown); valueOrDefault quiz.QuizStatus; OnChange (fun ev -> dispatch <| ChangeStatus ev.Value )][
                        for case in Reflection.FSharpType.GetUnionCases typeof<QuizStatus> do
                            option [][str case.Name]
                    ]
                ]
            ]
        ]
        div [Class "field is-grouped"][
            div [Class "control"][
                label [Class "label"][str "Questions Package"]
                match model.AvailablePackages with
                | Some pkgs ->
                    div [Class "select"][
                        let value = match model.Package with Some pkg -> pkg.PackageId | None -> -1
                        select[Disabled isReadOnly; Value value; OnChange (fun ev -> SelectPackage ev.Value |> dispatch)][
                            option[Value -1][str "Not Selected"]
                            for pkg in pkgs |> List.sortBy (fun p -> p.PackageId) |> List.rev do
                                option[Value pkg.PackageId][str <| trimMiddle 32 "..." pkg.Name]
                        ]
                    ]
                | None ->
                    div [Class "field has-addons"][
                        div [Class "control"][
                            let txt = match model.Package with | Some pkg -> str pkg.Name | None -> str "Not Selected"
                            input [Class "input"; Type "text"; Value txt; ReadOnly true; MaxLength 32.0]

                        ]
                        div [Class "control"][
                            button [Class "button"; OnClick (fun _ -> dispatch PackagesClick)][Fa.i [Fa.Solid.EllipsisH][]]
                        ]
                    ]
            ]

            match quiz.CurrentQw with
            | Some _ ->
                div [Class "control"][
                    label [Class "label"][str "Question"]
                    div [Class "select"][
                        let value = match quiz.PackageQwIdx with Some idx -> idx | None -> -1
                        select[Disabled isReadOnly; Value value; OnChange (fun ev -> SelectQwIdx ev.Value |> dispatch)][
                            option[Value -1][str "Not Selected"]
                            match model.Package with
                            | Some pkg ->
                                for (idx,qw) in pkg.Questions |> List.mapi (fun idx qw -> idx,qw) do
                                    option[Value idx][str <| sprintf "%i %s" (idx + 1) (trimEnd 32 "..." qw.Text)]
                            | None -> ()
                        ]
                    ]
                ]
            | None -> ()
        ]
        div[][
            match quiz.CurrentQw with
            | Some qw ->
                yield hr[Class "has-background-grey"]
                yield qwView dispatch user qw model.TimeDiff isReadOnly model.IsLoading
            | None -> ()
        ]

    ]

let qwView (dispatch : Msg -> unit) (user:AdminUser) (qw : QuizQuestion) timeDiff isReadOnly isLoading =

    div[][
        div [Class "field is-grouped"][
            div [Class "control"][
                label [Class "label is-large"][str "Question #"]
                input [Class "input is-large"; Type "text"; Disabled isReadOnly;  valueOrDefault qw.Name; OnChange (fun ev -> dispatch <| UpdateQwName ev.Value)]
            ]

            match qw.Status, qw.SecondsLeft (serverTime timeDiff) with
            | Countdown, sec when sec > 0 ->
                div [Class "control"][
                    label [classList ["label", true; "is-large", true; "has-text-danger", sec < 10]][str "Seconds Left"]
                    input [classList ["input", true; "is-large", true; "has-text-danger", sec < 10]; Type "number"; Disabled true; Value sec]
                ]
            | _ ->
                div [Class "control"; Style [Width "min-content"]][
                    label [Class "label is-large"][str "Seconds"]
                    input [Class "input is-large"; Type "number"; Disabled isReadOnly; valueOrDefault qw.Seconds; OnChange (fun ev -> dispatch <| UpdateQwSeconds ev.Value)]
                ]
        ]

        let ctrlBtn msg caption =
            button [Class "button is-large is-fullwidth"; Disabled isLoading; OnClick (fun _ -> dispatch msg)][str caption]

        match qw.Status, qw.SecondsLeft (serverTime timeDiff) with
        | Announcing, _ -> ctrlBtn Start "Start"
        | Countdown, sec when sec > 0 -> ctrlBtn Pause "Reset countdown"
        | Countdown, _ -> ctrlBtn Finish "Show answer"
        | Settled, _ -> ctrlBtn Next "Next"

        hr[Class "has-background-grey"]

        div [Class "field"][
            label [Class "label"][str "Question Text"]
            div [Class "control"][
                textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault qw.Text; OnChange (fun ev -> dispatch <| UpdateQwText ev.Value )][]
            ]
        ]

        yield! MainTemplates.imgArea () isReadOnly (QwImgChanged >> dispatch) (QwImgClear >> dispatch)  qw.ImgKey "" "Clear"

        br[]

        div [Class "field"][
            label [Class "label"][str "Answer"]
            div [Class "control"][
                textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault qw.Answer; OnChange (fun ev -> dispatch <| UpdateQwAnswer ev.Value )][]
            ]
        ]

        div [Class "field"][
            label [Class "label"][str "Comment"]
            div [Class "control"][
                textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault qw.Comment; OnChange (fun ev -> dispatch <| UpdateQwComment ev.Value )][]
            ]
        ]

        yield! MainTemplates.imgArea () isReadOnly (QwCommentImgChanged >> dispatch) (QwCommentImgClear >> dispatch)  qw.CommentImgKey "" "Clear"
    ]