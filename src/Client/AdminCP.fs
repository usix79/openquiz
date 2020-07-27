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
    | ChangeStreamUrl of string
    | PackagesClick
    | PackagesResp of RESP<PackageRecord list>
    | SelectPackage of string
    | PackagesCardResp of RESP<PackageCard>
    | SelectSlipIdx of string
    | UpdateTourName of string
    | UpdateTourSeconds of string
    | UpdateSlipName of string
    | UpdateQwText of key:QwKey * txt:string
    | UpdateQwTextPart of key:QwKey * partIdx:int * txt:string
    | UpdateQwAnswer of key:QwKey * txt:string
    | UpdateQwComment of key:QwKey * txt:string
    | UpdateQwPoints of key:QwKey * txt:string
    | UpdateQwJpdPoints of key:QwKey * txt:string
    | UpdateQwWithChoice of key:QwKey * bool
    | UpdateQwEOT of key:QwKey * bool
    | NextQw
    | NextQwPart
    | DisplayMedia
    | Start
    | Tick
    | Pause
    | Settle
    | NextSlip

type Model = {
    Errors : Map<string, string>
    Quiz : QuizControlCard option
    Package : PackageCard option
    AvailablePackages : PackageRecord list option
    IsLoading : bool
    TimeDiff: TimeSpan
} with
    member x.IsLastSlip =
        match x.Quiz, x.Package with
        | Some quiz, Some pkg ->
            match quiz.PackageId, quiz.PackageSlipIdx with
            | Some pkgId, Some qwIdx when pkgId = pkg.PackageId -> qwIdx = pkg.Slips.Length - 1
            | _ -> false
        | _ -> false


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

let setSlipIdx txt model =
    match Int32.TryParse txt with
    | true, id ->
        match model.Package with
        | Some pkg ->
            model
            |> updateCard (fun q -> {q with PackageSlipIdx = if id <> -1 then Some id else None})
            |> (fun model ->
                    match pkg.GetSlip id with
                    | Some slip -> model |> updateTour (fun q -> {q with Slip = slip; QwIdx = 0; QwPartIdx = 0; IsMediaDisplayed = false; Name = if slip.Caption <> "" then slip.Caption else q.Name})
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

let updateTour (f : TourControlCard -> TourControlCard) model =
    match model.Quiz with
    | Some quiz ->
        match quiz.CurrentTour with
        | Some qw -> {model with Quiz = Some {quiz with CurrentTour = Some <| f qw}}
        | None -> model
    | _ -> model

let updateSlip (qwKey:QwKey) (f:SingleSlip -> SingleSlip) (model:Model) =
    model |> updateTour (fun tour ->
        match tour.Slip with
        | Single slip -> {tour with Slip = f slip  |> Single }
        | Multiple (name, slips) -> {tour with Slip = (name, slips |> List.mapi (fun idx slip -> if idx = qwKey.QwIdx then f slip else slip)) |> Multiple}
    )

let scheduleTick (model : Model,  cmd : Cmd<Msg>) =
    match model.Quiz with
    | Some quiz when quiz.QuizStatus = Live ->
        match quiz.CurrentTour with
        | Some tour when tour.IsCoundownActive (serverTime model.TimeDiff) -> model, Cmd.batch[cmd; timeoutCmd Tick 1000]
        | _ -> model, cmd
    | _ -> model, cmd


let init (api:IAdminApi) user : Model*Cmd<Msg> =
    {Errors = Map.empty; Quiz = None; Package = None; IsLoading = true; AvailablePackages = None; TimeDiff = TimeSpan.Zero} |> apiCmd api.getQuizCard () QuizCardResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | DeleteError id -> cm |> delError id |> noCmd
    | QuizCardResp {Value = Ok res; ST = st } -> cm |> setCard api res st |> scheduleTick
    | ChangeStatus txt -> cm |> loading |> apiCmd api.changeQuizStatus {|QuizStatus = (defaultArg (fromString txt) Setup)|} QuizCardResp Exn
    | ChangeStreamUrl txt -> cm |> loading |> apiCmd api.changeStreamUrl txt QuizCardResp Exn
    | PackagesClick -> cm |> uploadPackages api
    | PackagesResp {Value = Ok pkgs} -> {cm with AvailablePackages = Some pkgs} |> editing |> noCmd
    | SelectPackage txt -> cm |> setPackage api txt
    | SelectSlipIdx txt -> cm |> setSlipIdx txt |> noCmd
    | PackagesCardResp {Value = Ok res} -> {cm with Package = Some res} |> editing |> noCmd
    | UpdateTourName txt -> cm |> updateTour (fun qw -> {qw with Name = txt}) |> noCmd
    | UpdateTourSeconds txt -> cm |> updateTour (fun qw -> {qw with Seconds = Int32.Parse txt}) |> noCmd
    | UpdateSlipName txt -> cm |> updateTour (fun qw -> {qw with Slip = match qw.Slip with Single _ -> qw.Slip | Multiple (name,slips) -> (txt,slips)|>Multiple}) |> noCmd
    | UpdateQwText (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Question = Solid txt}) |> noCmd
    | UpdateQwTextPart (key,partIdx,txt) -> cm |> updateSlip key (fun slip -> slip.SetQwText partIdx txt) |> noCmd
    | UpdateQwAnswer (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Answer = OpenAnswer txt}) |> noCmd
    | UpdateQwComment (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Comment = txt}) |> noCmd
    | UpdateQwPoints (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Points = System.Decimal.Parse(txt)}) |> noCmd
    | UpdateQwJpdPoints (key,txt) -> cm |> updateSlip key (fun slip -> {slip with JeopardyPoints = ofDecimal (Some txt) }) |> noCmd
    | UpdateQwWithChoice (key,v) -> cm |> updateSlip key (fun slip -> {slip with WithChoice = v}) |> noCmd
    | UpdateQwEOT (key,v) -> cm |> updateSlip key (fun slip -> {slip with EndOfTour = v}) |> noCmd
    | NextQw -> cm |> loading |> apiCmd api.nextQuestion cm.Quiz.Value QuizCardResp Exn
    | NextQwPart -> cm |> loading |> apiCmd api.nextQuestionPart cm.Quiz.Value QuizCardResp Exn
    | DisplayMedia -> cm |> loading |> apiCmd api.showQuestionMedia cm.Quiz.Value QuizCardResp Exn
    | Start -> cm |> loading |> apiCmd api.startCountDown cm.Quiz.Value QuizCardResp Exn
    | Tick -> cm |> noCmd |> scheduleTick
    | Pause -> cm |> loading |> apiCmd api.pauseCountDown () QuizCardResp Exn
    | Settle -> cm |> loading |> apiCmd api.settleTour () QuizCardResp Exn
    | NextSlip -> cm |> loading |> apiCmd api.nextTour () QuizCardResp Exn
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (settings:Settings) (model : Model) =
    div [Class "columns"][
        div [Class "column"][
            if model.IsLoading then
                button [Class "button is-loading is-large is-fullwidth is-light"][]
        ]
        div [Class "column"][
            div [][
                match model.Quiz with
                | Some quiz -> yield quizView dispatch user settings model quiz
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

let quizView (dispatch : Msg -> unit) (user:AdminUser) (settings:Settings) (model : Model) (quiz : QuizControlCard) =

    let changesNotAllowed =
        match quiz.CurrentTour with
        | Some qw when qw.Status = Countdown || qw.Status = Settled -> true
        | _ -> false

    let isCountdown =
        match quiz.CurrentTour with
        | Some qw when qw.Status = Countdown -> true
        | _ -> false

    let isReadOnly = changesNotAllowed || model.IsLoading

    div [][
        div [Class "field is-grouped"][
            div [Class "control"][
                label [Class "label"][str "Status"]
                div [Class "select"][
                    select[Disabled (model.IsLoading || isCountdown); valueOrDefault quiz.QuizStatus; OnChange (fun ev -> dispatch <| ChangeStatus ev.Value )][
                        for case in Reflection.FSharpType.GetUnionCases typeof<QuizStatus> do
                            option [][str case.Name]
                    ]
                ]
            ]
            div [Class "control is-expanded"][
                label [Class "label"][str "Stream URL"]
                div [Class "control is-expanded has-icons-right"][
                    input [Class "input"; Type "text"; MaxLength 256.0;
                        Value (quiz.StreamUrl |> Option.defaultValue "");
                        OnChange (fun ev -> dispatch <| ChangeStreamUrl ev.Value)]
                ]
            ]
        ]
        div [Class "field is-grouped"][
            div [Class "control"][
                let isControlReadOnly = changesNotAllowed && (not model.IsLastSlip) || model.IsLoading
                label [Class "label"][str "Questions Package"]
                match model.AvailablePackages with
                | Some pkgs ->
                    div [Class "select"][
                        let value = match model.Package with Some pkg -> pkg.PackageId | None -> -1
                        select[Disabled isControlReadOnly; Value value; OnChange (fun ev -> SelectPackage ev.Value |> dispatch)][
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

            match quiz.CurrentTour with
            | Some _ ->
                div [Class "control"][
                    label [Class "label"][str "Question"]
                    div [Class "select"][
                        let value = match quiz.PackageSlipIdx with Some idx -> idx | None -> -1
                        select[Disabled isReadOnly; Value value; OnChange (fun ev -> SelectSlipIdx ev.Value |> dispatch)][
                            option[Value -1][str "Not Selected"]
                            match model.Package with
                            | Some pkg ->
                                for (idx,slip) in pkg.Slips |> List.mapi (fun idx slip -> idx,slip) do
                                    option[Value idx][str <| sprintf "%i %s" (idx + 1) (trimEnd 32 "..." slip.Annotation)]
                            | None -> ()
                        ]
                    ]
                ]
            | None -> ()
        ]
        div[][
            match quiz.CurrentTour with
            | Some qw ->
                yield hr[Class "has-background-grey"]
                yield qwView dispatch user settings qw model.TimeDiff isReadOnly model.IsLoading model.IsLastSlip
            | None -> ()
        ]

    ]

let qwTextArea dispatch key txt isReadOnly =
    div [Class "field"][
        label [Class "label"][str "Question Text"]
        div [Class "control"][
            textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault txt; OnChange (fun ev -> dispatch <| UpdateQwText (key,ev.Value) )][]
        ]
    ]

let qwInput dispatch placeholder txt key partIdx anounced =
    div [Class "control has-icons-left has-icons-right"; Style [MarginBottom "3px"]][
        input [Class "input"; Type "text"; Placeholder placeholder; ReadOnly anounced;
            valueOrDefault txt; MaxLength 256.0; OnChange (fun ev -> UpdateQwTextPart (key,partIdx,ev.Value) |> dispatch)]
        span [Class "icon is-left"][str (sprintf "%i" (partIdx + 1))]
        if (anounced) then
            span [Class "icon is-right"][Fa.i [Fa.Solid.Check][]]
    ]

let awTextArea dispatch key txt isReadOnly =
    div [Class "field"][
        label [Class "label"][str "Answer"]
        div [Class "control"][
            textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault txt; OnChange (fun ev -> dispatch <| UpdateQwAnswer (key,ev.Value) )][]
        ]
    ]

let cmtTextArea dispatch key txt isReadOnly =
    div [Class "field"][
        label [Class "label"][str "Comment"]
        div [Class "control"][
            textarea [Class "textarea"; Disabled isReadOnly; MaxLength 512.0; valueOrDefault txt; OnChange (fun ev -> dispatch <| UpdateQwComment (key,ev.Value) )][]
        ]
    ]

let singleSlipEl dispatch settings status (qwIdx:int) (slip:SingleSlip) nextQwPartIdx isReadOnly isFromMultislip =
    let key = {TourIdx = -1; QwIdx = qwIdx}
    div [][

        div [Class "field is-grouped"][
            div [Class "control"][
                label [Class "label"][str "Points"]
                input [Class "input"; Style[Width "80px"]; Type "number"; Disabled isReadOnly; valueOrDefault slip.Points; OnChange (fun ev -> dispatch <| UpdateQwPoints (key,ev.Value) )]
            ]
            div [Class "control"][
                label [Class "label"][str "Jeopardy"]
                input [Class "input"; Style[Width "80px"]; Type "number"; Disabled isReadOnly; valueOrDefault slip.JeopardyPoints; OnChange (fun ev -> dispatch <| UpdateQwJpdPoints (key,ev.Value) )]
            ]
            label [Class "checkbox"][
                input [Type "checkbox";
                    Checked slip.WithChoice; Disabled isReadOnly; OnChange (fun ev -> UpdateQwWithChoice (key, (ev.Checked)) |> dispatch)]
                str " with choice"
            ]
            if not isFromMultislip then
                label [Class "checkbox"][
                    input [Type "checkbox";
                        Checked slip.EndOfTour; Disabled isReadOnly; OnChange (fun ev -> UpdateQwEOT (key, (ev.Checked)) |> dispatch)]
                    str " end of tour"
                ]
        ]

        match slip.Question with
        | Solid qw -> qwTextArea dispatch key qw isReadOnly
        | Split list ->
            div [Class "field"][
                label [Class "label"][str "Questions Text"]
                for (idx,qw) in list |> List.indexed do
                    qwInput dispatch (sprintf "Question part %i" (idx + 1)) qw key idx (idx < nextQwPartIdx || status <> Announcing)
            ]

        yield! MainTemplates.mediaEl settings.MediaHost slip.QuestionMedia false
        br[]
        match slip.Answer with
        | OpenAnswer txt ->
            awTextArea dispatch key txt isReadOnly
        | ChoiceAnswer list ->
            div[Class "content"][
                strong[][str "Answer"]
                ul[][
                    for ch in list do
                        li [][
                            str ch.Text
                            if ch.IsCorrect then
                                Fa.i[Fa.Solid.Check][]
                        ]
                ]
            ]

        br[]
        cmtTextArea dispatch key slip.Comment isReadOnly
        yield! MainTemplates.mediaEl settings.MediaHost slip.AnswerMedia false
    ]

let multipleSlipEl dispatch settings status (name:string) (slips:SingleSlip list) nextQwIdx nextQwPartIdx isReadOnly =
    div[][
        div [Class "control"][
            label [Class "label"][str "Tour Name"]
            input [Class "input"; Type "text"; Disabled isReadOnly;  valueOrDefault name; OnChange (fun ev -> dispatch <| UpdateSlipName ev.Value)]
        ]
        br[]
        div [Class "table-container"; Style[Width "800px"]][

            table [Class "table"][
                thead[][
                    tr[][
                        for n in 1 .. slips.Length do
                            th [][str <| sprintf "Question %i" n]
                    ]
                ]
                tbody [][
                    tr[][
                        for (idx,slip) in slips |> List.indexed do
                            td [classList ["has-background-success", idx = nextQwIdx ]][
                                singleSlipEl dispatch settings status idx slip 0 (isReadOnly || (idx < nextQwIdx)) true
                            ]
                    ]
                ]
            ]
        ]
    ]

let secondsLeftToString sec =
    if sec > 10
    then (sec - 10).ToString() + " + 10"
    else sec.ToString()

let qwView (dispatch : Msg -> unit) (user:AdminUser) (settings:Settings) (tour : TourControlCard) timeDiff isReadOnly isLoading isLastQw =

    div[][
        div [Class "field is-grouped"][
            div [Class "control"][
                label [Class "label is-large"][str "#"]
                input [Class "input is-large"; Type "text"; Disabled isReadOnly;  valueOrDefault tour.Name; OnChange (fun ev -> dispatch <| UpdateTourName ev.Value)]
            ]

            match tour.Status, tour.SecondsLeft (serverTime timeDiff) with
            | Countdown, sec when sec > 0 ->
                div [Class "control"][
                    label [classList ["label", true; "is-large", true; "has-text-danger", sec <= 10]][str "Seconds Left"]
                    input [classList ["input", true; "is-large", true; "has-text-danger", sec <= 10]; Type "text"; Disabled true; Value (secondsLeftToString sec)]
                ]
            | _ ->
                div [Class "control"; Style [Width "min-content"]][
                    label [Class "label is-large"][str "Seconds"]
                    input [Class "input is-large"; Type "number"; Disabled isReadOnly; valueOrDefault tour.Seconds; OnChange (fun ev -> dispatch <| UpdateTourSeconds ev.Value)]
                ]
        ]

        let ctrlBtn msg caption =
            button [Class "button is-large is-fullwidth"; Disabled isLoading; OnClick (fun _ -> dispatch msg)][str caption]

        match tour.Status, tour.SecondsLeft (serverTime timeDiff) with
        | Announcing, _  when tour.NeedToDisplayMedia -> ctrlBtn DisplayMedia "Show Media"
        | Announcing, _  when tour.IsReadyForCountdown -> ctrlBtn Start "Start Countdown"
        | Announcing, _  when not tour.IsLastPart -> ctrlBtn NextQwPart (sprintf "Show Question Part %i" (tour.QwPartIdx + 1))
        | Announcing, _  -> ctrlBtn NextQw (sprintf "Show Question %i" (tour.QwIdx + 1))
        | Countdown, sec when sec > 0 -> ctrlBtn Pause "Reset Countdown"
        | Countdown, _ -> ctrlBtn Settle "Show answer"
        | Settled, _ when isLastQw -> ctrlBtn (ChangeStatus (Finished.ToString())) "Finish"
        | Settled, _-> ctrlBtn NextSlip "Next Slip"

        hr[Class "has-background-grey"]

        match tour.Slip with
        | Single slip -> singleSlipEl dispatch settings tour.Status 0 slip tour.QwPartIdx isReadOnly false
        | Multiple (name,slips) -> multipleSlipEl dispatch settings tour.Status name slips tour.QwIdx tour.QwPartIdx isReadOnly
    ]