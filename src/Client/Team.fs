module rec Team

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fable.SimpleJson
open Elmish.React

open Shared
open Common
open TeamModels

type Tab = History | Question | Results

type AnswersStatus = Input | Sending | Sent | Failed

type AnswersSlip = {
    TourIdx : int
    Status : AnswersStatus
    Values : Map<int, (string*bool)>
} with
    member x.Get qwIdx =
        match x.Values.TryGetValue qwIdx with
        | true, x -> x
        | _ -> "",false

type Msg =
    | QuizCardResp of RESP<QuizCard>
    | Exn of exn
    | Reactivate
    | ChangeTab of Tab
    | UpdateAnswer of qwIdx:int*txt:string
    | ToggleJeopardy of qwIdx:int
    | CountdownTick of {|QwIndex: int|}
    | QuizChanged of QuizChangedEvent
    | SourceError of string
    | Heartbeat
    | AnswerResponse of RESP<unit>
    | GetHistoryResp of RESP<TeamHistoryRecord list>
    | GetResultsResp of RESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>

type Model = {
    IsActive : bool
    Quiz : QuizCard option
    Answers : AnswersSlip option
    ActiveTab : Tab
    Error : string
    TimeDiff: TimeSpan
    SseSource: Infra.SseSource option
    IsConnectionOk : bool
    History : TeamHistoryRecord list
    TeamResults : TeamResult list
    QuestionResults : QuestionResult list
} with
    member x.CurrentTour =
        match x.Quiz with
        | Some quiz -> quiz.TC
        | None -> None

    member x.IsCountdownActive =
        match x.Quiz with
        | Some quiz ->
            match quiz.TC with
            | Some qw -> quiz.QS = Live && qw.IsCountdownActive (serverTime x.TimeDiff)
            | None -> false
        | None -> false

let error txt model =
    {model with Error = txt}

let ok model =
    {model with Error = ""}

let startup user quiz serverTime model =
    {model with
        IsActive = true
        Quiz = Some quiz
        TimeDiff = timeDiff serverTime
        Answers =
            Infra.loadFromLocalStorage<AnswersSlip> (awStorageKey user)
            |> Option.defaultValue {
                    TourIdx = quiz.TC |> Option.map (fun tour -> tour.Idx) |> Option.defaultValue -1
                    Status = if quiz.Aw.Count > 0 then Sent else Input
                    Values = quiz.Aw
            }
            |> Some
    }

let updateQuiz (f : QuizCard -> QuizCard) model  =
    match model.Quiz with
    | Some quiz -> {model with Quiz = Some <| f quiz}
    | None -> model

let initAnswers (model : Model, cmd : Cmd<Msg>) =
    let createAnswers (tour:TourCard) timeDiff =
        {
            TourIdx = tour.Idx
            Status = if tour.IsCountdownFinished (serverTime timeDiff) then Failed else Input
            Values = Map.empty
        }

    match model.CurrentTour with
    | Some tour ->
        match model.Answers with
        | Some aw when aw.TourIdx <> tour.Idx -> {model with Answers = Some (createAnswers tour model.TimeDiff)}
        | None -> {model with Answers = Some (createAnswers tour model.TimeDiff)}
        | _ -> model
    | None -> {model with Answers = None}
    , cmd

let setupCountdown (model : Model, cmd : Cmd<Msg>) =
    match model.IsCountdownActive with
    | true ->
        match model.CurrentTour with
        | Some qw ->  model, timeoutCmd (CountdownTick {|QwIndex = qw.Idx|}) 1000
        | None -> model |> noCmd
    | false -> model |> noCmd
    |> batchCmd cmd

let subscribe quizId (model:Model, cmd : Cmd<Msg>) =
    match model.Quiz with
    | Some quiz ->
        let url = Infra.sseUrl quizId quiz.V quiz.LT
        let source = Infra.SseSource(url)

        //let subUpdate dispatch = source.OnMessage (QuizChanged >> dispatch)
        let subUpdate dispatch =
            source.SSE.onmessage <- (fun evt ->
                    let evt = Json.parseAs<QuizChangedEvent> (sprintf "%A" evt.data)
                    QuizChanged evt |> dispatch
            )

        let subError dispatch = source.OnError (SourceError >> dispatch)
        let heartbeat dispatch = source.OnHeartbeat (fun _ ->  dispatch Heartbeat)
        {model with SseSource = Some source}, Cmd.batch[cmd; Cmd.ofSub subUpdate; Cmd.ofSub subError; Cmd.ofSub heartbeat]
    | None -> model, cmd

let unsubscribe (model:Model) =
    match model.SseSource with
    | Some source -> source.Close()
    | _ -> ()

let applyEvent (evt:QuizChangedEvent) (model:Model) =
    model |> updateQuiz (fun quiz -> {quiz with QS = evt.QS; TC = evt.T})

let answerStatus satus model =
    match model.Answers with
    | Some aw -> {model with Answers = Some {aw with Status = satus}}
    | _ -> model

let answerText qwIdx txt model =
    match model.Answers with
    | Some aw when aw.Status = Input ->
        let (_,jpd) = aw.Get qwIdx
        {model with Answers = Some {aw with Values = aw.Values.Add(qwIdx,(txt,jpd))}}
    | _ -> model

let toggleJpd qwIdx model =
    match model.Answers with
    | Some aw when aw.Status = Input ->
        let (txt, jpd) = aw.Get qwIdx
        {model with Answers = Some {aw with Values = aw.Values.Add(qwIdx,(txt, not jpd))}}
    | _ -> model

let sendAnswer api (model : Model) =
    let answerCmd api model =
        match model.Answers with
        | Some aw ->
            let req =
                aw.Values
                |> Map.toList
                |> List.choose (fun (qwIdx,(txt, jpd)) -> if not (String.IsNullOrEmpty txt) then Some ({TourIdx = aw.TourIdx; QwIdx = qwIdx}, (txt, jpd)) else None)
                |> Map.ofList
            if req.Count > 0 then model |> answerStatus Sending |> apiCmd api.answers req AnswerResponse Exn
            else model |> answerStatus Sent |> noCmd
        | _ ->  model |> noCmd

    match model.Answers with
    | Some aw when aw.Status = Input ->
        match model.CurrentTour with
        | Some tour when (aw.TourIdx <> tour.Idx)
            || (tour.TS = Countdown && not (tour.IsCountdownActive (serverTime model.TimeDiff)))
            || (tour.TS = Settled)
            -> model |> answerCmd api
        | _ -> model |> noCmd
    | _ -> model |> noCmd

let connection isOk model =
    {model with IsConnectionOk = isOk}

let awStorageKey (user:TeamUser) =
    sprintf "aw-%i-%i" user.QuizId user.TeamId

let saveAnswer (user:TeamUser) (model: Model) =
    match model.Answers with
    | Some aw -> Infra.saveToLocalStorage (awStorageKey user) aw
    | _ -> ()
    model

let deleteAnswer (user:TeamUser) (model: Model) =
    Infra.removeFromLocalStorage (awStorageKey user)
    model

let init (api:ITeamApi) user : Model*Cmd<Msg> =
    {IsActive = true; Quiz = None; Answers = None; IsConnectionOk = false;  ActiveTab = Question;
        Error = ""; TimeDiff = TimeSpan.Zero; SseSource = None; History = []; TeamResults = []; QuestionResults = []} |> apiCmd api.getState () QuizCardResp Exn

let update (api:ITeamApi) (user:TeamUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | Reactivate -> cm |> apiCmd api.takeActiveSession () QuizCardResp Exn
    | QuizCardResp {Value = Ok res; ST = st} -> cm |> startup user res st |> ok |> sendAnswer api |> subscribe user.QuizId |> initAnswers |> setupCountdown
    | CountdownTick _ -> cm |> sendAnswer api |> setupCountdown
    | QuizChanged evt -> cm |> applyEvent evt |> sendAnswer api |> initAnswers |> setupCountdown
    | AnswerResponse {Value = Ok _} -> cm |> deleteAnswer user |> answerStatus Sent |> ok |> noCmd
    | SourceError _ -> cm |> connection false |> noCmd
    | UpdateAnswer (qwIdx,txt) -> cm |> answerText qwIdx txt |> saveAnswer user |> noCmd
    | ToggleJeopardy qwIdx -> cm |> toggleJpd qwIdx |> saveAnswer user |> noCmd
    | Heartbeat -> cm |> connection true |> noCmd
    | ChangeTab Question -> {cm with ActiveTab = Question} |> noCmd
    | ChangeTab History -> {cm with ActiveTab = History} |> apiCmd api.getHistory () GetHistoryResp Exn
    | ChangeTab Results -> {cm with ActiveTab = Results} |> apiCmd api.getResults () GetResultsResp Exn
    | GetHistoryResp {Value = Ok res} -> {cm with History = res} |> noCmd
    | GetResultsResp {Value = Ok res} -> {cm with TeamResults = res.Teams; QuestionResults = res.Questions} |> noCmd
    | Exn ex when ex.Message = Errors.SessionIsNotActive ->
        unsubscribe cm
        {cm with IsActive = false} |> noCmd
    | Exn ex -> cm |> error ex.Message |> noCmd
    | Err txt -> cm |> error txt |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:TeamUser) (model : Model) =
    match model.IsActive with
    | true ->
        match model.Quiz with
        | Some quiz -> activeView dispatch user quiz model
        | None -> str model.Error
    | false -> notActiveView dispatch user model.Error

let notActiveView (dispatch : Msg -> unit) (user:TeamUser) error =
    section [Class "hero is-danger is-medium is-fullheight"][
        div [Class "hero-body"][
            div [Class "container has-text-centered is-fluid"][
                p [Class "title"] [ str "NOT ACTIVE" ]
                p [ Class "title" ] [ str user.QuizName ]
                p [ Class "subtitle" ] [ str user.TeamName ]
                p [ Class "subtitle" ] [ str "Team's session is running from another device. Only one device is allowed per team." ]
                a [Class "button is-large"; OnClick (fun _ -> dispatch (Reactivate)) ] [ str "Use this device" ]
                p [Class "help is-white"][ str error ]
            ]
        ]
    ]

let activeView (dispatch : Msg -> unit) (user:TeamUser) quiz model =
    let serverTime = serverTime model.TimeDiff
    let secondsLeft, isCountdownActive, isCountdownFinished =
        match quiz.TC with
        | Some tour -> tour.SecondsLeft serverTime, tour.IsCountdownActive serverTime, tour.IsCountdownFinished serverTime
        | None -> 0, false, false

    div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
        div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [
            MainTemplates.mixlrFrame quiz.Mxlr
            MainTemplates.playTitle user.QuizName quiz.Img model.IsConnectionOk quiz.Mxlr.IsNone

            h4 [Class "subtitle is-4" ] [ str user.TeamName ]

            div [Class "container"] [
                match quiz.TS with
                | New -> div [Class "notification is-white"][str "Waiting for confirmation of the registration..."]
                | Admitted ->
                    div[][
                        match model.ActiveTab with
                        | History -> yield historyView dispatch model
                        | Question ->
                            match quiz.QS, model.Answers with
                            | Live, Some answers -> yield quiestionView dispatch quiz answers isCountdownFinished
                            | _ -> yield MainTemplates.playQuiz quiz.QS quiz.Msg
                        | Results -> yield resultsView dispatch user model
                    ]
                | Rejected -> div [Class "notification is-white"][span[Class "has-text-danger has-text-weight-bold"][str "Registration has been rejected ("]]
            ]
            p [Class "help is-danger"][ str model.Error ]
            div [ Style [Height "66px"]] []
        ]
        if quiz.TS = Admitted then
            MainTemplates.playFooter (ChangeTab >> dispatch) History Question Results model.ActiveTab isCountdownActive secondsLeft
    ]

let quiestionView (dispatch : Msg -> unit) quiz answers isCountdownFinished =
    div [] [
        match quiz.TC with
        | Some tour ->
            match tour.Slip with
            | SS slip -> yield singleQwView dispatch tour slip answers isCountdownFinished
            | MS (name,slips) -> yield multipleQwView dispatch tour name slips answers isCountdownFinished
        | None -> ()
    ]

let answerStatusIcon status txt =
    span [Class "icon is-large is-right has-text-black"][
        Fa.i [ match status with
                | Input -> Fa.Solid.Question
                | Sending -> Fa.Solid.Spinner
                | Sent when String.IsNullOrEmpty txt -> Fa.Solid.Exclamation
                | Sent -> Fa.Solid.Check
                | Failed -> Fa.Solid.Exclamation ] [
        ]
    ]

let awArea dispatch aw jpd status withChoice readOnly =
    div [Class "control has-icons-right"][
        if withChoice then
            p [Class "control"][
                a [classList ["button", true; "has-text-grey-light", not jpd; "has-text-danger", jpd];
                 Title "Jeopardy!"; ReadOnly readOnly; OnClick (fun _ -> dispatch <| ToggleJeopardy 0)][Fa.i [ Fa.Solid.Paw] [str " Jeopardy!" ]]
            ]

        textarea [ Class "textarea"; MaxLength 64.0;
            ReadOnly readOnly; valueOrDefault aw;
            OnChange (fun ev -> dispatch <| UpdateAnswer (0,ev.Value) )][]
        answerStatusIcon status aw
    ]

let singleQwView dispatch tour slip answers isCountdownFinished =
    div[][
        MainTemplates.singleTourInfo tour.Name slip

        let (txt,jpd) = answers.Get 0

        match slip with
        | X3 -> ()
        | QW slip ->
            label [Class "label"][str "Your answer"]
            awArea dispatch txt jpd answers.Status slip.Ch isCountdownFinished
        | AW slip ->
            label [Class "label"][str "Your answer"]
            awArea dispatch txt jpd answers.Status slip.Ch true
    ]

let awInput dispatch idx aw jpd status withChoice readOnly =
    div [Style [MaxWidth "320px"; Display DisplayOptions.InlineBlock]][
        div [Class "field has-addons"][
            if (withChoice) then
                p [Class "control"][
                    a [classList ["button", true; "has-text-grey-light", not jpd; "has-text-danger", jpd];
                     Title "Jeopardy!"; ReadOnly readOnly; OnClick (fun _ -> dispatch <| ToggleJeopardy idx)][Fa.i [ Fa.Solid.Paw] [ ]]
                ]
            p [Class "control has-icons-right"][
                input [Class "input"; Type "text"; MaxLength 64.0; Placeholder "Your Answer";
                    ReadOnly readOnly; valueOrDefault aw; OnChange (fun ev -> dispatch <| UpdateAnswer (idx,ev.Value) )]

                answerStatusIcon status aw
            ]
        ]
    ]

let multipleQwView dispatch tour name slips answers isCountdownFinished =
    div [][
        h5 [Class "subtitle is-5"] [str name]
        for (idx,slip) in slips |> List.indexed do
            let (aw, jpd) = answers.Get idx
            match slip with
            | QW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "Question %s.%i" tour.Name (idx + 1)]
                yield! MainTemplates.imgEl slip.Img
                p [] (splitByLines slip.Txt)
                awInput dispatch idx aw jpd answers.Status slip.Ch isCountdownFinished
                br[]
                br[]

            | AW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "Question %s.%i" tour.Name (idx + 1)]
                awInput dispatch idx aw jpd answers.Status slip.Ch isCountdownFinished
                p [Class "has-text-weight-light is-family-secondary is-size-6"][
                    str "correct answer: "
                    str (slip.Txt.Split('\n').[0])
                ]
                yield! MainTemplates.imgEl slip.Img
                p [Class "is-italic has-text-weight-light is-family-secondary is-size-7"] (splitByLines slip.Com)
                br[]

            | X3 -> str "x3"
    ]

let historyView dispatch model =
    table [Class "table is-hoverable is-fullwidth"][
        thead [ ] [
            tr [ ] [
                th [Style [Width "30px"] ] [ str "#" ]
                th [ ] [ str "Answer" ]
                th [ ] [ str "Points" ]
            ]
        ]

        tbody [ ] [
            for aw in model.History |> List.sortByDescending (fun aw -> aw.QwIdx)  do
                let modifiers =
                    match aw.Result with
                    | Some v when v > 0m -> ["has-text-success", true]
                    | Some _ -> ["has-text-danger", true]
                    | None -> []

                tr [ ][
                    td [] [p [classList modifiers][str aw.QwName]]
                    td [] [p [classList modifiers][
                        if aw.AwJpd then Fa.i [Fa.Solid.Paw; Fa.PullRight][]
                        str (defaultArg aw.AwTxt "")]
                    ]
                    td [] [
                        let txt =
                            match aw.Result with
                            | Some d when d > 0m -> (sprintf "+%M" d)
                            | Some d -> d.ToString()
                            | None -> ""
                        p [classList modifiers][str txt]
                    ]
                ]

                if aw.QwAw <> "" then
                    tr [ ][
                        td [] []
                        td [ColSpan 2] [
                            span [Class "is-italic has-text-weight-light is-family-secondary is-size-7"][
                                str "correct answer: "
                                str (aw.QwAw.Split('\n').[0])
                            ]
                        ]
                    ]
        ]
    ]

let resultsView dispatch (user:TeamUser) model =
    let currentRes =
        model.TeamResults
        |> List.tryFind (fun r -> r.TeamId = user.TeamId)

    MainTemplates.resultsView currentRes model.TeamResults

