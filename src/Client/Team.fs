module rec Team

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Elmish.React

open Shared
open Common
open TeamModels

type Tab = History | Question | Results

type AnswerStatus = Input | Sending | Sent | Failed

type Answer = {
    QwIdx : int
    Status : AnswerStatus
    Text : string
}

type Msg =
    | QuizCardResp of RESP<QuizCard>
    | Exn of exn
    | Reactivate
    | ChangeTab of Tab
    | UpdateAnswer of string
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
    Answer : Answer option
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
        Answer =
            match Infra.loadFromLocalStorage<Answer> (awStorageKey user) with
            | Some aw -> Some aw
            | None ->
                match quiz.Aw with
                | Some txt -> Some {QwIdx = (match quiz.TC with Some qw -> qw.Idx | None -> -1); Status = Sent; Text = txt}
                | None -> None
    }

let updateQuiz (f : QuizCard -> QuizCard) model  =
    match model.Quiz with
    | Some quiz -> {model with Quiz = Some <| f quiz}
    | None -> model

let initAnswer (model : Model, cmd : Cmd<Msg>) =
    let createAnswer (qw:TourCard) timeDiff =
        {QwIdx = qw.Idx; Text = ""; Status = if qw.IsCountdownActive (serverTime timeDiff) then Input else Failed}

    match model.CurrentTour with
    | Some tour ->
        match tour.TS with
        | Announcing -> {model with Answer = None}
        | Countdown | Settled ->
            match model.Answer with
            | Some aw when aw.QwIdx <> tour.Idx -> {model with Answer = Some (createAnswer tour model.TimeDiff)}
            | None -> {model with Answer = Some (createAnswer tour model.TimeDiff)}
            | _ -> model
    | None -> {model with Answer = None}
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

        let subUpdate dispatch = source.OnMessage (QuizChanged >> dispatch)
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
    match model.Answer with
    | Some aw -> {model with Answer = Some {aw with Status = satus}}
    | _ -> model

let answerText txt model =
    match model.Answer with
    | Some aw when aw.Status = Input -> {model with Answer = Some {aw with Text = txt}}
    | _ -> model

let sendAnswer api (model : Model) =
    let answerCmd api model =
        match model.Answer with
        | Some aw when not (String.IsNullOrWhiteSpace aw.Text) ->
            model |> answerStatus Sending |> apiCmd api.answer {|Answer = aw.Text; QwIndex = aw.QwIdx|} AnswerResponse Exn
        | _ ->  model |> answerStatus Failed |> noCmd

    match model.Answer with
    | Some aw when aw.Status = Input ->
        match model.CurrentTour with
        | Some tour when (aw.QwIdx <> tour.Idx)
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
    match model.Answer with
    | Some aw -> Infra.saveToLocalStorage (awStorageKey user) aw
    | _ -> ()
    model

let deleteAnswer (user:TeamUser) (model: Model) =
    Infra.removeFromLocalStorage (awStorageKey user)
    model

let init (api:ITeamApi) user : Model*Cmd<Msg> =
    {IsActive = true; Quiz = None; Answer = None; IsConnectionOk = false;  ActiveTab = Question;
        Error = ""; TimeDiff = TimeSpan.Zero; SseSource = None; History = []; TeamResults = []; QuestionResults = []} |> apiCmd api.getState () QuizCardResp Exn

let update (api:ITeamApi) (user:TeamUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | Reactivate -> cm |> apiCmd api.takeActiveSession () QuizCardResp Exn
    | QuizCardResp {Value = Ok res; ST = st} -> cm |> startup user res st |> ok |> sendAnswer api |> subscribe user.QuizId |> initAnswer |> setupCountdown
    | CountdownTick _ -> cm |> sendAnswer api |> setupCountdown
    | QuizChanged evt -> cm |> applyEvent evt |> sendAnswer api |> initAnswer |> setupCountdown
    | AnswerResponse {Value = Ok _} -> cm |> deleteAnswer user |> answerStatus Sent |> ok |> noCmd
    | SourceError _ -> cm |> connection false |> noCmd
    | UpdateAnswer txt -> cm |> answerText txt |> saveAnswer user |> noCmd
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
    let secondsLeft, isCountdownActive =
        match quiz.TC with
        | Some q -> q.SecondsLeft serverTime, q.IsCountdownActive serverTime
        | None -> 0, false

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
                            match quiz.QS with
                            | Live -> yield quiestionView dispatch quiz model.Answer isCountdownActive
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

let quiestionView (dispatch : Msg -> unit) quiz answer isCountdownActive =
    div [] [
        MainTemplates.playTour quiz.TC

        match answer with
        | Some aw ->
            label [Class "label"][str "Your answer"]
            div [Class "control has-icons-right"][
                textarea [ Class "textarea"; MaxLength 128.0;
                    ReadOnly (not isCountdownActive); valueOrDefault aw.Text;
                    OnChange (fun ev -> dispatch <| UpdateAnswer ev.Value )][]
                span [Class "icon is-large is-right has-text-black"][
                    Fa.i [ match aw.Status with
                            | Input -> Fa.Solid.Question
                            | Sending -> Fa.Solid.Spinner
                            | Sent -> Fa.Solid.Check
                            | Failed -> Fa.Solid.Exclamation ] [
                    ]
                ]
            ]
        | _ -> ()
    ]

let historyView dispatch model =
    table [Class "table is-hoverable is-fullwidth"][
        thead [ ] [
            tr [ ] [
                th [Style [Width "30px"] ] [ str "#" ]
                th [ ] [ str "Answer" ]
                th [ ] [ str "Result" ]
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
                    td [] [p [classList modifiers][str (defaultArg aw.AwTxt "")]]
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

