module rec Aud

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Elmish.React

open Shared
open Shared.AudModels
open Common

type Tab = History | Question | Results

type Msg =
    | GetQuizRsp of RESP<QuizCard>
    | ChangeTab of Tab
    | QuizChanged of QuizChangedEvent
    | SourceError of string
    | Heartbeat
    | CountdownTick of {|QwIndex: int|}
    | GetHistoryResp of RESP<HistoryRecord list>
    | GetResultsResp of RESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
    | Exn of exn

type Model = {
    Quiz : QuizCard option
    ActiveTab : Tab
    Error : string
    TimeDiff: TimeSpan
    SseSource: Infra.SseSource option
    IsConnectionOk : bool
    History : HistoryRecord list
    TeamResults : TeamResult list
    QuestionResults : QuestionResult list
} with
    member x.CurrentQuestion =
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

let subscribe quizId (model:Model) =
    match model.Quiz with
    | Some quiz ->
        let url = Infra.sseUrl quizId quiz.V quiz.LT
        let source = Infra.SseSource(url)

        let subUpdate dispatch = source.OnMessage (QuizChanged >> dispatch)
        let subError dispatch = source.OnError (SourceError >> dispatch)
        let heartbeat dispatch = source.OnHeartbeat (fun _ ->  dispatch Heartbeat)
        {model with SseSource = Some source}, Cmd.batch[Cmd.ofSub subUpdate; Cmd.ofSub subError; Cmd.ofSub heartbeat]
    | None -> model |> noCmd

let setupCountdown (model : Model, cmd : Cmd<Msg>) =
    match model.IsCountdownActive with
    | true ->
        match model.CurrentQuestion with
        | Some qw ->  model, timeoutCmd (CountdownTick {|QwIndex = qw.Idx|}) 1000
        | None -> model |> noCmd
    | false -> model |> noCmd
    |> batchCmd cmd

let updateQuiz (f : QuizCard -> QuizCard) model  =
    match model.Quiz with
    | Some quiz -> {model with Quiz = Some <| f quiz}
    | None -> model

let connection isOk model =
    {model with IsConnectionOk = isOk}

let init (api:IAudApi): Model * Cmd<Msg> =
    {Quiz = None; ActiveTab = Question; Error = ""; TimeDiff = TimeSpan.Zero; SseSource = None; IsConnectionOk = false;
        History = []; TeamResults = []; QuestionResults = []} |> apiCmd api.getQuiz () GetQuizRsp Exn

let update (api:IAudApi) (user:AudUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetQuizRsp {Value = Ok res; ST = st} -> {cm with Quiz = Some res; TimeDiff = timeDiff st} |> subscribe user.QuizId |> setupCountdown
    | CountdownTick _ -> cm |> noCmd |> setupCountdown
    | QuizChanged evt -> cm |> updateQuiz (fun quiz -> {quiz with QS = evt.QS; TC = evt.T}) |> noCmd |> setupCountdown
    | ChangeTab Question -> {cm with ActiveTab = Question} |> ok |> noCmd
    | ChangeTab History -> {cm with ActiveTab = History} |> ok |> apiCmd api.getHistory () GetHistoryResp Exn
    | ChangeTab Results -> {cm with ActiveTab = Results} |> ok |> apiCmd api.getResults () GetResultsResp Exn
    | GetHistoryResp {Value = Ok res} -> {cm with History = res} |> noCmd
    | GetResultsResp {Value = Ok res} -> {cm with TeamResults = res.Teams; QuestionResults = res.Questions} |> noCmd
    | SourceError _ -> cm |> connection false |> noCmd
    | Heartbeat -> cm |> connection true |> noCmd
    | Err txt -> cm |> error txt |> noCmd
    | Exn ex -> cm |> error ex.Message |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (model : Model) =
    match model.Quiz with
    | Some quiz -> quizView dispatch model quiz
    | None when model.Error = "" -> str "Initializing..."
    | None -> str model.Error

let quizView (dispatch : Msg -> unit) (model:Model) (quiz:QuizCard) =
    let serverTime = serverTime model.TimeDiff
    let secondsLeft, isCountdownActive =
        match quiz.TC with
        | Some q -> q.SecondsLeft serverTime, q.IsCountdownActive serverTime
        | None -> 0, false

    div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
        div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [

            MainTemplates.mixlrFrame quiz.Mxlr
            MainTemplates.playTitle quiz.QN quiz.Img model.IsConnectionOk quiz.Mxlr.IsNone

            h4 [Class "subtitle is-4" ] [Fa.i [Fa.Solid.Users] [ str " audience"] ]

            div [Class "container"] [
                match model.ActiveTab with
                | History -> yield historyView dispatch model
                | Question ->
                    match quiz.QS with
                    | Live ->
                        match quiz.TC with
                        | Some tour ->
                            match tour.Slip with
                            | SS slip ->  yield MainTemplates.singleTourInfo tour.Name slip
                            | MS (name,slips) -> yield multipleQwView tour name slips
                        | None -> ()
                    | _ -> yield MainTemplates.playQuiz quiz.QS quiz.Msg
                | Results -> yield MainTemplates.resultsView None model.TeamResults

            ]
            p [Class "help is-danger"][ str model.Error ]
            div [ Style [Height "66px"]] []
        ]

        MainTemplates.playFooter (ChangeTab >> dispatch) History Question Results model.ActiveTab isCountdownActive secondsLeft
    ]

let multipleQwView tour name slips =
    div [][
        h5 [Class "subtitle is-5"] [str name]
        for (idx,slip) in slips |> List.indexed do
            match slip with
            | QW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "Question %s.%i" tour.Name (idx + 1)]
                yield! MainTemplates.imgEl slip.Img
                p [] (splitByLines slip.Txt)
                br[]
                br[]

            | AW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "Question %s.%i" tour.Name (idx + 1)]
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
                th [ ] [ str "Correct Answers" ]
                th [ ] [ str "" ]
            ]
        ]

        tbody [ ] [
            for aw in model.History |> List.rev do

                tr [ ][
                    td [] [p [][str aw.QwName]]
                    td [] [
                        p [Class "is-italic"][ str (aw.QwAw.Split('\n').[0])]
                    ]
                    td [] [ ]
                ]
        ]
    ]