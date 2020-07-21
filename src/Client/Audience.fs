module rec Aud

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fable.SimpleJson
open Elmish.React

open Shared
open Shared.AudModels
open Common

type Tab = History | Question | Results

type Msg =
    | GetQuizRsp of RESP<QuizCard>
    | ChangeTab of Tab
    | QuizChanged of QuizChangedEvent
    | CountdownTick of {|QwIndex: int|}
    | GetHistoryResp of RESP<HistoryRecord list>
    | ConnectionErrors of string array
    | Exn of exn

type Model = {
    Quiz : QuizCard option
    ActiveTab : Tab
    Error : string
    TimeDiff: TimeSpan
    History : HistoryRecord list
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
        let subUpdate dispatch =
            AppSync.subscribe quizId quiz.LT (QuizChanged >> dispatch) (ConnectionErrors >> dispatch)

        model, Cmd.ofSub subUpdate
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

let init (api:IAudApi) (user:AudUser): Model * Cmd<Msg> =
    AppSync.configure user.AppSyncCfg.Endpoint user.AppSyncCfg.Region user.AppSyncCfg.ApiKey
    {Quiz = None; ActiveTab = Question; Error = ""; TimeDiff = TimeSpan.Zero;
        History = []} |> apiCmd api.getQuiz () GetQuizRsp Exn

let update (api:IAudApi) (user:AudUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetQuizRsp {Value = Ok res; ST = st} -> {cm with Quiz = Some res; TimeDiff = timeDiff st} |> subscribe user.QuizId |> setupCountdown
    | CountdownTick _ -> cm |> noCmd |> setupCountdown
    | QuizChanged evt -> cm |> updateQuiz (fun quiz -> {quiz with QS = evt.QS; TC = evt.T; Url = evt.Url}) |> noCmd |> setupCountdown
    | ChangeTab Question -> {cm with ActiveTab = Question} |> ok |> noCmd
    | ChangeTab History -> {cm with ActiveTab = History} |> ok |> apiCmd api.getHistory () GetHistoryResp Exn
    | ChangeTab Results -> {cm with ActiveTab = Results} |> ok |> noCmd
    | GetHistoryResp {Value = Ok res} -> {cm with History = res} |> noCmd
    | Err txt -> cm |> error txt |> noCmd
    | Exn ex -> cm |> error ex.Message |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) settings (model : Model) (user:AudUser) (l10n:L10n.AudienceL10n)=
    match model.Quiz with
    | Some quiz -> quizView dispatch settings model user.QuizId quiz l10n
    | None when model.Error = "" -> str "Initializing..."
    | None -> str model.Error

let quizView (dispatch : Msg -> unit) settings (model:Model) (quizId:int) (quiz:QuizCard) l10n =
    let serverTime = serverTime model.TimeDiff
    let secondsLeft, isCountdownActive =
        match quiz.TC with
        | Some q -> q.SecondsLeft serverTime, q.IsCountdownActive serverTime
        | None -> 0, false

    div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
        div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [

            div [Class "container"; Style[MaxWidth "800px"]] [
                yield MainTemplates.playTitle settings.MediaHost quiz.Img quiz.Mxlr quiz.Url

                yield h3 [Class "title is-3"] [ str quiz.QN ]
                yield h4 [Class "subtitle is-4" ] [Fa.i [Fa.Solid.Users] [ str <| " " + l10n.Audience] ]

                match model.ActiveTab with
                | History -> yield historyView dispatch model l10n
                | Question ->
                    match quiz.QS with
                    | Live ->
                        match quiz.TC with
                        | Some tour ->
                            match tour.Slip with
                            | SS slip ->  yield singleQwView settings tour slip l10n
                            | MS (name,slips) -> yield multipleQwView settings tour name slips l10n
                        | None -> ()
                    | _ -> yield MainTemplates.playQuiz quiz.QS quiz.Msg l10n.Common
                | Results -> yield MainTemplates.resultsViewEmb  quizId quiz.LT None

            ]
            p [Class "help is-danger"][ str model.Error ]
            div [ Style [Height "66px"]] []
        ]

        MainTemplates.playFooter (ChangeTab >> dispatch) History Question Results model.ActiveTab isCountdownActive secondsLeft l10n.Common
    ]

let singleQwView (settings:Settings) tour (slip:Shared.SingleSlipCard) l10n =
    div[][
        MainTemplates.singleTourInfo settings.MediaHost tour.Name slip l10n.Common

        match slip with
        | X3 -> ()
        | QW slip ->
            match slip.Choices with
            | None -> ()
            | Some list ->
                br[]
                div [Class "columns is-centered"][
                    div [Class "column is-half mx-3"][
                        ul[][
                            for ch in list do
                                li [][
                                    div [Class "control has-icons-left has-icons-right"][
                                        a [Class "button is-fullwidth"][str ch]
                                    ]
                                    br[]
                                ]
                        ]
                    ]
                ]

        | AW slip ->
            match slip.Aw with
            | OpenAnswer _ -> ()
            | ChoiceAnswer list ->
                br[]
                div [Class "columns is-centered"][
                    div [Class "column is-half mx-3"][
                        ul[][
                            for ch in list do
                                li [][
                                    div [Class "control has-icons-left"][
                                        a [Class "button is-fullwidth"][str ch.Text]

                                        if ch.IsCorrect then
                                            span [Class "icon is-large is-left has-text-black"][
                                                Fa.i [Fa.Regular.Grin] [ ]
                                            ]
                                    ]
                                    br[]
                                ]
                        ]
                    ]
                ]
    ]

let multipleQwView (settings:Settings) tour name slips l10n =
    div [][
        h5 [Class "subtitle is-5"] [str name]
        for (idx,slip) in slips |> List.indexed do
            match slip with
            | QW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "%s %s.%i" l10n.Question tour.Name (idx + 1)]
                yield! MainTemplates.mediaEl settings.MediaHost slip.Media true
                p [] (splitByLines slip.Txt)
                br[]
                br[]

            | AW slip ->
                p [Class "has-text-weight-semibold"] [str <| sprintf "%s %s.%i" l10n.Question tour.Name (idx + 1)]
                p [Class "has-text-weight-light is-family-secondary is-size-6"][
                    str <| l10n.CorrectAnswer + ": "
                    str (slip.Aw.ToRawString().Split('\n').[0])
                ]
                yield! MainTemplates.mediaEl settings.MediaHost slip.Media true
                p [Class "is-italic has-text-weight-light is-family-secondary is-size-7"] (splitByLines slip.Com)
                br[]

            | X3 -> str "x3"
    ]

let historyView dispatch model l10n =
    table [Class "table is-hoverable is-fullwidth"][
        thead [ ] [
            tr [ ] [
                th [Style [Width "30px"] ] [ str "#" ]
                th [ ] [ str l10n.CorrectAnswers ]
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