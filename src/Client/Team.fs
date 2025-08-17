module rec Team

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open IconHelpers
open Fable.SimpleJson
open Elmish.React

open OpenQuiz.Shared
open Common
open TeamModels

type Tab =
    | History
    | Question
    | Results

type AnswersStatus =
    | Input
    | Sending
    | Sent
    | Failed

type AnswersSlip =
    { TourIdx: int
      Status: AnswersStatus
      Values: Map<int, (string * bool)> }

    member x.Get qwIdx =
        match x.Values.TryGetValue qwIdx with
        | true, x -> x
        | _ -> "", false

type Msg =
    | QuizCardResp of RESP<QuizCard>
    | Exn of exn
    | Reactivate
    | ChangeTab of Tab
    | UpdateAnswer of qwIdx: int * txt: string
    | ToggleJeopardy of qwIdx: int
    | CountdownTick of {| QwIndex: int |}
    | QuizChanged of QuizChangedEvent
    | AnswerResponse of RESP<unit>
    | GetHistoryResp of RESP<TeamHistoryRecord list>
    | ConnectionErrors of string array
    | Vote of qwKey: QwKey * bool option
    | VoteResponse of RESP<unit>


type Model =
    { IsActive: bool
      Quiz: QuizCard option
      Answers: AnswersSlip option
      ActiveTab: Tab
      Error: string
      TimeDiff: TimeSpan
      History: TeamHistoryRecord list }

    member x.CurrentTour =
        match x.Quiz with
        | Some quiz -> quiz.TC
        | None -> None

    member x.IsCountdownActive =
        match x.Quiz with
        | Some quiz ->
            match quiz.TC with
            | Some qw -> quiz.QS = Live && qw.IsCountdownActive(serverTime x.TimeDiff)
            | None -> false
        | None -> false

let error txt model = { model with Error = txt }

let ok model = { model with Error = "" }

let startup user quiz serverTime model =
    { model with
        IsActive = true
        Quiz = Some quiz
        TimeDiff = timeDiff serverTime
        Answers =
            Infra.loadFromLocalStorage<AnswersSlip> (awStorageKey user)
            |> Option.defaultValue
                { TourIdx = quiz.TC |> Option.map (fun tour -> tour.Idx) |> Option.defaultValue -1
                  Status = if quiz.Aw.Count > 0 then Sent else Input
                  Values = quiz.Aw }
            |> Some }

let updateQuiz (f: QuizCard -> QuizCard) model =
    match model.Quiz with
    | Some quiz -> { model with Quiz = Some <| f quiz }
    | None -> model

let initAnswers (model: Model, cmd: Cmd<Msg>) =
    let createAnswers (tour: TourCard) timeDiff =
        { TourIdx = tour.Idx
          Status =
            if tour.IsCountdownFinished(serverTime timeDiff) then
                Failed
            else
                Input
          Values = Map.empty }

    match model.CurrentTour with
    | Some tour ->
        match model.Answers with
        | Some aw when aw.TourIdx <> tour.Idx ->
            { model with
                Answers = Some(createAnswers tour model.TimeDiff) }
        | None ->
            { model with
                Answers = Some(createAnswers tour model.TimeDiff) }
        | _ -> model
    | None -> { model with Answers = None }
    , cmd

let setupCountdown (model: Model, cmd: Cmd<Msg>) =
    match model.IsCountdownActive with
    | true ->
        match model.CurrentTour with
        | Some qw -> model, timeoutCmd (CountdownTick {| QwIndex = qw.Idx |}) 1000
        | None -> model |> noCmd
    | false -> model |> noCmd
    |> batchCmd cmd

let subscribe quizId (model: Model, cmd: Cmd<Msg>) =
    match model.Quiz with
    | Some quiz ->
        let subUpdate dispatch =
            AppSync.subscribe quizId quiz.LT (QuizChanged >> dispatch) (ConnectionErrors >> dispatch)

        model, Cmd.batch [ cmd; Cmd.ofEffect subUpdate ]
    | None -> model, cmd

let unsubscribe (model: Model) = AppSync.unsubscribe ()

let applyEvent (evt: QuizChangedEvent) (model: Model) =
    model
    |> updateQuiz (fun quiz ->
        { quiz with
            QS = evt.QS
            TC = evt.T
            Url = evt.Url })

let answerStatus satus model =
    match model.Answers with
    | Some aw ->
        { model with
            Answers = Some { aw with Status = satus } }
    | _ -> model

let answerText qwIdx txt model =
    match model.Answers with
    | Some aw when aw.Status = Input ->
        let (_, jpd) = aw.Get qwIdx

        { model with
            Answers =
                Some
                    { aw with
                        Values = aw.Values.Add(qwIdx, (txt, jpd)) } }
    | _ -> model

let toggleJpd qwIdx model =
    match model.Answers with
    | Some aw when aw.Status = Input ->
        let (txt, jpd) = aw.Get qwIdx

        { model with
            Answers =
                Some
                    { aw with
                        Values = aw.Values.Add(qwIdx, (txt, not jpd)) } }
    | _ -> model

let sendAnswer api (model: Model) =
    let answerCmd api model =
        match model.Answers with
        | Some aw ->
            let req =
                aw.Values
                |> Map.toList
                |> List.choose (fun (qwIdx, (txt, jpd)) ->
                    if not (String.IsNullOrEmpty txt) then
                        Some({ TourIdx = aw.TourIdx; QwIdx = qwIdx }, (txt, jpd))
                    else
                        None)
                |> Map.ofList

            if req.Count > 0 then
                model |> answerStatus Sending |> apiCmd api.answers req AnswerResponse Exn
            else
                model |> answerStatus Sent |> noCmd
        | _ -> model |> noCmd

    match model.Answers with
    | Some aw when aw.Status = Input ->
        match model.CurrentTour with
        | Some tour when
            (aw.TourIdx <> tour.Idx)
            || (tour.TS = Countdown && not (tour.IsCountdownActive(serverTime model.TimeDiff)))
            || (tour.TS = Settled)
            ->
            model |> answerCmd api
        | _ -> model |> noCmd
    | _ -> model |> noCmd

let awStorageKey (user: TeamUser) =
    sprintf "aw-%i-%i" user.QuizId user.TeamId

let saveAnswer (user: TeamUser) (model: Model) =
    match model.Answers with
    | Some aw -> Infra.saveToLocalStorage (awStorageKey user) aw
    | _ -> ()

    model

let deleteAnswer (user: TeamUser) (model: Model) =
    Infra.removeFromLocalStorage (awStorageKey user)
    model

let updateVote api qwKey vote (model: Model) =
    { model with
        History =
            model.History
            |> List.map (fun r -> if r.QwKey = qwKey then { r with Vote = vote } else r) }


let init (api: ITeamApi) (user: TeamUser) : Model * Cmd<Msg> =
    AppSync.configure user.AppSyncCfg.Endpoint user.AppSyncCfg.Region user.AppSyncCfg.ApiKey

    { IsActive = true
      Quiz = None
      Answers = None
      ActiveTab = Question
      Error = ""
      TimeDiff = TimeSpan.Zero
      History = [] }
    |> apiCmd api.getState () QuizCardResp Exn

let update (api: ITeamApi) (user: TeamUser) (msg: Msg) (cm: Model) : Model * Cmd<Msg> =
    match msg with
    | Reactivate -> cm |> apiCmd api.takeActiveSession () QuizCardResp Exn
    | QuizCardResp { Value = Ok res; ST = st } ->
        cm
        |> startup user res st
        |> ok
        |> sendAnswer api
        |> subscribe user.QuizId
        |> initAnswers
        |> setupCountdown
    | CountdownTick _ -> cm |> sendAnswer api |> setupCountdown
    | QuizChanged evt -> cm |> applyEvent evt |> sendAnswer api |> initAnswers |> setupCountdown
    | AnswerResponse { Value = Ok _ } -> cm |> deleteAnswer user |> answerStatus Sent |> ok |> noCmd
    | UpdateAnswer(qwIdx, txt) -> cm |> answerText qwIdx txt |> saveAnswer user |> noCmd
    | ToggleJeopardy qwIdx -> cm |> toggleJpd qwIdx |> saveAnswer user |> noCmd
    | ChangeTab Question -> { cm with ActiveTab = Question } |> noCmd
    | ChangeTab History -> { cm with ActiveTab = History } |> apiCmd api.getHistory () GetHistoryResp Exn
    | ChangeTab Results -> { cm with ActiveTab = Results } |> noCmd
    | GetHistoryResp { Value = Ok res } -> { cm with History = res } |> noCmd
    | Vote(qwKey, vote) ->
        cm
        |> updateVote api qwKey vote
        |> apiCmd api.vote {| Key = qwKey; Vote = vote |} VoteResponse Exn
    | VoteResponse { Value = Ok _ } -> cm |> noCmd
    | ConnectionErrors errors -> cm |> error (String.Join("/", errors)) |> noCmd
    | Exn ex when ex.Message = Errors.SessionIsNotActive ->
        unsubscribe cm
        { cm with IsActive = false } |> noCmd
    | Exn ex -> cm |> error ex.Message |> noCmd
    | Err txt -> cm |> error txt |> noCmd
    | _ -> cm |> noCmd

let view (dispatch: Msg -> unit) (user: TeamUser) (settings: Settings) (model: Model) (l10n: L10n.TeamL10n) =
    match model.IsActive with
    | true ->
        match model.Quiz with
        | Some quiz -> activeView dispatch user settings quiz model l10n
        | None -> str model.Error
    | false -> notActiveView dispatch user model.Error l10n

let notActiveView (dispatch: Msg -> unit) (user: TeamUser) error l10n =
    section
        [ Class "hero is-danger is-medium is-fullheight" ]
        [ div
              [ Class "hero-body" ]
              [ div
                    [ Class "container has-text-centered is-fluid" ]
                    [ p [ Class "title" ] [ str l10n.NotActive ]
                      p [ Class "title" ] [ str user.QuizName ]
                      p [ Class "subtitle" ] [ str user.TeamName ]
                      p [ Class "subtitle" ] [ str l10n.AnotherSession ]
                      a [ Class "button is-large"; OnClick(fun _ -> dispatch (Reactivate)) ] [ str l10n.UseThisDevice ]
                      p [ Class "help is-white" ] [ str error ] ] ] ]

let activeView (dispatch: Msg -> unit) (user: TeamUser) (settings: Settings) quiz model l10n =
    let serverTime = serverTime model.TimeDiff

    let secondsLeft, isCountdownActive, isCountdownFinished =
        match quiz.TC with
        | Some tour ->
            tour.SecondsLeft serverTime, tour.IsCountdownActive serverTime, tour.IsCountdownFinished serverTime
        | None -> 0, false, false

    div
        [ Style
              [ Width "100%"
                Height "100%"
                MinWidth "375px"
                TextAlign TextAlignOptions.Center
                Position PositionOptions.Relative ] ]
        [ div
              [ Style
                    [ OverflowY OverflowOptions.Auto
                      Position PositionOptions.Absolute
                      Top "0"
                      Width "100%" ] ]
              [

                div
                    [ Class "container"; Style [ MaxWidth "800px" ] ]
                    [ MainTemplates.playTitle settings.MediaHost quiz.Img quiz.Mxlr quiz.Url

                      h3 [ Class "title is-3" ] [ str user.QuizName ]
                      h4 [ Class "subtitle is-4" ] [ str user.TeamName ]

                      match quiz.TS with
                      | New -> div [ Class "notification is-white" ] [ str l10n.WaitingForConfirmation ]
                      | Admitted ->
                          div
                              []
                              [ match model.ActiveTab with
                                | History -> yield historyView dispatch model l10n
                                | Question ->
                                    match quiz.QS, model.Answers with
                                    | Live, Some answers ->
                                        yield
                                            quiestionView
                                                dispatch
                                                settings
                                                quiz
                                                answers
                                                isCountdownActive
                                                isCountdownFinished
                                                l10n
                                    | _ -> yield MainTemplates.playQuiz quiz.QS quiz.Msg l10n.Common
                                | Results ->
                                    yield
                                        MainTemplates.resultsViewEmb
                                            user.QuizId
                                            quiz.RT
                                            (Some user.TeamId)
                                            settings.MediaHost ]
                      | Rejected ->
                          div
                              [ Class "notification is-white" ]
                              [ span
                                    [ Class "has-text-danger has-text-weight-bold" ]
                                    [ str l10n.RegistrationHasBeenRejected ] ] ]
                p [ Class "help is-danger" ] [ str model.Error ]
                div [ Style [ Height "66px" ] ] []

                if quiz.TS = Admitted then
                    MainTemplates.playFooter
                        (ChangeTab >> dispatch)
                        History
                        Question
                        Results
                        model.ActiveTab
                        isCountdownActive
                        secondsLeft
                        l10n.Common

                    MainTemplates.playSounds settings.MediaHost secondsLeft ] ]

let quiestionView (dispatch: Msg -> unit) (settings: Settings) quiz answers isCountdownActive isCountdownFinished l10n =
    div
        []
        [ match quiz.TC with
          | Some tour ->
              match tour.Slip with
              | SS slip ->
                  yield singleQwView dispatch settings tour slip answers isCountdownActive isCountdownFinished l10n
              | MS(name, slips) ->
                  yield multipleQwView dispatch settings tour name slips answers isCountdownFinished l10n
          | None -> () ]

let answerStatusIcon status txt =
    span
        [ Class "icon is-large is-right has-text-black" ]
        [ Fa.i
              [ match status with
                | Input -> Fa.Solid.Question
                | Sending -> Fa.Solid.Spinner
                | Sent when String.IsNullOrEmpty txt -> Fa.Solid.Exclamation
                | Sent -> Fa.Solid.Check
                | Failed -> Fa.Solid.Exclamation ]
              [] ]

let jeopardyControl dispatch jpd readOnly (l10n: L10n.TeamL10n) =
    p
        [ Class "control" ]
        [ a
              [ classList [ "button", true; "has-text-grey-light", not jpd; "has-text-danger", jpd ]
                Title l10n.Jeopardy
                ReadOnly readOnly
                OnClick(fun _ -> dispatch <| ToggleJeopardy 0) ]
              [ span [] [ Fa.i [ Fa.Solid.Paw ] []; str (" " + l10n.Jeopardy) ] ] ]

let awArea dispatch aw jpd status withChoice readOnly l10n =
    div
        [ Class "control has-icons-right" ]
        [ if withChoice then
              jeopardyControl dispatch jpd readOnly l10n

          textarea
              [ Class "textarea"
                MaxLength 64.0
                ReadOnly readOnly
                valueOrDefault aw
                OnChange(fun ev -> dispatch <| UpdateAnswer(0, ev.Value)) ]
              []
          answerStatusIcon status aw ]

let singleQwView
    dispatch
    (settings: Settings)
    tour
    slip
    answers
    isCountdownActive
    isCountdownFinished
    (l10n: L10n.TeamL10n)
    =
    div
        []
        [ MainTemplates.singleTourInfo settings.MediaHost tour.Name slip l10n.Common

          let (txt, jpd) = answers.Get 0

          match slip with
          | X3 -> ()
          | QW slip ->
              if isCountdownActive || isCountdownFinished then
                  label [ Class "label" ] [ str l10n.YourAnswer ]

                  match slip.Choices with
                  | None -> awArea dispatch txt jpd answers.Status slip.Ch isCountdownFinished l10n
                  | Some list ->
                      if slip.Ch then
                          jeopardyControl dispatch jpd isCountdownFinished l10n

                      br []

                      div
                          [ Class "columns is-centered" ]
                          [ div
                                [ Class "column is-half mx-3" ]
                                [ ul
                                      []
                                      [ for ch in list do
                                            li
                                                []
                                                [ div
                                                      [ Class "control has-icons-left has-icons-right" ]
                                                      [ a
                                                            [ classList
                                                                  [ "button", true
                                                                    "is-fullwidth", true
                                                                    "is-info", txt = ch ]
                                                              OnClick(fun _ -> dispatch <| UpdateAnswer(0, ch)) ]
                                                            [ str ch ]
                                                        if txt = ch then
                                                            answerStatusIcon answers.Status txt

                                                        ]
                                                  br [] ] ] ] ]

          | AW slip ->
              match slip.Aw with
              | OpenAnswer _ ->
                  label [ Class "label" ] [ str l10n.YourAnswer ]
                  awArea dispatch txt jpd answers.Status slip.Ch true l10n
              | ChoiceAnswer list ->
                  if slip.Ch then
                      jeopardyControl dispatch jpd true l10n

                  br []

                  div
                      [ Class "columns is-centered" ]
                      [ div
                            [ Class "column is-half mx-3" ]
                            [ ul
                                  []
                                  [ for ch in list do
                                        li
                                            []
                                            [ div
                                                  [ Class "control has-icons-left has-icons-right" ]
                                                  [ a
                                                        [ classList
                                                              [ "button", true
                                                                "is-fullwidth", true
                                                                "is-success", ch.IsCorrect && txt = ch.Text
                                                                "is-danger", (not ch.IsCorrect) && txt = ch.Text ] ]
                                                        [ str ch.Text ]

                                                    if ch.IsCorrect then
                                                        span
                                                            [ Class "icon is-large is-left has-text-black" ]
                                                            [ icon Icons.grin ]

                                                    if txt = ch.Text then
                                                        answerStatusIcon answers.Status txt ]
                                              br [] ] ] ] ] ]

let awInput dispatch idx aw jpd status withChoice readOnly (l10n: L10n.TeamL10n) =
    div
        [ Style [ MaxWidth "320px"; Display DisplayOptions.InlineBlock ] ]
        [ div
              [ Class "field has-addons" ]
              [ if withChoice then
                    p
                        [ Class "control" ]
                        [ a
                              [ classList [ "button", true; "has-text-grey-light", not jpd; "has-text-danger", jpd ]
                                Title l10n.Jeopardy
                                ReadOnly readOnly
                                OnClick(fun _ -> dispatch <| ToggleJeopardy idx) ]
                              [ Fa.i [ Fa.Solid.Paw ] [] ] ]
                p
                    [ Class "control has-icons-right" ]
                    [ input
                          [ Class "input"
                            Type "text"
                            MaxLength 64.0
                            Placeholder l10n.YourAnswer
                            ReadOnly readOnly
                            valueOrDefault aw
                            OnChange(fun ev -> dispatch <| UpdateAnswer(idx, ev.Value)) ]

                      answerStatusIcon status aw ] ] ]

let multipleQwView dispatch settings tour name slips answers isCountdownFinished l10n =
    div
        []
        [ h5 [ Class "subtitle is-5" ] [ str name ]
          for (idx, slip) in slips |> List.indexed do
              let (aw, jpd) = answers.Get idx

              match slip with
              | QW slip ->
                  p [ Class "has-text-weight-semibold" ] [ str <| sprintf "%s %s.%i" l10n.Question tour.Name (idx + 1) ]
                  yield! MainTemplates.mediaEl settings.MediaHost slip.Media true
                  p [] (splitByLines slip.Txt)
                  awInput dispatch idx aw jpd answers.Status slip.Ch isCountdownFinished l10n
                  br []
                  br []

              | AW slip ->
                  p [ Class "has-text-weight-semibold" ] [ str <| sprintf "%s %s.%i" l10n.Question tour.Name (idx + 1) ]
                  awInput dispatch idx aw jpd answers.Status slip.Ch isCountdownFinished l10n

                  p
                      [ Class "has-text-weight-light is-family-secondary is-size-6" ]
                      [ str <| l10n.CorrectAnswer + ": "
                        str (slip.Aw.ToRawString().Split('\n').[0]) ]

                  yield! MainTemplates.mediaEl settings.MediaHost slip.Media true
                  p [ Class "is-italic has-text-weight-light is-family-secondary is-size-7" ] (splitByLines slip.Com)
                  br []

              | X3 -> str "x3" ]

let historyView dispatch model l10n =
    table
        [ Class "table is-hoverable is-fullwidth" ]
        [ thead
              []
              [ tr
                    []
                    [ th [ Style [ Width "30px" ] ] [ str "#" ]
                      th [ Style [ TextAlign TextAlignOptions.Left ] ] [ str l10n.Answer ]
                      th [] [ str l10n.Points ]
                      th [ Title l10n.Vote; Style [ Width "84px" ] ] [ str "@" ] ] ]

          tbody
              []
              [ for aw in model.History |> List.sortByDescending (fun aw -> aw.QwKey) do
                    let modifiers =
                        match aw.Result with
                        | Some v when v > 0m -> [ "has-text-success", true ]
                        | Some _ -> [ "has-text-danger", true ]
                        | None -> []

                    tr
                        []
                        [ td [] [ p [ classList modifiers ] [ str aw.QwName ] ]
                          td
                              [ Style [ TextAlign TextAlignOptions.Left ] ]
                              [ p
                                    [ classList modifiers ]
                                    [ if aw.AwJpd then
                                          Fa.i [ Fa.Solid.Paw; Fa.PullRight ] []
                                      str (defaultArg aw.AwTxt "") ] ]
                          td
                              []
                              [ let txt =
                                    match aw.Result with
                                    | Some d when d > 0m -> (sprintf "+%M" d)
                                    | Some d -> d.ToString()
                                    | None -> ""

                                p [ classList modifiers ] [ str txt ] ]
                          td
                              []
                              [ if aw.AnswerReceived then
                                    let isUpSelected = aw.Vote |> Option.defaultValue false
                                    let isDownSelected = aw.Vote |> Option.defaultValue true |> not

                                    button
                                        [ Class "button is-small is-white"
                                          OnClick(fun _ ->
                                              Vote(aw.QwKey, (if isUpSelected then None else Some true)) |> dispatch) ]
                                        [ span
                                              [ Class
                                                <| "icon "
                                                   + if isUpSelected then "has-text-success" else "has-text-grey" ]
                                              [ Fa.i [ Fa.Regular.ThumbsUp ] [] ] ]

                                    button
                                        [ Class "button is-small is-white"
                                          OnClick(fun _ ->
                                              Vote(aw.QwKey, (if isDownSelected then None else Some false)) |> dispatch) ]
                                        [ span
                                              [ Class
                                                <| "icon "
                                                   + if isDownSelected then
                                                         "has-text-danger"
                                                     else
                                                         "has-text-grey" ]
                                              [ Fa.i [ Fa.Regular.ThumbsDown ] [] ] ] ] ]

                    if aw.QwAw <> "" then
                        tr
                            []
                            [ td [] []
                              td
                                  [ ColSpan 3; Style [ TextAlign TextAlignOptions.Left ] ]
                                  [ span
                                        [ Class "is-italic has-text-weight-light is-family-secondary is-size-7" ]
                                        [ str <| l10n.CorrectAnswer + ": "; str (aw.QwAw.Split('\n').[0]) ] ] ] ] ]