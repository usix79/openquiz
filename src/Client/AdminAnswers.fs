module rec AdminAnswers

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Elmish.React

open Shared
open Shared.AdminModels
open Common

let TEAMsOnPAGE = 250

type Msg =
    | GetAnswersResp of RESP<AnswersBundle>
    | DeleteError of string
    | Exn of exn
    | SelectQuestion of QwKey
    | ToggleShowAllTeams
    | SelectRange of Range option
    | ResultChanged of teamId:int*key:QwKey*res:string
    | ResultWithoutAnswer of teamId:int*key:QwKey*res:string
    | ResultsUpdated of RESP<unit>

type Model = {
    Bundle : AnswersBundle option
    Errors : Map<string, string>
    IsLoading : bool
    CurrentQuestion : {| Key:QwKey; LastReview:DateTime |} option
    ShowAllTeams : bool
    TimeDiff: TimeSpan
    LastReviews : Map<QwKey, DateTime>
    SessionStart : DateTime
    Range : Range option
}

let getActualPoints (jpd:bool) (qw:QuestionRecord) =
    match qw.JpdPt with
    | Some pt when not qw.Ch -> pt
    | Some pt when qw.Ch && jpd -> pt
    | _ -> qw.Pt

let addError txt model =
    {model with Errors = model.Errors.Add(Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let loading  model =
    {model with IsLoading = true}

let editing model =
    {model with IsLoading = false}

let reviewTime (key:QwKey) (model : Model)  =
    match model.LastReviews.TryGetValue key with
    | true, dt -> dt
    | false, _ -> model.SessionStart

let isNewAnswer (aw:Answer) (lastReview:DateTime)=
    match aw.Res with
    | None when compareDates aw.RT lastReview > 0.0 -> true
    | Some _ when aw.IsA && compareDates (defaultArg aw.UT DateTime.MinValue) lastReview > 0.0 -> true
    | _ -> false

let questionStats (key:QwKey) (model:Model)  =

    let lastReview = model |> reviewTime key

    let f (r:{|Total:int; Win:int; Lose:int; Open:int; New:int|}) (team:TeamAnswersRecord) =
        match team.Awrs.TryGetValue key with
        | true, aw ->
            let res = aw.Res
            {|r with
                Total = r.Total + 1
                Win = r.Win + if res.IsSome && res.Value > 0m then 1 else 0
                Lose = r.Lose + if res.IsSome && res.Value <= 0m then 1 else 0
                Open = r.Open + if res.IsNone then 1 else 0
                New = r.New + if isNewAnswer aw lastReview then 1 else 0
             |}
        | _ -> r

    match model.Bundle with Some bundle -> bundle.Teams | _ -> []
    |> List.fold f {|Total = 0; Win = 0; Lose = 0; Open = 0; New = 0|}

let selectQuestion idx (model:Model) =
    let currentQuestion = {|Key = idx; LastReview = model |> reviewTime idx|}
    let lastReviews = model.LastReviews.Add(idx, serverTime model.TimeDiff)
    saveToSessionStorage "AnswersLR" lastReviews
    {model with CurrentQuestion = Some currentQuestion; LastReviews = lastReviews}

let setResults (api:IAdminApi) (teamId:int) (key:QwKey) (v :string) (model:Model) =
    let res =
        match System.Decimal.TryParse v with
        | (true,value) -> Some value
        | _ -> None

    match model.Bundle with
    | Some bundle ->
        match bundle.GetAw teamId key with
        | Some aw ->
            let answersToResult = bundle.FindAnswers key aw.Txt aw.Jpd
            let answersToUpdate =
                answersToResult
                |> List.map (fun (teamId,aw) -> teamId, {aw with Res = res; IsA = false; UT = Some <| serverTime model.TimeDiff })
                |> Map.ofList
            let answersToSend =
                answersToResult
                |> List.map (fun (teamId,_) -> {|QwKey = key; Res = res; TeamId = teamId|})

            { model with Bundle = Some <| bundle.UpdateAnswers key answersToUpdate }
            |> loading
            |> apiCmd api.updateResults answersToSend ResultsUpdated Exn
        | None -> model |> noCmd
    | None -> model |> noCmd

let setResultWithoutAnswer (api:IAdminApi) (teamId:int) (key:QwKey) (v :string) (model:Model) =
    let res =
        match System.Decimal.TryParse v with
        | (true,value) -> Some value
        | _ -> None

    match model.Bundle with
    | Some bundle ->
        let aw = {  Txt = ""
                    Jpd = false
                    RT = DateTime.UtcNow
                    Res = res
                    IsA = false
                    UT = None }

        let answersToResult = [teamId, aw]

        let answersToUpdate =
            answersToResult
            |> List.map (fun (teamId,aw) -> teamId, {aw with Res = res; IsA = false; UT = Some <| serverTime model.TimeDiff })
            |> Map.ofList

        let answersToSend =
            answersToResult
            |> List.map (fun (teamId,_) -> {|QwKey = key; Res = res; TeamId = teamId|})

        { model with Bundle = Some <| bundle.UpdateAnswers key answersToUpdate }
        |> loading
        |> apiCmd api.updateResultsWithoutAnswer answersToSend ResultsUpdated Exn
    | None -> model |> noCmd

let rangeFromHashQuery () =
    let (_, hashQuery) = Infra.getUrlHashParts()
    let m = System.Text.RegularExpressions.Regex.Match (hashQuery, ":(\\d+)-(\\d+)")
    if m.Success then
        Some {From = Int32.Parse((m.Groups.Item 1).Value); To = Int32.Parse((m.Groups.Item 2).Value)}
    else None

let setRange api range (model:Model) =
    let hashQuery =
        match range with
        | Some range -> sprintf ":%i-%i" range.From range.To
        | None -> ""
    {model with Range = range; IsLoading = true}, Cmd.batch[
        apiCmd' api.getAnswers range GetAnswersResp Exn
        Infra.urlWithNewHashQuery hashQuery |> Navigation.Navigation.modifyUrl]

let init (api:IAdminApi) user : Model*Cmd<Msg> =
    let lastReviews =
        match loadFromSessionStorage<Map<QwKey, DateTime>> "AnswersLR" with
        | Some map -> map
        | None -> Map.empty

    let sessionStart =
        match loadFromSessionStorage<DateTime> "AnswersSS" with
        | Some sst -> sst
        | None ->
            let sst = DateTime.UtcNow
            saveToSessionStorage "AnswersSS" sst
            sst

    let range = rangeFromHashQuery()

    {
        Bundle = None;
        Errors = Map.empty;
        IsLoading = true;
        TimeDiff = TimeSpan.Zero
        LastReviews = lastReviews
        CurrentQuestion = None
        ShowAllTeams = false
        SessionStart = sessionStart
        Range = range

    } |> apiCmd api.getAnswers range GetAnswersResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetAnswersResp {Value = Ok res; ST = st } -> {cm with Bundle = Some res; TimeDiff = timeDiff st} |> editing |> noCmd
    | SelectQuestion key -> cm |> selectQuestion key |> noCmd
    | ToggleShowAllTeams -> {cm with ShowAllTeams = not cm.ShowAllTeams} |> noCmd
    | SelectRange range -> cm |> setRange api range
    | ResultChanged (teamId, key, v) -> cm |> setResults api teamId key v
    | ResultWithoutAnswer (teamId, key, v) -> cm |> setResultWithoutAnswer api teamId key v
    | ResultsUpdated {Value = Ok _} -> cm |> editing |> noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    div [Class "columns"][
        div [Class "column is-narrow"][
            div [][
                match model.Bundle with
                | Some bundle -> yield menuView dispatch bundle model
                | None -> ()
            ]
        ]
        div [Class "column is-8"][
            match model.Bundle, model.IsLoading with
            | Some bundle, false ->
                yield paginator dispatch bundle model.Range
                match model.CurrentQuestion with
                | Some cq ->
                    match bundle.GetQw cq.Key with
                    | Some qw ->  yield! answersTable dispatch bundle qw cq model.ShowAllTeams
                    | None -> ()
                | _ -> ()
            | _ -> ()
        ]
        div [Class "column is-2"][
            if model.IsLoading then
                button [Class "button is-loading is-large is-fullwidth is-light"][]

            for error in model.Errors do
                div [Class "notification is-danger is-light"][
                    button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                    str error.Value
                ]
        ]
    ]

let menuView dispatch (bundle:AnswersBundle) model =
    aside [Class "menu"][
        p [Class "menu-label"][str "Questions"]
        ul [Class "menu-list "][
            for qw in bundle.Questions |> List.sortBy (fun q -> q.Key) |> List.rev ->
                li [][
                    let selectedIdx = match model.CurrentQuestion with Some cqw -> cqw.Key | None -> {TourIdx = -1; QwIdx = -1}
                    a [classList ["light-item", true; "has-background-white", qw.Key = selectedIdx];
                        OnClick (fun _ -> SelectQuestion qw.Key |> dispatch)][
                        let stats = questionStats qw.Key model

                        str <| sprintf "%s - " qw.Nm
                        span [ Class "has-text-primary"] [ str <| stats.Win.ToString()]
                        str <| sprintf "/%i" stats.Total
                        if stats.New > 0 then
                            span [ Class "has-text-weight-bold"] [ str (sprintf " (%i new)" stats.New)]
                    ]
                ]
        ]
    ]

let answersTable dispatch  (bundle:AnswersBundle) (qw:QuestionRecord)  cq showAllTeams = [
    h5[Class "title is-5"] [str <| sprintf "Question: %s  \"%s\"" qw.Nm qw.Ann]
    if qw.Awr <> "" then
        p [Class "content"][
            b[][str "Answer(s): "]
            p [] (splitByLines qw.Awr)
        ]

    label [Class "checkbox"][
        input [Type "checkbox"; Checked showAllTeams; OnChange (fun _ -> dispatch ToggleShowAllTeams)]
        str " show all teams"
    ]

    table [Class "table is-hoverable is-fullwidth"][
        thead [ ] [
            tr [ ] [
                th [ ] [ str "Id" ]
                th [ ] [ str "Team" ]
                th [ ] [ str "Answer" ]
                th [ Style [ TextAlign TextAlignOptions.Center; Width "100px"] ] [ str "Points" ]
                th [ Style [ Width "50px" ]] [ str "Time" ]
            ]
        ]
        let answers =
            bundle.Teams
            |> List.map (fun team -> team, team.Awrs.TryGetValue qw.Key)
            |> List.filter (fun (_, (found, _)) -> showAllTeams || found )
            |> List.sortBy (fun (team, _) -> team.Id)

        tbody [] [
            for team, (found, aw) in answers do
                if found then yield answersRow dispatch team qw aw cq.LastReview
                else yield noAnswersRow dispatch team qw
        ]
    ]
]

let answersRow dispatch team (qw:QuestionRecord) (aw:Answer) (lastReview:DateTime) =
    let modifiers =
        match aw.Res with
        | Some res when res > 0m -> ["has-text-success", true]
        | Some res when res <= 0m -> ["has-text-danger", true]
        | None -> []
        | _ -> []

    let modifiers =
        if isNewAnswer aw lastReview then ("has-text-weight-bold", true) :: modifiers else modifiers

    tr [ ][
        td [] [span [classList modifiers] [str (team.Id.ToString())]]
        td [] [span [classList modifiers] [str team.Nm]]
        td [] [
            span [classList modifiers] [
                if aw.Jpd then Fa.i [Fa.Solid.Paw; Fa.PullRight][]
                str aw.Txt
            ]
        ]
        td [] [
            let points = getActualPoints aw.Jpd qw
            div [Class "field has-addons"][
                a [Class "button is-small is-success is-inverted";
                    OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key, points.ToString())))][
                    span [Class "icon"][ Fa.i [ Fa.Solid.PlusSquare; Fa.Size Fa.Fa2x ] [] ]
                ]
                input [Class "input is-small"; Type "number"; Style [ Width "50px" ];
                    OnChange (fun ev -> dispatch (ResultChanged (team.Id, qw.Key, ev.Value)));
                    Value (aw.Res.ToString())]
                a [Class "button is-small";
                    OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key,"")))][
                    span [Class "icon"][ Fa.i [ Fa.Regular.Circle; Fa.Size Fa.FaExtraSmall ] [] ]
                ]
                if (qw.JpdPt.IsSome) then
                    a [Class "button is-small is-danger is-inverted";
                        OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key, ((-points).ToString()))))][
                        span [Class "icon"][ Fa.i [ Fa.Solid.MinusSquare; Fa.Size Fa.Fa2x ] [] ]
                ]

            ]
        ]

        let timeSpent =
            match qw.ST with
            | Some dt -> Some(aw.RT.Subtract(dt).TotalSeconds)
            | None -> None

        match timeSpent with
        | Some seconds -> td [] [ span [classList ["has-text-danger", ((int)seconds > (qw.Sec + 20))]][str (int(seconds).ToString())]]
        | None -> td[][]
   ]

let noAnswersRow dispatch team (qw:QuestionRecord) =

    tr [ ][
        td [] [span [] [str (team.Id.ToString())]]
        td [] [span [] [str team.Nm]]
        td [] [span [ Style [FontStyle "italic"]] [str "== no answer =="]]
        td [] [
            div [] [
                input [
                    Class "input is-small"; Type "number"; Style [ Width "50px" ]
                    OnChange (fun ev -> dispatch (ResultWithoutAnswer (team.Id, qw.Key, ev.Value)))]
            ]
        ]

        td[][]
   ]

let paginator dispatch (bundle:AnswersBundle) (range: Range option) =

    nav[Class "pagination"; Role "navigation"; AriaLabel "pagination"][
        ul [Class "pagination-list"][
            match range with
            | Some range ->
                li [Class "pagination-link"; Style[Cursor "pointer"]; OnClick (fun _ -> dispatch <| SelectRange None)][str "All teams"]
                li [Class "pagination-ellipsis"][str " "]
                li [Class "pagination-link is-current"][str <| sprintf "%i-%i" range.From range.To]
            | None ->
                let totalIndexes = bundle.Teams.Length / TEAMsOnPAGE
                if totalIndexes > 0 then
                    li [Class "pagination-link is-current"][str "All teams"]
                    li [Class "pagination-ellipsis"][str " "]
                    for idx in 0 .. totalIndexes do
                        let range = {From=idx * TEAMsOnPAGE + 1; To=(idx + 1) * TEAMsOnPAGE}
                        li [Class "pagination-link"; Style[Cursor "pointer"]; OnClick (fun _ -> Some range |> SelectRange |> dispatch)][
                            str <| sprintf "%i-%i" range.From range.To]
        ]
    ]