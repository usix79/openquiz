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

type Msg =
    | GetAnswersResp of RESP<AnswersBundle>
    | DeleteError of string
    | Exn of exn
    | SelectQuestion of QwKey
    | ResultChanged of teamId:int*key:QwKey*res:string
    | ResultsUpdated of RESP<unit>


type Model = {
    Bundle : AnswersBundle option
    Errors : Map<string, string>
    IsLoading : bool
    CurrentQuestion : {|Key:QwKey; LastReview:DateTime|} option
    TimeDiff: TimeSpan
    LastReviews : Map<QwKey, DateTime>
    SessionStart : DateTime
}

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
            let answersToResult = bundle.FindAnswers key aw.Txt
            let answersToUpdate =
                answersToResult
                |> List.map (fun (teamId,aw) -> teamId, {aw with Res = res; IsA = false; UT = Some <| serverTime model.TimeDiff })
                |> Map.ofList
            let answersToSend =
                answersToResult
                |> List.map (fun (teamId,_) -> {|QwKey = key; Res = res; TeamId = teamId|})

            {model with
                Bundle = Some <| bundle.UpdateAnswers key answersToUpdate
            }
            |> loading
            |> apiCmd api.updateResults answersToSend ResultsUpdated Exn
        | None -> model |> noCmd
    | None -> model |> noCmd

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

    {
        Bundle = None;
        Errors = Map.empty;
        IsLoading = true;
        TimeDiff = TimeSpan.Zero
        LastReviews = lastReviews
        CurrentQuestion = None
        SessionStart = sessionStart
    } |> apiCmd api.getAnswers () GetAnswersResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetAnswersResp {Value = Ok res; ST = st } -> {cm with Bundle = Some res; TimeDiff = timeDiff st} |> editing |> noCmd
    | SelectQuestion key -> cm |> selectQuestion key |> noCmd
    | ResultChanged (teamId, key, v) -> cm |> setResults api teamId key v
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
            match model.Bundle, model.CurrentQuestion with
            | Some bundle, Some cq ->
                match bundle.GetQw cq.Key with
                | Some qw ->  yield! answersTable dispatch bundle qw cq
                | None -> ()
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

let answersTable dispatch  (bundle:AnswersBundle) (qw:QuestionRecord)  cq = [
    h5[Class "title is-5"] [ str <| "Question: " + qw.Nm ]

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
            |> List.filter (fun (_, (found,aw)) -> found)
            |> List.map (fun (team, (found, aw)) -> team, aw)
            |> List.sortBy (fun (team, _) -> team.Id)

        tbody [] [
            for team, aw in answers -> answersRow dispatch team qw aw cq.LastReview
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
        td [] [span [classList modifiers] [str aw.Txt]]
        td [] [
            div [Class "field has-addons"][
                a [Class "button is-small is-success is-inverted";
                    OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key, (qw.Pt.ToString()))))][
                    span [Class "icon"][ Fa.i [ Fa.Solid.PlusSquare; Fa.Size Fa.Fa2x ] [] ]
                ]
                input [Class "input is-small"; Type "number"; Style [ Width "50px" ];
                    OnChange (fun ev -> dispatch (ResultChanged (team.Id, qw.Key, ev.Value)));
                    Value (aw.Res.ToString())]
                a [Class "button is-small";
                    OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key,"")))][
                    span [Class "icon"][ Fa.i [ Fa.Regular.Circle; Fa.Size Fa.FaExtraSmall ] [] ]
                ]
                if (qw.Jpd) then
                    a [Class "button is-small is-danger is-inverted";
                        OnClick (fun _ -> dispatch (ResultChanged (team.Id, qw.Key, ((-qw.Pt).ToString()))))][
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
