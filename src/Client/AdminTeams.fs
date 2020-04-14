module rec AdminTeams

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
    | Exn of exn
    | DeleteError of string
    | GetTeamsResp of RESP<TeamRecord list>
    | CreateTeam
    | CreateTeamResp of RESP<{|Record : TeamRecord|}>
    | CreateDlgChange of string
    | CreateDlgOk
    | CreateDlgCancel
    | ToggleCard of int
    | GetTeamResp of RESP<TeamCard>
    | UpdateName of string
    | UpdateStatus of string
    | UpdateToken of string
    | CancelCard
    | SubmitCard
    | SubmitResp of RESP<TeamRecord>
    | ChangeStatus of int * TeamStatus

type Model = {
    Teams : TeamRecord list
    Errors : Map<string, string>
    CardIsLoading : int option
    Card : TeamCard option
    CreateDlg : string option
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let loading cardId model =
    {model with CardIsLoading = Some cardId}

let editing model =
    {model with CardIsLoading = None}

let createCmd (api:IAdminApi) (model:Model) =
    match model.CreateDlg with
    | Some txt -> {model with CreateDlg = None} |> apiCmd api.createTeam {|TeamName = txt|} CreateTeamResp Exn
    | None -> model |> noCmd

let toggleCard (api:IAdminApi) teamId model =
    match model.CardIsLoading with
    | Some _ -> model |> noCmd
    | None ->
        match model.Card with
        | Some card when card.TeamId = teamId -> {model with Card = None} |> noCmd
        | _ -> {model with Card = None} |> loading teamId |> apiCmd api.getTeamCard {|TeamId = teamId|} GetTeamResp Exn

let validateName txt =
    if String.IsNullOrWhiteSpace(txt) then "Name is required" else ""

let updateCard f model =
    match model.Card with
    | Some card -> {model with Card = Some <| f card}
    | _ -> model

let validate (card : TeamCard) =
    [validateName card.TeamName]
    |> List.filter (fun s -> s <> "")

let submitCard api model =
    match model.Card with
    | Some card ->
        model |> loading card.TeamId |> apiCmd api.updateTeamCard card SubmitResp Exn
    | None -> model |> noCmd

let replaceRecord record model =
    {model with Teams = record :: (model.Teams |> List.filter (fun q -> q.TeamId <> record.TeamId))}

let init (api:IAdminApi) user st : Model*Cmd<Msg> =
    {Errors = Map.empty; Teams = []; CardIsLoading = None; Card = None; CreateDlg = None} |> apiCmd api.getTeams () GetTeamsResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | DeleteError id -> cm |> delError id |> noCmd
    | GetTeamsResp {Value = Ok res } -> {cm with Teams = res} |> noCmd
    | CreateTeam -> {cm with CreateDlg = Some ""} |> noCmd
    | CreateDlgChange txt -> {cm with CreateDlg = Some txt} |> noCmd
    | CreateDlgCancel -> {cm with CreateDlg = None} |> noCmd
    | CreateDlgOk -> cm |> createCmd api
    | CreateTeamResp {Value = Ok res} -> {cm with Teams = res.Record :: cm.Teams} |> noCmd
    | ToggleCard teamId -> cm |> toggleCard api teamId
    | GetTeamResp {Value = Ok res } -> {cm with Card = Some res} |> editing |> noCmd
    | UpdateName txt -> cm |> updateCard (fun c -> {c with TeamName = txt}) |> noCmd
    | UpdateStatus txt -> cm |> updateCard (fun c -> {c with TeamStatus = defaultArg (fromString txt) New}) |> noCmd
    | UpdateToken txt -> cm |> updateCard (fun c -> {c with EntryToken = txt}) |> noCmd
    | CancelCard -> {cm with Card = None} |> noCmd
    | SubmitCard -> cm |> submitCard api
    | SubmitResp {Value = Ok res } -> {cm with Card = None} |> editing |> replaceRecord res |> noCmd
    | ChangeStatus (teamId, status) -> cm |> loading teamId |> apiCmd api.changeTeamStatus {|TeamId = teamId; TeamStatus = status|} SubmitResp Exn
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    div[][
        nav [Class "level"][
            div [Class "level-left"][

            ]
            div [Class "level-right"][
                p [Class "level-item"][
                    button [Class "button"; OnClick (fun _ -> dispatch CreateTeam)][str "Create New Team"]
                ]
            ]
        ]

        for error in model.Errors do
            div [Class "notification is-danger is-light"][
                button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                str error.Value
            ]

        table [Class "table is-fullwidth is-hoverable"][
            thead[][
                tr[][
                    th [Style[Width "30px"]][str ""]
                    th [Style[Width "30px"]][str "Id"]
                    th [][str "Name"]
                    th [][str "Entry URL"]
                    th [Style[Width "60px"]][str "Status"]
                ]
            ]
            tbody [][
                for team in model.Teams |> List.sortBy (fun q -> q.TeamId) |> List.rev do
                    let hasCard = match model.Card with Some card when card.TeamId = team.TeamId -> true | _ -> false
                    let isLoading = match model.CardIsLoading with Some id when id = team.TeamId -> true | _ -> false

                    tr[ classList ["has-text-weight-bold", team.TeamStatus = New; "has-text-danger", team.TeamStatus = Rejected]][
                        th [] [
                            button [classList ["button", true; "is-small", true; "is-loading", isLoading]; OnClick (fun _ -> dispatch <| ToggleCard team.TeamId)][
                                str <| if hasCard then "-" else "+"
                            ]
                        ]
                        td [] [str <| team.TeamId.ToString()]
                        td [] [str team.TeamName]
                        td [] [str <| urlForTeam user.QuizId team.TeamId team.EntryToken]
                        td [] [
                            str <| team.TeamStatus.ToString()

                            if (team.TeamStatus = New) then
                                button [Class "button is-small"; Title "Admit"; OnClick (fun _ -> ChangeStatus (team.TeamId, Admitted) |> dispatch)][span [Class "icon has-text-info"][Fa.i [Fa.Regular.ThumbsUp][]]]
                                button [Class "button is-small"; Title "Reject"; OnClick (fun _ -> ChangeStatus (team.TeamId, Rejected) |> dispatch)][span [Class "icon has-text-danger"][Fa.i [Fa.Regular.ThumbsDown][]]]
                        ]
                    ]
                    if hasCard then
                        tr [][
                            th [][]
                            td [ColSpan 4] [ card dispatch model.Card.Value isLoading]
                        ]
            ]
        ]

        match model.CreateDlg with
        | Some txt ->
            MainTemplates.inputModal dispatch "Enter Team's Name" txt CreateDlgChange CreateDlgOk CreateDlgCancel
        | None -> ()
    ]

let card (dispatch : Msg -> unit) (card : AdminModels.TeamCard) isLoading =
    div[][
        div [Class "columns"][
            div [Class "column"][
                div [Class "field"][
                    label [Class "label"][str "Name"; span [Class "has-text-danger"][str "*"]]
                    let error = validateName card.TeamName
                    div [Class "control"][
                        input [classList ["input", true; "is-danger", error <> ""]; Type "text"; Placeholder "Team Name"; MaxLength 64.0;
                            valueOrDefault card.TeamName;
                            OnChange (fun ev -> dispatch <| UpdateName ev.Value)]
                    ]
                    p [Class "help is-danger"][str error]
                ]
                div [Class "field"][
                    label [Class "label"][str "Entry token"]
                    div [Class "control"][
                        input [Class "input"; Type "text"; valueOrDefault card.EntryToken; OnChange (fun ev -> dispatch <| UpdateToken ev.Value)]
                    ]
                ]
            ]
            div [Class "column"][
                div [Class "control"][
                    label [Class "label"][str "Status"]
                    div [Class "select"][
                        select[valueOrDefault card.TeamStatus; OnChange (fun ev -> dispatch <| UpdateStatus ev.Value )][
                            for case in Reflection.FSharpType.GetUnionCases typeof<TeamStatus> do
                                option [][str case.Name]
                        ]
                    ]
                ]
                div [Class "content"][
                    ul[][
                        li[][
                            str "Registration date "
                            str <| card.RegistrationDate.ToString("dd.MM.yyyy HH:mm")
                        ]
                    ]
                ]
            ]
        ]
        div [Class "field is-grouped"][
            div [Class "control"][
                let hasErrors = not (validate card |> List.isEmpty)
                button [Class "button is-dark "; Disabled hasErrors; OnClick (fun _ -> dispatch SubmitCard)] [ str "Submit"]
            ]
            div [Class "control"][
                button [Class "button"; OnClick (fun _ -> dispatch CancelCard)] [ str "Cancel"]
            ]
        ]

    ]