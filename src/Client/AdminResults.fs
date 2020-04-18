module AdminResults

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared
open Shared.AdminModels
open Common

type Msg =
    | GetResultsResp of RESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
    | Exn of exn
    | DeleteError of string
    | SwitchView


type Model = {
    Errors : Map<string, string>
    IsLoading : bool
    Teams : TeamResult list
    Questions : QuestionResult list
    IsDetailed : bool
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let loading  model =
    {model with IsLoading = true}

let editing model =
    {model with IsLoading = false}

let init (api:IAdminApi) user : Model*Cmd<Msg> =
    {Errors = Map.empty; IsLoading = true; Teams = []; Questions = []; IsDetailed = false} |> apiCmd api.getResults () GetResultsResp Exn

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetResultsResp {Value = Ok res } -> {cm with Teams = res.Teams; Questions = res.Questions} |> editing |> noCmd
    | SwitchView -> {cm with IsDetailed = not cm.IsDetailed} |> noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    div [Class "table-container"][
        if model.IsLoading then
            button [Class "button is-loading is-large is-fullwidth is-light"][]

        for error in model.Errors do
            div [Class "notification is-danger is-light"][
                button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                str error.Value
            ]

        button [Class "button"; OnClick (fun _ -> dispatch SwitchView)][
            str <| if model.IsDetailed then "to compact view" else "to detailed view"
        ]
        br[]
        br[]
        div [Class "table-container"; Style[Width "100%"]][
            table [Class "table is-hoverable"] [
                thead [ ] [
                    tr [ ] [
                        th [ ] [ str "Id" ]
                        th [ ] [ str "Team" ]
                        if (model.IsDetailed) then
                            yield! [for qw in model.Questions |> List.sortBy (fun qw -> qw.Idx) ->  th [] [str qw.Name]]
                        th [ ] [ str "Points" ]
                        th [ ] [ str "Place" ]
                    ]
                ]

                tbody [] [
                    for team in model.Teams |> List.sortByDescending (fun r -> r.Points, -r.TeamId)  do
                        tr [] [
                            td[] [str <| team.TeamId.ToString()]
                            td[Style [WhiteSpace WhiteSpaceOptions.Nowrap]] [str team.TeamName]
                            if model.IsDetailed then
                                yield! [
                                    for index in 1 .. model.Questions.Length ->
                                        td[] [
                                            match team.History.TryGetValue index with
                                            | true, res -> res.ToString()
                                            | _ -> ""
                                            |> str
                                        ]
                                ]
                            td [] [str <| team.Points.ToString()]
                            td [] [
                                if team.PlaceFrom = team.PlaceTo
                                then team.PlaceFrom.ToString()
                                else sprintf "%i-%i" team.PlaceFrom team.PlaceTo
                                |> str
                            ]
                        ]
                ]
            ]
        ]
    ]


