module rec MainPub

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.FontAwesome

open Shared
open Common
open MainModels

type Msg =
    | Exn of exn
    | DeleteError of string
    | GetPubModelResp of RESP<{|Profile : MainModels.ExpertProfile; Quizzes : MainModels.QuizPubRecord list|}>
    | OpenEditForm of int // quizId
    | SubmitRegFrom
    | CancelRegFrom
    | RegFromTeamName of string
    | RegisterTeamResp of RESP<MainModels.ExpertCompetition>

type Model = {
    Errors : Map<string, string>
    Profile : MainModels.ExpertProfile option
    Quizzes : MainModels.QuizPubRecord list
    RegForm : RegForm option
}

type RegForm = {
    QuizId : int
    TeamName : string
    IsSending : bool
    Error : string
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let openRegForm quizId cm =
    match cm.RegForm with
    | Some regForm when regForm.IsSending -> cm
    | _ ->
        let teamName =
            match cm.Profile with
            | Some p when p.Competitions.ContainsKey quizId -> p.Competitions.[quizId].TeamName
            | _ -> ""
        {cm with RegForm = Some {QuizId = quizId; TeamName = teamName; IsSending = false; Error = ""}}

let cancelRegForm cm =
    match cm.RegForm with
    | Some regForm when regForm.IsSending -> cm
    | _ -> {cm with RegForm = None}

let submitRegForm api cm =
    match cm.RegForm with
    | Some regForm when regForm.IsSending -> cm |> noCmd
    | Some regForm when not <| validateForm regForm -> cm |> noCmd
    | Some regForm ->
        {cm with RegForm = Some {regForm with IsSending = true}}
        |> apiCmd api.registerTeam {|QuizId = regForm.QuizId; TeamName = regForm.TeamName|} RegisterTeamResp Exn
    | _ -> cm |> noCmd

let formError txt cm =
    match cm.RegForm with
    | Some regForm -> {cm with RegForm = Some {regForm with IsSending = false; Error = txt}}
    | _ -> cm

let formTeamName txt cm =
    match cm.RegForm with
    | Some regForm -> {cm with RegForm = Some {regForm with TeamName = txt}}
    | _ -> cm

let validateForm regForm =
    not <| String.IsNullOrWhiteSpace regForm.TeamName

let registerResp comp cm =
    match cm.Profile with
    | Some p -> {cm with RegForm = None; Profile = Some <| p.UpdateCompetition comp}
    | None -> cm

let init (api:IMainApi) user : Model*Cmd<Msg> =
    {Errors = Map.empty; Quizzes = []; RegForm = None; Profile = None} |> apiCmd api.getPubModel () GetPubModelResp Exn

let update (api:IMainApi)(user:MainUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | Exn ex -> cm |> addError ex.Message |> noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | GetPubModelResp {Value = Ok res} -> {cm with Profile = Some res.Profile; Quizzes = res.Quizzes}|>  noCmd
    | GetPubModelResp {Value = Error txt} -> cm |> addError txt |> noCmd
    | OpenEditForm quizId -> cm |> openRegForm quizId |> noCmd
    | CancelRegFrom  -> cm |> cancelRegForm |> noCmd
    | SubmitRegFrom  -> cm |> submitRegForm api
    | RegFromTeamName txt -> cm |> formTeamName txt |> noCmd
    | RegisterTeamResp {Value = Ok comp} -> cm |> registerResp comp |> noCmd
    | RegisterTeamResp {Value = Error txt} -> cm |> formError txt |> noCmd


let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
    match user.IsPrivate with
    | true -> viewAsPrivate dispatch user model (model.Quizzes |> List.tryHead)
    | false -> viewAsUsual dispatch user model

let viewAsUsual (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
    div [] [
        div [Class "title"] [str "Welcome to the Open Quiz!"]
        div [Class "subtitle"] [
            p[][
                str "Here you can compete in open quizzes as well as produce new quizzes. "
                str "The service is "
                strong [Class "has-text-danger"] [str "completely free"]
                str " for commercial and noncommercial use."
            ]
        ]

        for error in model.Errors do
            div [Class "notification is-danger"][
                button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                str error.Value
            ]

        br[]

        let featureQuizzes = model.Quizzes |> List.filter (fun q -> q.Status <> Finished) |> List.sortBy (fun q -> q.StartTime)
        if not (List.isEmpty featureQuizzes) then
            h3 [Class "title"] [str "Featured quizzes"]

            for quizzes in featureQuizzes |> List.chunkBySize 3 do
                div [Class "columns"][
                    for quiz in quizzes do
                        div [Class "column is-one-third"][
                            yield quizBox dispatch model.Profile quiz model.RegForm
                        ]
                ]
        br[]

        let finishedQuizzes = model.Quizzes |> List.filter (fun q -> q.Status = Finished) |> List.sortBy (fun q -> q.StartTime)
        if not (List.isEmpty finishedQuizzes) then
            h3 [Class "title"] [str "Finished quizzes"]

            for quizzes in finishedQuizzes |> List.chunkBySize 3 do
                div [Class "columns"][
                    for quiz in quizzes do
                        div [Class "column is-one-third"][
                            yield quizBox dispatch model.Profile quiz model.RegForm
                        ]
                ]
    ]

let quizBox (dispatch : Msg -> unit) (profile:ExpertProfile option) (quiz : QuizPubRecord)  (regForm : RegForm option) =
    div [Class "box is-shadowless"][
        article [Class "media"][
            div [Class "media-left"][
                figure [Class "image is-64x64"][
                    let url = if quiz.ImgKey <> "" then Infra.urlForImg quiz.ImgKey else Infra.defaultImg
                    img [Src url; Alt "Image"]
                ]
            ]
            div [Class "media-content"][
                div [Class "content"][
                    p[][
                        strong [][str quiz.Brand]
                        str "   "
                        match quiz.StartTime with Some dt -> str (dt.ToString("yyyy-MM-dd HH:mm")) | None -> str "???"
                        if quiz.Status = Live then
                            str " "
                            span [Class "tag is-danger is-light"][str "live"]
                        br[]
                        strong [][str quiz.Name]
                        br[]
                        small [] (splitByLines quiz.Description)
                        if quiz.EventPage <> "" then
                            a[Href quiz.EventPage][str "details"]

                    ]
                ]
                match quiz.Status <> Finished, profile, regForm with
                | true, Some p, Some form when form.QuizId = quiz.QuizId -> yield! levelWithEditForm dispatch p quiz form
                | _, Some p, _ when p.Competitions.ContainsKey quiz.QuizId -> yield! levelWithRegistrationInfo dispatch p quiz
                | true, Some p, _ -> yield! levelWithRegisterBtn dispatch quiz
                | _ -> ()
            ]
        ]
    ]

let levelWithRegisterBtn dispatch (quiz : QuizPubRecord) = [
    div [Class "level is-mobile"][
        div [Class "level-left"][]
        div [Class "level-right"][
            p [Class "level-item is-light"][
                a [Class "button is-small"; OnClick (fun _ -> OpenEditForm quiz.QuizId |> dispatch) ][str "Register"]
            ]
        ]
    ]
]

let levelWithRegistrationInfo dispatch (profile:ExpertProfile) (quiz : QuizPubRecord) = [
    match profile.Competitions.TryGetValue quiz.QuizId with
    | true, comp ->
        small [Class "has-text-weight-light"][
            str "Registered as"
            a [Href (urlForTeam quiz.QuizId comp.TeamId comp.EntryToken); Target "_blank";
                Style [MarginLeft "5px"; MarginRight "5px"]][
                str comp.TeamName; str " "; Fa.i [Fa.Solid.ExternalLinkAlt][]
           ]
        ]
        if (quiz.Status <> Finished) then
            div [Class "level is-mobile"][
                div [Class "level-left"][
                    match comp.TeamStatus with
                    | New -> span [Class "tag is-warning is-light"][str "waiting confirmation"]
                    | Admitted -> span [Class "tag is-success is-light"][str "confirmed"]
                    | Rejected -> span [Class "tag is-danger is-light"][str "rejected"]
                ]
                div [Class "level-right"][
                    if (quiz.Status = Published) then
                        p [Class "level-item is-light"][
                            a [Class "button is-small"; Title "Edit"; OnClick (fun _ -> OpenEditForm quiz.QuizId |> dispatch) ][Fa.i [Fa.Regular.Edit][]]
                        ]
                ]
            ]
    | _ -> ()

]

let levelWithEditForm dispatch (profile:ExpertProfile) (quiz : QuizPubRecord) (regForm : RegForm ) = [
    div [Class "field has-addons is-light"; Style [Width "100%"]][
        p [classList ["control", true; "is-expanded", true; "is-loading", regForm.IsSending]][
            input [classList ["input", true; "is-small", true; "is-danger", regForm.Error <> ""]; Type "text"; Placeholder "Team Name"; MaxLength 64.0;
                    valueOrDefault regForm.TeamName; Disabled regForm.IsSending; OnChange (fun ev -> RegFromTeamName ev.Value |> dispatch)]
        ]
        let isDisabled = regForm.IsSending || not <| validateForm regForm
        p [Class "control"][
            a [Class "button is-small"; Disabled isDisabled; OnClick (fun _ -> SubmitRegFrom |> dispatch) ][Fa.i [Fa.Solid.Check][]]
        ]
        p [Class "control"][
            a [Class "button is-small"; Disabled regForm.IsSending; OnClick (fun _ -> CancelRegFrom |> dispatch) ][Fa.i [Fa.Solid.Times][]]
        ]
    ]
    p [Class "help is-danger"][str regForm.Error]
]


let viewAsPrivate (dispatch : Msg -> unit) (user:MainUser) (model : Model) (quiz:QuizPubRecord option) =
    section [Class "hero is-shadowless is-fullheight"] [
        div [Class "hero-head"] [
            div [Class "container"][
                nav [Class "navbar is-transparent is-spaced"; Role "navigation"; AriaLabel "dropdown navigation"] [
                    div [Class "navbar-brand"] [
                        div [Class "navbar-item is-paddingleft is-hidden-desktop"][str user.Name]
                    ]
                    div [Class "navbar-menu"][
                        div [Class "navbar-end"][
                            div [Class "navbar-item"] [
                                figure [Class "image"][
                                    img [Class "is-rounded"; Style [Height "50px"; Width "50px"; MaxHeight "50px"]; Src user.PictureUrl]
                                ]
                                span [Style [MarginLeft "5px"]][str user.Name]
                            ]
                        ]
                    ]
                ]

                div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
                    div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [
                        match quiz with
                        | Some quiz -> yield! privateQuizView dispatch model.Profile quiz model.RegForm
                        | None -> ()
                    ]
                ]
            ]
        ]
        div [Class "hero-body"] [
            div [Class "container"][
                for error in model.Errors do
                    div [Class "notification is-danger"][
                        button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                        str error.Value
                    ]
            ]
        ]
        MainTemplates.footer
    ]


let privateQuizView (dispatch : Msg -> unit) (profile:ExpertProfile option) (quiz : QuizPubRecord) (regForm : RegForm option) = [
    br []
    figure [ Class "image is-128x128"; Style [Display DisplayOptions.InlineBlock] ] [ img [ Src <| Infra.urlForImgSafe quiz.ImgKey ] ]
    br []
    h3 [Class "title is-3"] [str quiz.Name]

    div [Class "notification is-white"][
        p [Class "subtitle is-5"][
            match quiz.StartTime with
            | Some dt -> str (dt.ToString("yyyy-MM-dd HH:mm"))
            | None -> str "???"

            if quiz.Status = Live then
                str " "
                span [Class "tag is-danger is-light"][str "live"]
            br[]
        ]

        p [] (splitByLines quiz.Description)
    ]

    div [Style [Width "320px"; Display DisplayOptions.InlineBlock]] [
        match quiz.Status <> Finished, profile, regForm with
        | true, Some p, Some form when form.QuizId = quiz.QuizId -> yield! levelWithEditForm dispatch p quiz form
        | _, Some p, _ when p.Competitions.ContainsKey quiz.QuizId -> yield! levelWithRegistrationInfo dispatch p quiz
        | true, Some p, _ -> yield! levelWithRegisterBtn dispatch quiz
        | _ -> ()
    ]

]