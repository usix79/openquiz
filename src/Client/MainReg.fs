module rec MainReg

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
    | GetRegModelResp of RESP<MainModels.QuizRegRecord>
    | OpenRegForm
    | SubmitRegFrom
    | CancelRegFrom
    | RegFromTeamName of string
    | RegisterTeamResp of RESP<MainModels.QuizRegRecord>

type Model = {
    Error : string
    Quiz : MainModels.QuizRegRecord option
    RegForm : RegForm option
}

type RegForm = {
    QuizId : int
    TeamName : string
    IsSending : bool
    Error : string
}

let openRegForm cm =
    match cm.Quiz with
    | Some quiz ->
        match cm.RegForm with
        | Some regForm when regForm.IsSending -> cm
        | _ ->
            let teamName =
                match quiz.Comp with
                | Some comp -> comp.TeamName
                | _ -> ""
            {cm with RegForm = Some {QuizId = quiz.QuizId; TeamName = teamName; IsSending = false; Error = ""}}
    | None -> cm

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
        |> apiCmd api.registerTeam {|TeamName = regForm.TeamName|} RegisterTeamResp Exn
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

let init (api:IMainApi) user : Model*Cmd<Msg> =
    {Error = ""; Quiz = None; RegForm = None} |> apiCmd api.getRegModel () GetRegModelResp Exn

let update (api:IMainApi)(user:MainUser) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetRegModelResp {Value = Ok res} -> {cm with Quiz = Some res}|>  noCmd
    | OpenRegForm -> cm |> openRegForm |> noCmd
    | CancelRegFrom  -> cm |> cancelRegForm |> noCmd
    | SubmitRegFrom  -> cm |> submitRegForm api
    | RegFromTeamName txt -> cm |> formTeamName txt |> noCmd
    | RegisterTeamResp {Value = Ok quiz} -> {cm with RegForm = None; Quiz = Some quiz} |> noCmd
    | RegisterTeamResp {Value = Error txt} -> cm |> formError txt |> noCmd
    | Err txt -> {cm with Error = txt} |> noCmd
    | Exn ex -> {cm with Error = ex.Message} |> noCmd
    | _ -> cm |> noCmd

let levelWithRegisterBtn dispatch = [
    div [Class "level is-mobile"][
        div [Class "level-left"][]
        div [Class "level-right"][
            p [Class "level-item is-light"][
                a [Class "button is-small"; OnClick (fun _ -> OpenRegForm |> dispatch) ][str "Register"]
            ]
        ]
    ]
]

let levelWithRegistrationInfo dispatch (quiz : QuizRegRecord) (comp:ExpertCompetition) = [
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
                if (quiz.Status = Setup) then
                    p [Class "level-item is-light"][
                        a [Class "button is-small"; Title "Edit"; OnClick (fun _ -> OpenRegForm |> dispatch) ][Fa.i [Fa.Regular.Edit][]]
                    ]
            ]
        ]
]

let levelWithEditForm dispatch (quiz : QuizRegRecord) (regForm : RegForm ) = [
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

let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
    section [Class "hero is-shadowless is-fullheight"] [
        div [Class "hero-head"] [
            div [Class "container has-text-centered"][
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

                match model.Quiz with
                | Some quiz -> yield! quizView dispatch quiz model.RegForm
                | None when model.Error <> "" -> span [Class "has-text-danger"][ str model.Error]
                | None  -> str "Loading ..."
            ]
        ]

        MainTemplates.footerHero
    ]

let quizView (dispatch : Msg -> unit) (quiz : QuizRegRecord) (regForm : RegForm option) = [
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
        match regForm with
        | Some form ->  yield! levelWithEditForm dispatch quiz form
        | None ->
            match quiz.Comp with
            | Some comp -> yield! levelWithRegistrationInfo dispatch quiz comp
            | None when quiz.Status <> Finished -> yield! levelWithRegisterBtn dispatch
            | None -> ()
    ]
]