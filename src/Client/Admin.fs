module Admin

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Elmish.Navigation

open Shared
open Common

type Msg =
    | SwitchToTeams
    | Teams of AdminTeams.Msg
    | SwitchToCP
    | CP of AdminCP.Msg
    | SwitchToAnswers
    | Answers of AdminAnswers.Msg
    | SwitchToResults
    | Results of AdminResults.Msg

type Model =
    | Empty
    | Teams of AdminTeams.Model
    | CP of AdminCP.Model
    | Answers of AdminAnswers.Model
    | Results of AdminResults.Model

let replaceHashCmd (msg:Msg) =
    match msg with
    | SwitchToTeams -> Infra.urlWithNewHash "#teams" |> Navigation.modifyUrl
    | SwitchToCP -> Infra.urlWithNewHash "#cp" |> Navigation.modifyUrl
    | SwitchToAnswers -> Infra.urlWithNewHash "#answers" |> Navigation.modifyUrl
    | SwitchToResults -> Infra.urlWithNewHash "#results" |> Navigation.modifyUrl
    | _ -> Cmd.none

let switchByHashMsg () =
    let (hashTag, _) = Infra.getUrlHashParts()

    match hashTag with
    | "#teams" -> SwitchToTeams
    | "#cp" -> SwitchToCP
    | "#answers" -> SwitchToAnswers
    | "#results" -> SwitchToResults
    | _ -> SwitchToTeams

let init api user : Model*Cmd<Msg> =
    Empty, Cmd.OfFunc.result <| switchByHashMsg()

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg, cm with
    | Msg.SwitchToTeams, _ ->
        let subModel, subCmd = AdminTeams.init api user
        Teams subModel, Cmd.batch[Cmd.map Msg.Teams subCmd; replaceHashCmd msg]
    | Msg.Teams subMsg, Teams subModel ->
        let subModel,subCmd = AdminTeams.update api user subMsg subModel
        Teams subModel, Cmd.map Msg.Teams subCmd
    | Msg.SwitchToCP, _ ->
        let subModel, subCmd = AdminCP.init api user
        CP subModel, Cmd.batch[Cmd.map Msg.CP subCmd; replaceHashCmd msg]
    | Msg.CP subMsg, CP subModel ->
        let subModel,subCmd = AdminCP.update api user subMsg subModel
        CP subModel, Cmd.map Msg.CP subCmd
    | Msg.SwitchToAnswers, _ ->
        let subModel, subCmd = AdminAnswers.init api user
        Answers subModel, Cmd.batch[Cmd.map Msg.Answers subCmd; replaceHashCmd msg]
    | Msg.Answers subMsg, Answers subModel ->
        let subModel,subCmd = AdminAnswers.update api user subMsg subModel
        Answers subModel, Cmd.map Msg.Answers subCmd
    | Msg.SwitchToResults, _ ->
        let subModel, subCmd = AdminResults.init api user
        Results subModel, Cmd.batch[Cmd.map Msg.Results subCmd; replaceHashCmd msg]
    | Msg.Results subMsg, Results subModel ->
        let subModel,subCmd = AdminResults.update api user subMsg subModel
        Results subModel, Cmd.map Msg.Results subCmd
    | _ -> cm |> noCmd


let view (dispatch : Msg -> unit) (user:AdminUser) (settings:Settings) (model : Model) =
    section [Class "hero is-shadowless is-fullheight is-light"] [
        div [Class "hero-head"] [
            div [Class "container"][
                nav [Class "navbar is-transparent is-spaced"; Role "navigation"; AriaLabel "dropdown navigation"] [
                    div [Class "navbar-brand"] [
                        a [Class "navbar-item"; Style [MarginRight "auto"]] [
                            let imgSrc = Infra.urlForMediaOrDefault settings.MediaHost user.QuizImg Infra.defaultMediaImg
                            figure [Class "image is-64x64"][
                                img [Src imgSrc; Alt "logo"; Style [Height "64px"; Width "64px"; MaxHeight "64px"]]
                            ]
                        ]
                    ]
                    h4 [Class "title is-4 is-paddingleft"; Style [MaxWidth "320px"]][str user.QuizName]

                    a [classList ["navbar-item", true; "has-text-weight-bold", match model with Teams _ -> true | _ -> false]; OnClick (fun _ -> dispatch SwitchToTeams)][str "Teams"]
                    a [classList ["navbar-item", true; "has-text-weight-bold", match model with CP _ -> true | _ -> false]; OnClick (fun _ -> dispatch SwitchToCP)][str "Control Panel"]
                    a [classList ["navbar-item", true; "has-text-weight-bold", match model with Answers _ -> true | _ -> false]; OnClick (fun _ -> dispatch SwitchToAnswers)][str "Answers"]
                    a [classList ["navbar-item", true; "has-text-weight-bold", match model with Results _ -> true | _ -> false]; OnClick (fun _ -> dispatch SwitchToResults)][str "Results"]
                ]
            ]
        ]
        div [Class "hero-body"] [
            div [Class "container"; Style [MarginBottom "auto"]] [
                match model with
                | Empty -> str "empty"
                | Teams subModel -> AdminTeams.view (Msg.Teams >> dispatch) user subModel
                | CP subModel -> AdminCP.view (Msg.CP >> dispatch) user settings subModel
                | Answers subModel -> AdminAnswers.view (Msg.Answers >> dispatch) user subModel
                | Results subModel -> AdminResults.view (Msg.Results >> dispatch) user subModel settings.MediaHost
            ]
        ]
        div [Class "hero-foot has-background-dark has-text-grey-light"] [
            div [Class "container"; Style [TextAlign TextAlignOptions.Center]][
                str "\u00a9"
                span[Class "is-hidden-touch"][str " Serhii Sabirov"]
                str " 2020/21"
                str " - "
                a [Href "/terms.html"; Class "has-text-grey-light"] [str "Terms"]
                str " - "
                a [Href "/disclaimer.html"; Class "has-text-grey-light"] [str "Disclaimer"]
                str " - "
                a [Href "/privacy-policy-en.html"; Class "has-text-grey-light"] [str "Privacy"]
                str " - "
                a [Href "https://t.me/open_quiz"; Class "has-text-grey-light" ] [str "Contact"]
            ]
        ]
    ]