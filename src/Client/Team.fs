module rec Team

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Elmish.Navigation

open Shared
open Shared.TeamModels
open Common

type Tab = History | Question | Results

type Msg =
    | QuizCardResp of RESP<TeamModels.QuizCard>
    | Exn of exn
    | Reactivate
    | ChangeTab of Tab

type Model = {
    IsActive : bool
    Quiz : QuizCard option
    ActiveTab : Tab
    Error : string
    TimeDiff: System.TimeSpan
}

let error txt model =
    {model with Error = txt}

let ok model =
    {model with Error = ""}

let startup quiz serverTime model =
    {model with IsActive = true; Quiz = Some quiz; TimeDiff = timeDiff serverTime} |> ok

let init (api:ITeamApi) user : Model*Cmd<Msg> =
    {IsActive = true; Quiz = None; ActiveTab = Question; Error = ""; TimeDiff = TimeSpan.Zero} |> apiCmd api.getState () QuizCardResp Exn

let update (api:ITeamApi) user (msg : Msg) (cm : Model) st : Model * Cmd<Msg> =
    match msg with
    | QuizCardResp {Value = Ok res; ST = st} -> cm |> startup res st |> noCmd
    | Reactivate -> cm |> apiCmd api.takeActiveSession () QuizCardResp Exn
    | Exn ex when ex.Message = Errors.SessionIsNotActive -> {cm with IsActive = false} |> noCmd
    | Exn ex -> cm |> error ex.Message |> noCmd
    | Err txt -> cm |> error txt |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:TeamUser) (model : Model) =
    match model.IsActive with
    | true ->
        match model.Quiz with
        | Some quiz -> activeView dispatch user quiz model
        | None -> str model.Error
    | false -> notActiveView dispatch user model.Error

let notActiveView (dispatch : Msg -> unit) (user:TeamUser) error =
    section [Class "hero is-danger is-medium is-fullheight"][
        div [Class "hero-body"][
            div [Class "container has-text-centered is-fluid"][
                p [Class "title"] [ str "NOT ACTIVE" ]
                p [ Class "title" ] [ str user.QuizName ]
                p [ Class "subtitle" ] [ str user.TeamName ]
                p [ Class "subtitle" ] [ str "Team's session is running from another device. Only one device is allowed per team." ]
                a [Class "button is-large"; OnClick (fun _ -> dispatch (Reactivate)) ] [ str "Use this device" ]
                p [Class "help is-white"][ str error ]
            ]
        ]
    ]

let activeView (dispatch : Msg -> unit) (user:TeamUser) quiz model =
    div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
        div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [

            br []
            figure [ Class "image is-128x128"; Style [Display DisplayOptions.InlineBlock] ] [ img [ Src <| Infra.urlForImgSafe quiz.Img ] ]
            br []
            h3 [Class "title is-3"] [ str user.QuizName ]
            h4 [Class "subtitle is-4" ] [ str user.TeamName ]

            match quiz.TS with
            | New -> div [Class "notification is-white"][str "Waiting for confirmation of the registration..."]
            | Admitted ->
                div[][
                    match model.ActiveTab with
                    | History -> yield str "ANSWERS"
                    | Question ->
                        match quiz.QS with
                        | Live -> yield quiestionView dispatch user quiz model
                        | _ ->
                            yield
                                div [Class "notification is-white"][
                                    p [Class "subtitle is-5"][
                                        match quiz.QS with
                                        | Draft | Published -> str "Coming soon ..."
                                        | Finished | Archived -> str "Finished"
                                        | _ -> ()
                                    ]
                                    p [][for l in quiz.Msg.Split ([|'\n'|]) do yield str l; yield br[]]
                                 ]
                    | Results -> yield str "RESULTS"
                ]
            | Rejected -> div [Class "notification is-white"][span[Class "has-text-danger has-text-weight-bold"][str "Registration has been rejected ("]]

            p [Class "help is-danger"][ str model.Error ]
            div [ Style [Height "66px"]] []
        ]

        if quiz.TS = Admitted then
            div [Style [Position PositionOptions.Fixed; Bottom "0"; Height "62px"; Width "100%"; BackgroundColor "#FFFFFF"; OverflowX OverflowOptions.Hidden]]  [
                div [Class "tabs is-white is-large is-toggle is-fullwidth"] [
                    ul[][
                        li [classList ["has-text-weight-bold", model.ActiveTab = History] ] [
                            a [OnClick (fun _ -> dispatch (ChangeTab History))] [ str "History" ]
                        ]
                        li [classList ["has-text-weight-bold", model.ActiveTab = Question; "has-background-danger", false]] [
                            a [OnClick (fun _ -> dispatch (ChangeTab Question))] [
                                str "Question"
                                //if isCountdownActive then str (secondsLeft.ToString()) else str "Quiestion"
                            ]
                        ]
                        li [classList ["has-text-weight-bold", model.ActiveTab = History] ] [
                            a [OnClick (fun _ -> dispatch (ChangeTab Results))] [ str "Results" ]
                        ]
                    ]
                ]
           ]
    ]

let quiestionView (dispatch : Msg -> unit) (user:TeamUser) quiz model =
    str "QUESTION"