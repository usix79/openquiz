module rec MainReg

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.FontAwesome

open OpenQuiz.Shared
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

type Model =
    { Error: string
      Quiz: MainModels.QuizRegRecord option
      RegForm: RegForm option }

type RegForm =
    { QuizId: int
      TeamName: string
      IsSending: bool
      Error: string }

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

            { cm with
                RegForm =
                    Some
                        { QuizId = quiz.QuizId
                          TeamName = teamName
                          IsSending = false
                          Error = "" } }
    | None -> cm

let cancelRegForm cm =
    match cm.RegForm with
    | Some regForm when regForm.IsSending -> cm
    | _ -> { cm with RegForm = None }

let submitRegForm api cm =
    match cm.RegForm with
    | Some regForm when regForm.IsSending -> cm |> noCmd
    | Some regForm when not <| validateForm regForm -> cm |> noCmd
    | Some regForm ->
        { cm with
            RegForm = Some { regForm with IsSending = true } }
        |> apiCmd api.registerTeam {| TeamName = regForm.TeamName |} RegisterTeamResp Exn
    | _ -> cm |> noCmd

let formError txt cm =
    match cm.RegForm with
    | Some regForm ->
        { cm with
            RegForm =
                Some
                    { regForm with
                        IsSending = false
                        Error = txt } }
    | _ -> cm

let formTeamName txt cm =
    match cm.RegForm with
    | Some regForm ->
        { cm with
            RegForm = Some { regForm with TeamName = txt } }
    | _ -> cm

let validateForm regForm =
    not <| String.IsNullOrWhiteSpace regForm.TeamName

let init (api: IMainApi) user : Model * Cmd<Msg> =
    { Error = ""
      Quiz = None
      RegForm = None }
    |> apiCmd api.getRegModel () GetRegModelResp Exn

let update (api: IMainApi) (user: MainUser) (msg: Msg) (cm: Model) : Model * Cmd<Msg> =
    match msg with
    | GetRegModelResp { Value = Ok res } -> { cm with Quiz = Some res } |> noCmd
    | OpenRegForm -> cm |> openRegForm |> noCmd
    | CancelRegFrom -> cm |> cancelRegForm |> noCmd
    | SubmitRegFrom -> cm |> submitRegForm api
    | RegFromTeamName txt -> cm |> formTeamName txt |> noCmd
    | RegisterTeamResp { Value = Ok quiz } ->
        { cm with
            RegForm = None
            Quiz = Some quiz }
        |> noCmd
    | RegisterTeamResp { Value = Error txt } -> cm |> formError txt |> noCmd
    | Err txt -> { cm with Error = txt } |> noCmd
    | Exn ex -> { cm with Error = ex.Message } |> noCmd
    | _ -> cm |> noCmd

let levelWithRegisterBtn dispatch (l10n: L10n.RegL10n) =
    [ a
          [ Class "button is-primary is-fullwidth"
            OnClick(fun _ -> OpenRegForm |> dispatch) ]
          [ str l10n.Register ] ]

let levelWithRegistrationInfo dispatch (quiz: QuizRegRecord) (comp: ExpertCompetition) (l10n: L10n.RegL10n) =
    [ table
          [ Class "table" ]
          [ thead [] [ tr [] [ th [] [ str l10n.RegisteredAs ]; th [] [ str l10n.Status ]; th [] [] ] ]
            tbody
                []
                [ tr
                      []
                      [ td [] [ str comp.TeamName ]
                        td
                            []
                            [ match comp.TeamStatus with
                              | New -> span [ Class "tag is-warning is-light" ] [ str l10n.Pending ]
                              | Admitted -> span [ Class "tag is-success is-light" ] [ str l10n.Confirmed ]
                              | Rejected -> span [ Class "tag is-danger is-light" ] [ str l10n.Rejected ] ]
                        td
                            []
                            [ if (quiz.Status = Setup) then
                                  a
                                      [ Class "button is-small"
                                        Title l10n.Edit
                                        OnClick(fun _ -> OpenRegForm |> dispatch) ]
                                      [ Fa.i [ Fa.Regular.Edit ] [] ] ] ] ] ]
      if comp.TeamStatus = Admitted then
          a
              [ Class "button is-success is-light is-fullwidth"
                Href(urlForTeam quiz.QuizId comp.TeamId comp.EntryToken) ]
              [ str (l10n.Enter + " " + quiz.Name) ] ]

let levelWithEditForm dispatch settings (quiz: QuizRegRecord) (regForm: RegForm) (l10n: L10n.RegL10n) =
    [ div
          [ Class "field has-addons is-light"; Style [ Width "100%" ] ]
          [ p
                [ classList [ "control", true; "is-expanded", true; "is-loading", regForm.IsSending ] ]
                [ input
                      [ classList [ "input", true; "is-small", true; "is-danger", regForm.Error <> "" ]
                        Type "text"
                        Placeholder l10n.TeamName
                        MaxLength 64.0
                        valueOrDefault regForm.TeamName
                        Disabled regForm.IsSending
                        OnChange(fun ev -> RegFromTeamName ev.Value |> dispatch) ] ]
            let isDisabled = regForm.IsSending || not <| validateForm regForm

            p
                [ Class "control" ]
                [ a
                      [ Class "button is-small"
                        Disabled isDisabled
                        OnClick(fun _ -> SubmitRegFrom |> dispatch) ]
                      [ Fa.i [ Fa.Solid.Check ] [] ] ]

            p
                [ Class "control" ]
                [ a
                      [ Class "button is-small"
                        Disabled regForm.IsSending
                        OnClick(fun _ -> CancelRegFrom |> dispatch) ]
                      [ Fa.i [ Fa.Solid.Times ] [] ] ] ]
      p [ Class "help is-danger" ] [ str regForm.Error ] ]

let view (dispatch: Msg -> unit) (user: MainUser) (settings: Settings) (model: Model) (l10n: L10n.RegL10n) =
    section
        [ Class "hero is-shadowless is-fullheight" ]
        [ div
              [ Class "hero-head" ]
              [ div
                    [ Class "container has-text-centered" ]
                    [ nav
                          [ Class "navbar is-transparent is-spaced"
                            Role "navigation"
                            AriaLabel "dropdown navigation" ]
                          [ div
                                [ Class "navbar-brand" ]
                                [ div [ Class "navbar-item is-paddingleft is-hidden-desktop" ] [ str user.Name ] ]
                            div
                                [ Class "navbar-menu" ]
                                [ div
                                      [ Class "navbar-end" ]
                                      [ div
                                            [ Class "navbar-item" ]
                                            [ match user.PictureUrl with
                                              | Some url ->
                                                  figure
                                                      [ Class "image" ]
                                                      [ img
                                                            [ Class "is-rounded"
                                                              Style [ Height "50px"; Width "50px"; MaxHeight "50px" ]
                                                              Src url ] ]
                                              | None -> ()
                                              span [ Style [ MarginLeft "5px" ] ] [ str user.Name ] ] ] ] ]

                      match model.Quiz with
                      | Some quiz -> yield! quizView dispatch settings quiz model.RegForm l10n
                      | None when model.Error <> "" -> span [ Class "has-text-danger" ] [ str model.Error ]
                      | None -> str "Loading ..." ] ]

          MainTemplates.footerHero ]

let quizView
    (dispatch: Msg -> unit)
    (settings: Settings)
    (quiz: QuizRegRecord)
    (regForm: RegForm option)
    (l10n: L10n.RegL10n)
    =
    [ br []
      figure
          [ Class "image is-128x128"; Style [ Display DisplayOptions.InlineBlock ] ]
          [ img [ Src <| Infra.urlForMediaImgSafe settings.MediaHost quiz.ImgKey ] ]
      br []
      h3 [ Class "title is-3" ] [ str quiz.Name ]
      h4 [ Class "subtitle is-4" ] [ Fa.i [ Fa.Solid.DoorOpen ] [ str <| " " + l10n.Registration ] ]

      div
          [ Class "notification is-white" ]
          [ p
                [ Class "subtitle is-5" ]
                [ match quiz.StartTime with
                  | Some dt -> str (dt.ToString("yyyy-MM-dd HH:mm"))
                  | None -> str "???"

                  if quiz.Status = Live then
                      str " "
                      span [ Class "tag is-danger is-light" ] [ str "live" ]
                  br [] ]

            p [] (splitByLines quiz.Description)

            if quiz.EventPage <> "" then
                a [ Href quiz.EventPage ] [ str l10n.Details ]

            if quiz.Information <> "" then
                br []
                div [ Class "notification is-info" ] [ p [] (splitByLines quiz.Information) ] ]

      div
          [ Style [ Width "320px"; Display DisplayOptions.InlineBlock ] ]
          [ match regForm with
            | Some form -> yield! levelWithEditForm dispatch settings quiz form l10n
            | None ->
                match quiz.Comp with
                | Some comp -> yield! levelWithRegistrationInfo dispatch quiz comp l10n
                | None when quiz.Status <> Finished -> yield! levelWithRegisterBtn dispatch l10n
                | None -> () ] ]