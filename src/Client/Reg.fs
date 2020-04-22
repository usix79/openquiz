module rec Reg

open System
open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Elmish.React

open Shared
open Shared.RegModels
open Common

type Msg =
    | GetRecordRsp of RESP<QuizRecord>
    | Exn of exn
    | DeleteError of string

type Model = {
    Quiz : QuizRecord option
    Errors : Map<string, string>
}

let addError txt model =
    {model with Errors = model.Errors.Add(Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let init (api:IRegApi): Model * Cmd<Msg> =
    {Quiz = None; Errors = Map.empty} |> apiCmd api.getRecord () GetRecordRsp Exn

let update (api:IRegApi) (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | GetRecordRsp {Value = Ok res} -> {cm with Quiz = Some res} |> noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | Err txt -> cm |> addError txt |> noCmd
    | Exn ex -> cm |> addError ex.Message |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (model : Model) =
    section [Class "hero is-shadowless is-fullheight"] [
        div [Class "hero-head"] [
            div [Class "container"][
                nav [Class "navbar is-transparent is-spaced"; Role "navigation"; AriaLabel "dropdown navigation"] [
                    div [Class "navbar-brand"] [
                    ]
                ]

                div [Style [Width "100%"; Height "100%"; MinWidth "375px"; TextAlign TextAlignOptions.Center; Position PositionOptions.Relative]] [
                    div [Style [OverflowY OverflowOptions.Auto; Position PositionOptions.Absolute; Top "0"; Width "100%"]] [
                        div[][
                            match model.Quiz with
                            | Some quiz -> yield! quizView dispatch quiz
                            | None -> ()
                        ]

                        a [Href "/login"; Class "button thickbox"; Style [Margin "5px"]; Target "_self"] [
                            str "Please login to succeed registration"
                        ]
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

let quizView (dispatch : Msg -> unit) (quiz:QuizRecord) = [
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
]