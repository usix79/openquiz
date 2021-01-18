module rec MainProdQuizzes

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.Core.JsInterop
open Fable.FontAwesome

open Shared
open Common
open MainModels

importAll "flatpickr/dist/themes/light.css"

type Msg =
    | CreateQuiz
    | CreateQuizResp of RESP<{|Record : QuizProdRecord; Card:QuizProdCard|}>
    | GetQuizzesResp of RESP<QuizProdRecord list>
    | Exn of exn
    | DeleteError of string
    | ToggleCard of int // quizId
    | GetCardResp of RESP<QuizProdCard>
    | UpdateStartTime of System.DateTime
    | UpdateName of string
    | UpdateMixlrCode of string
    | UpdateEventPage of string
    | UpdateWelcomeTxt of string
    | UpdateInfoTxt of string
    | UpdateFarewellTxt of string
    | UpdateIsPremoderated of bool
    | CancelCard
    | SubmitCard
    | SubmitCardResp of RESP<QuizProdRecord>
    | QuizImgChanged of {|File:Browser.Types.File; Tag:int|}
    | QuizImgClear of unit
    | QuizImgUploaded of quizId:int*bucketKey:string
    | ToggleDeleteForm
    | DeleteFormUpdateText of string
    | DeleteQuiz of int
    | DeleteQuizResp of TRESP<int,unit>

type Model = {
    Quizzes : QuizProdRecord list
    Card : QuizProdCard option
    Errors : Map<string, string>
    CardIsLoading : int option // quizId
    Form : DeleteForm option

}

let loading quizId model =
    {model with CardIsLoading = Some quizId}

let editing model =
    {model with CardIsLoading = None}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let toggleCard api quizId model =
    match model.CardIsLoading with
    | Some _ -> model |> noCmd
    | None ->
        match model.Card with
        | Some card when card.QuizId = quizId -> {model with Card = None; Form = None} |> noCmd
        | _ -> {model with Card = None; Form = None} |> loading quizId |> apiCmd api.getProdQuizCard {|QuizId = quizId|} GetCardResp Exn

let updateCard f model =
    match model.Card with
    | Some card -> {model with Card = Some <| f card}
    | _ -> model

let validateBrand txt =
    if System.String.IsNullOrWhiteSpace(txt) then "Brand is required" else ""

let validateName txt =
    if System.String.IsNullOrWhiteSpace(txt) then "Name is required" else ""

let validateStartTime dt =
    match dt with
    | Some _ -> ""
    | None -> "Start Time is required"

let validate (card:QuizProdCard) =
    [validateName card.Name; validateStartTime card.StartTime]
    |> List.filter (fun s -> s <> "")

let submitCard api model =
    match model.Card with
    | Some card ->
        model |> loading card.QuizId |> apiCmd api.updateProdQuizCard card SubmitCardResp Exn
    | None -> model |> noCmd

let replaceRecord record model =
    {model with Quizzes = record :: (model.Quizzes |> List.filter (fun q -> q.QuizId <> record.QuizId))}

let uploadFile quizId (api:IMainApi) (file:Browser.Types.File) model =
    if file.size > (1024*128) then model |> addError "max image size is 128K" |> noCmd
    else model |> loading quizId, uploadFileToS3Cmd api.getUploadUrl QuizImg file (fun key -> QuizImgUploaded (quizId,key)) Exn

let updateMixlrCode txt model =
    model |> updateCard (fun c ->
    { c with
        MixlrCode =
            match System.Int32.TryParse txt with
            | true, id when id > 0 -> Some id
            | _ -> None
    })

let toggleDeleteForm model =
    { model with Form = DeleteForm.Toggle model.Form}

let updateDeleteForm (f : DeleteForm -> DeleteForm) model =
    match model.Form with
    | Some form -> {model with Form = Some (f form)}
    | None -> model

let afterQuizDeletion (quizId:int) model =
    {model with Card = None; Form = None; Quizzes = model.Quizzes |> List.filter (fun q -> q.QuizId <> quizId)}

let init (api:IMainApi) user : Model*Cmd<Msg> =
    {Quizzes = []; Card = None; CardIsLoading = None; Errors = Map.empty; Form = None} |> apiCmd api.getProdQuizzes () GetQuizzesResp Exn

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg with
    | CreateQuiz -> cm |> apiCmd api.createQuiz () CreateQuizResp Exn
    | CreateQuizResp {Value = Ok res} -> {cm with Quizzes = res.Record :: cm.Quizzes; Card = Some res.Card} |> noCmd
    | GetQuizzesResp {Value = Ok res } -> {cm with Quizzes = res} |> noCmd
    | ToggleCard quizId -> cm |> toggleCard api quizId
    | GetCardResp {Value = Ok res } -> {cm with Card = Some res} |> editing |> noCmd
    | UpdateStartTime dt -> cm |> updateCard (fun c -> {c with StartTime = Some dt}) |> noCmd
    | UpdateName txt -> cm |> updateCard (fun c -> {c with Name = txt}) |> noCmd
    | UpdateEventPage txt -> cm |> updateCard (fun c -> {c with EventPage = txt}) |> noCmd
    | UpdateMixlrCode txt -> cm |> updateMixlrCode txt |> noCmd
    | UpdateWelcomeTxt txt -> cm |> updateCard (fun c -> {c with WelcomeText = txt}) |> noCmd
    | UpdateInfoTxt txt -> cm |> updateCard (fun c -> {c with InfoText = txt}) |> noCmd
    | UpdateFarewellTxt txt -> cm |> updateCard (fun c -> {c with FarewellText = txt}) |> noCmd
    | UpdateIsPremoderated b -> cm |> updateCard (fun c -> {c with WithPremoderation = b}) |> noCmd
    | CancelCard -> {cm with Card = None} |> noCmd
    | SubmitCard -> cm |> submitCard api
    | SubmitCardResp {Value = Ok res } -> {cm with Card = None} |> editing |> replaceRecord res |> noCmd
    | QuizImgClear _ ->  cm |> updateCard (fun c -> {c with ImgKey = ""}) |> noCmd
    | QuizImgChanged res -> cm |> uploadFile res.Tag api res.File
    | QuizImgUploaded (_,bucketKey) -> cm |> editing |> updateCard (fun c -> {c with ImgKey = bucketKey}) |> noCmd
    | ToggleDeleteForm -> cm |> toggleDeleteForm |> noCmd
    | DeleteFormUpdateText txt -> cm |> updateDeleteForm (fun form -> {form with ConfirmText = txt; FormError = ""}) |> noCmd
    | DeleteQuiz quizId -> cm |> updateDeleteForm (fun form -> {form with IsSending = true}) |> apiCmd api.deleteQuiz {|QuizId = quizId|} (taggedMsg DeleteQuizResp quizId) Exn
    | DeleteQuizResp {Tag = quizId; Rsp = {Value = Ok _}} -> cm |> afterQuizDeletion quizId |> noCmd
    | DeleteQuizResp {Tag = _; Rsp = {Value = Error txt}} -> cm |> updateDeleteForm (fun form -> {form with IsSending = false; FormError = txt}) |>  noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (appSettings:Settings) (model : Model) =
    div[][
        nav [Class "level"][
            div [Class "level-left"][

            ]
            div [Class "level-right"][
                p [Class "level-item"][
                    button [Class "button is-dark"; OnClick (fun _ -> dispatch CreateQuiz)][str "Create New Quiz"]
                ]
            ]
        ]

        MainTemplates.errors dispatch DeleteError model.Errors

        table [Class "table is-fullwidth is-hoverable"][
            thead[][
                tr[][
                    th [Style[Width "30px"]][str ""]
                    th [Style[Width "120px"]][str "Start Time"]
                    th [][str "Name"]
                    th [Style[Width "30px"]][str "Status"]
                    th [Style[Width "30px"]][str "Admin"]
                ]
            ]
            tbody [][
                for quiz in model.Quizzes |> List.sortBy (fun q -> q.QuizId) |> List.rev do
                    let hasCard = match model.Card with Some card when card.QuizId = quiz.QuizId -> true | _ -> false
                    let isLoading = match model.CardIsLoading with Some id when id = quiz.QuizId -> true | _ -> false

                    tr[][
                        th [] [
                            button [classList ["button", true; "is-small", true; "is-loading", isLoading]; OnClick (fun _ -> dispatch <| ToggleCard quiz.QuizId)][
                                str <| if hasCard then "-" else "+"
                            ]
                        ]
                        th [] [str <| match quiz.StartTime with Some st -> st.ToString("dd/MM HH:mm") | None -> "???"]
                        td [] [str quiz.Name]
                        td [] [str <| quiz.Status.ToString()]
                        td [] [
                                a [urlForAdmin quiz.QuizId quiz.AdminToken |> Href; Target "_blank"][
                                    str "link"
                                    span [Class "icon"][Fa.i [Fa.Solid.ExternalLinkAlt][]]
                                ]
                        ]
                    ]
                    if hasCard then
                        tr [][
                            th [][]
                            td [ColSpan 5] [ card dispatch appSettings model.Card.Value model.Form isLoading]
                        ]
            ]
        ]
    ]

let card (dispatch : Msg -> unit) (appSettings:Settings) (card : MainModels.QuizProdCard) (form : DeleteForm option) isLoading =
    div[][
        div [Class "columns"][
            div [Class "column"][
                div [Class "field"][
                    label [Class "label"][str "Quiz Name"; span [Class "has-text-danger"][str "*"]]
                    let error = validateName card.Name
                    div [Class "control"][
                        input [classList ["input", true; "is-danger", error <> ""]; Type "text"; Placeholder "BIZZ QUIZZ #3"; MaxLength 128.0;
                            valueOrDefault card.Name;
                            OnChange (fun ev -> dispatch <| UpdateName ev.Value)]

                    ]
                    p [Class "help is-danger"][str error]
                ]
                div [Class "field is-grouped"][
                    div [Class "control"][
                        label [Class "label"][str "Start Time"; span [Class "has-text-danger"][str "*"]]
                        let error = validateStartTime card.StartTime
                        let className = "input" + if error <> "" then " is-danger" else ""
                        let opts =  [ Flatpickr.ClassName className; Flatpickr.EnableTimePicker true; Flatpickr.TimeTwentyFour true;
                            Flatpickr.OnChange (UpdateStartTime >> dispatch)]
                        let opts =  match card.StartTime with Some dt -> (Flatpickr.Value dt) :: opts | _ -> opts
                        Flatpickr.flatpickr opts

                        p [Class "help is-danger"][str error]
                    ]
                ]

                div [Class "field"][
                    label [Class "label"][str "Welcome Message"]
                    div [Class "control"][
                        textarea [Class "textarea"; Placeholder "Describe your quiz"; MaxLength 512.0;
                            valueOrDefault card.WelcomeText;
                            OnChange (fun ev -> dispatch <| UpdateWelcomeTxt ev.Value )][]
                    ]
                ]
                div [Class "field"][
                    label [Class "label"][str "Farewell Message"]
                    div [Class "control"][
                        textarea [Class "textarea"; Placeholder "Say goodbye to your audience"; MaxLength 512.0;
                            valueOrDefault card.FarewellText;
                            OnChange (fun ev -> dispatch <| UpdateFarewellTxt ev.Value )][]
                    ]
                ]
                div [Class "field"][
                    label [Class "label"][str "Information for the registered teams"]
                    div [Class "control"][
                        textarea [Class "textarea"; Placeholder "private zoom link"; MaxLength 512.0;
                            valueOrDefault card.InfoText;
                            OnChange (fun ev -> dispatch <| UpdateInfoTxt ev.Value )][]
                    ]
                ]
            ]
            div [Class "column"][

                div [Class "field"][
                    label [Class "label"][str "Event Page"]
                    div [Class "control"][
                        input [Class "input"; Type "text"; Placeholder "Link to facebook event or telegram"; MaxLength 128.0;
                            valueOrDefault card.EventPage;
                            OnChange (fun ev -> dispatch <| UpdateEventPage ev.Value)]

                    ]
                ]

                div [Class "field"][
                    label [Class "label"][str "Mixlr User Id"]
                    div [Class "control"][
                        input [Class "input"; Type "number"; Placeholder ""; MaxLength 128.0;
                            valueOrDefault card.MixlrCode;
                            OnChange (fun ev -> dispatch <| UpdateMixlrCode ev.Value)]

                    ]
                    small[][
                        str "https://mixlr.com/users/"
                        span [Class "has-text-danger"][str "THISID"]
                        str "/embed from "
                        a[Href "https://mixlr.com/settings/embed/"][str "https://mixlr.com/settings/embed/"]
                    ]
                ]

                div [Class "field"][
                    label [Class "label"][str "Quiz picture (128x128) 128K size max"]

                    yield! MainTemplates.imgArea128 card.QuizId isLoading (QuizImgChanged>>dispatch) (QuizImgClear>>dispatch) appSettings.MediaHost card.ImgKey "/logo256.png" "Reset to default"
                ]

                div [Class "field"][
                    label [Class "label"][str "Application links"]
                    div [Class "content"][
                        let link caption href =
                            li[][str caption; str " "; a [Href href; Target "_blank"][
                                    str "link"; span [Class "icon"][Fa.i [Fa.Solid.ExternalLinkAlt][]]]]

                        ul[][
                            urlForAdmin card.QuizId card.AdminToken |> link "Admin"
                            urlForReg card.QuizId card.RegToken |> link "Registration"
                            urlForAud card.QuizId card.ListenToken |> link "Audience"
                            urlForResults card.QuizId card.Name card.ImgKey card.ListenToken |> link "Results Page"
                        ]
                    ]
                ]
                div [Class "field"][
                    div [Class "control"][
                        label [Class "checkbox"][
                            input [Type "checkbox"; Checked card.WithPremoderation; OnChange (fun ev -> dispatch <| UpdateIsPremoderated ev.Checked)]
                            str " registration is moderated"
                        ]
                    ]
                ]
            ]
        ]
        match form with
        | Some fm ->
            MainTemplates.deleteForm dispatch "Type name of the Quiz to confirm" card.Name fm.ConfirmText fm.IsSending fm.FormError
                ToggleDeleteForm DeleteFormUpdateText (DeleteQuiz card.QuizId)
        | None ->
            nav [Class "level"][
                div [Class "level-left"][
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
                div [Class "level-right"][
                    button [Class "button is-danger "; OnClick (fun _ -> dispatch ToggleDeleteForm)] [ str "Delete"]
                ]
            ]
    ]