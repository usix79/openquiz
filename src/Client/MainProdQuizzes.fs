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
    | UpdateBrand of string
    | UpdateStartTime of System.DateTime
    | UpdateName of string
    | UpdateMixlrCode of string
    | UpdateEventPage of string
    | UpdateStatus of string
    | UpdateWelcomeTxt of string
    | UpdateFarewellTxt of string
    | UpdateIsPrivat of bool
    | UpdateIsPremoderated of bool
    | CancelCard
    | SubmitCard
    | SubmitCardResp of RESP<QuizProdRecord>
    | QuizImgChanged of {|Type:string; Body:byte[]; Tag:int|}
    | QuizImgClear of unit
    | UploadQuizImgResp of TRESP<int, {|BucketKey:string|}>
    | ToggleDeleteForm
    | DeleteFormUpdateText of string
    | DeleteQuiz of int
    | DeleteQuizResp of TRESP<int,unit>

type DeleteForm = {
    ConfirmText : string
    Error : string
    IsSending : bool
}

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

let validate card =
    [validateBrand card.Brand; validateName card.Name; validateStartTime card.StartTime]
    |> List.filter (fun s -> s <> "")

let submitCard api model =
    match model.Card with
    | Some card ->
        model |> loading card.QuizId |> apiCmd api.updateProdQuizCard card SubmitCardResp Exn
    | None -> model |> noCmd

let replaceRecord record model =
    {model with Quizzes = record :: (model.Quizzes |> List.filter (fun q -> q.QuizId <> record.QuizId))}

let uploadFile quizId (api:IMainApi) respMsg fileType body model =
    if Array.length body > (1024*128) then
        model |> addError "max image size is 128K" |> noCmd
    else
        model |> loading quizId |> apiCmd api.uploadFile {|Cat = Quiz; FileType=fileType; FileBody=body|} respMsg Exn

let updateMixlrCode txt model =
    model |> updateCard (fun c ->
    { c with
        MixlrCode =
            match System.Int32.TryParse txt with
            | true, id when id > 0 -> Some id
            | _ -> None
    })

let toggleDeleteForm model =
    { model with
        Form =
            match model.Form with
            | Some _ -> None
            | None -> Some {ConfirmText = ""; Error = ""; IsSending = false}
    }

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
    | UpdateBrand txt -> cm |> updateCard (fun c -> {c with Brand = txt.ToUpper()}) |> noCmd
    | UpdateStartTime dt -> cm |> updateCard (fun c -> {c with StartTime = Some dt}) |> noCmd
    | UpdateName txt -> cm |> updateCard (fun c -> {c with Name = txt}) |> noCmd
    | UpdateStatus txt -> cm |> updateCard (fun c -> {c with Status = defaultArg (fromString txt) Draft}) |> noCmd
    | UpdateEventPage txt -> cm |> updateCard (fun c -> {c with EventPage = txt}) |> noCmd
    | UpdateMixlrCode txt -> cm |> updateMixlrCode txt |> noCmd
    | UpdateWelcomeTxt txt -> cm |> updateCard (fun c -> {c with WelcomeText = txt}) |> noCmd
    | UpdateFarewellTxt txt -> cm |> updateCard (fun c -> {c with FarewellText = txt}) |> noCmd
    | UpdateIsPrivat b -> cm |> updateCard (fun c -> {c with IsPrivate = b}) |> noCmd
    | UpdateIsPremoderated b -> cm |> updateCard (fun c -> {c with WithPremoderation = b}) |> noCmd
    | CancelCard -> {cm with Card = None} |> noCmd
    | SubmitCard -> cm |> submitCard api
    | SubmitCardResp {Value = Ok res } -> {cm with Card = None} |> editing |> replaceRecord res |> noCmd
    | QuizImgClear _ ->  cm |> updateCard (fun c -> {c with ImgKey = ""}) |> noCmd
    | QuizImgChanged res -> cm |> uploadFile res.Tag api (taggedMsg UploadQuizImgResp res.Tag) res.Type res.Body
    | UploadQuizImgResp {Tag = _; Rsp = {Value = Ok res}} -> cm |> editing |> updateCard (fun c -> {c with ImgKey = res.BucketKey}) |> noCmd
    | ToggleDeleteForm -> cm |> toggleDeleteForm |> noCmd
    | DeleteFormUpdateText txt -> cm |> updateDeleteForm (fun form -> {form with ConfirmText = txt; Error = ""}) |> noCmd
    | DeleteQuiz quizId -> cm |> updateDeleteForm (fun form -> {form with IsSending = true}) |> apiCmd api.deleteQuiz {|QuizId = quizId|} (taggedMsg DeleteQuizResp quizId) Exn
    | DeleteQuizResp {Tag = quizId; Rsp = {Value = Ok _}} -> cm |> afterQuizDeletion quizId |> noCmd
    | DeleteQuizResp {Tag = _; Rsp = {Value = Error txt}} -> cm |> updateDeleteForm (fun form -> {form with IsSending = false; Error = txt}) |>  noCmd
    | DeleteError id -> cm |> delError id |> noCmd
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
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

        for error in model.Errors do
            div [Class "notification is-danger is-light"][
                button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                str error.Value
            ]

        table [Class "table is-fullwidth is-hoverable"][
            thead[][
                tr[][
                    th [Style[Width "30px"]][str ""]
                    th [Style[Width "120px"]][str "Brand"]
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
                        th [] [str quiz.Brand]
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
                            td [ColSpan 5] [ card dispatch model.Card.Value model.Form isLoading]
                        ]
            ]
        ]
    ]

let card (dispatch : Msg -> unit) (card : MainModels.QuizProdCard) (form : DeleteForm option) isLoading =
    div[][
        div [Class "columns"][
            div [Class "column"][
                div [Class "field"][
                    label [Class "label"][str "Brand"; span [Class "has-text-danger"][str "*"]]
                    let error = validateBrand card.Brand
                    div [Class "control"][
                        input [classList ["input", true; "is-danger", error <> ""]; Type "text"; Placeholder "Your brand"; MaxLength 12.0;
                            valueOrDefault card.Brand;
                            OnChange (fun ev -> dispatch <| UpdateBrand ev.Value)]
                    ]
                    p [Class "help is-danger"][str error]
                ]
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

                    div [Class "control"][
                        label [Class "label"][str "Status"]
                        div [Class "select"][
                            select[valueOrDefault card.Status; OnChange (fun ev -> dispatch <| UpdateStatus ev.Value )][
                                for case in Reflection.FSharpType.GetUnionCases typeof<QuizStatus> do
                                    option [][str case.Name]
                            ]
                        ]
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
                    label [Class "label"][str "Mixler User Id"]
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
                    label [Class "label"][str "Quiz picture (128x128)"]

                    yield! MainTemplates.imgArea card.QuizId isLoading (QuizImgChanged>>dispatch) (QuizImgClear>>dispatch) card.ImgKey "/logo256.png" "Reset to default"
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
                        ]
                    ]
                ]
                div [Class "field" ] [
                    div [Class "control"][
                        label [Class "checkbox"][
                            input [Type "checkbox"; Checked card.IsPrivate; OnChange (fun ev -> dispatch <| UpdateIsPrivat ev.Checked)]
                            str " the quiz is private (will not be displayed on the main page)"
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
        | Some fm -> div [][ deleteForm dispatch fm card.QuizId card.Name]
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

let deleteForm dispatch form quizId quizName=
    div[][
        div [Class "field has-addons"; Style[PaddingBottom "5px"]][
            p [Class "control is-expanded"][
                input [Class "input"; Disabled form.IsSending; Type "text"; Placeholder "Type name of the Quiz to confirm"; MaxLength 128.0;
                    valueOrDefault form.ConfirmText;
                    OnChange (fun ev -> dispatch <| DeleteFormUpdateText ev.Value)]
            ]
            p [Class "control"][
                button [classList ["button", true; "has-text-danger", true; "has-text-weight-semibold", true; "is-loading", form.IsSending];
                  Disabled (form.ConfirmText <> quizName);
                  OnClick (fun _ -> dispatch <| DeleteQuiz quizId)][str "Delete"]
            ]
            p [Class "control"][
                button [classList ["button", true]; OnClick (fun _ -> dispatch ToggleDeleteForm)][str "Cancel"]
            ]
        ]
        p [Class "help is-danger"][str form.Error]
    ]

