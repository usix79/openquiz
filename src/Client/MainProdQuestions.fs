module rec MainProdQuestions

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.FontAwesome

open Shared
open Common
open MainModels

type QwKey = {
    PackageId:int
    Idx:int
}

type Msg =
    | Exn of exn
    | CreatePackage
    | DeleteError of string
    | GetPackagesResp of RESP<PackageRecord list>
    | ToggleCard of int
    | GetPackageResp of RESP<PackageCard>
    | CreatePackageResp of RESP<{|Record : PackageRecord; Card:PackageCard|}>
    | UpdateName of string
    | UpdateTransferToken of string
    | CancelCard
    | SubmitCard
    | SubmitCardResp of RESP<PackageRecord>
    | AppendQuiestion
    | DelQuestion of int
    | QwTextChanged of int * string
    | QwAnswerChanged of int * string
    | QwCommentChanged of int * string
    | QwImgChanged of {|Type:string; Body:byte[]; Tag:QwKey|}
    | QwImgClear of QwKey
    | UploadQwImgResponse of TRESP<QwKey, {|BucketKey:string|}>
    | CommentImgChanged of {|Type:string; Body:byte[]; Tag:QwKey|}
    | CommentImgClear of QwKey
    | UploadCommentImgResponse of TRESP<QwKey, {|BucketKey:string|}>
    | ToggleAquiringForm
    | AquiringFormUpdatePackageId of string
    | AquiringFormUpdateTransferToken of string
    | AquiringFormSend
    | AquiringFormCancel
    | AquiringResp of RESP<PackageRecord>
    | ToggleDeleteForm
    | DeleteFormUpdateText of string
    | DeletePackage of int
    | DeletePackageResp of TRESP<int,unit>

type AquiringForm = {
    PackageId : int
    TransferToken : string
    IsSending : bool
}

type DeleteForm = {
    ConfirmText : string
    Error : string
    IsSending : bool
}

type Model = {
    Packages : PackageRecord list
    Errors : Map<string, string>
    CardIsLoading : int option // packageId
    Card : PackageCard option
    AquiringForm : AquiringForm option
    DeleteForm : DeleteForm option
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let loading pkgId model =
    {model with CardIsLoading = Some pkgId}

let toggleCard api packageId model =
    match model.CardIsLoading with
    | Some _ -> model |> noCmd
    | None ->
        match model.Card with
        | Some card when card.PackageId = packageId -> {model with Card = None} |> noCmd
        | _ -> {model with Card = None} |> loading packageId |> apiCmd api.getProdPackageCard {|PackageId = packageId|} GetPackageResp Exn

let editing model =
    {model with CardIsLoading = None}

let validateName txt =
    if System.String.IsNullOrWhiteSpace(txt) then "Name is required" else ""

let validate (card : PackageCard) =
    [validateName card.Name]
    |> List.filter (fun s -> s <> "")

let updateCard f model =
    match model.Card with
    | Some card -> {model with Card = Some <| f card}
    | _ -> model

let updateQw idx f model =
    model
    |> updateCard (fun card ->
        match card.GetQuestion idx with
        | Some qw -> f qw |> card.UpdateQuestion idx
        | None -> card
    )

let submitCard api model =
    match model.Card with
    | Some card ->
        model |> loading card.PackageId |> apiCmd api.updateProdPackageCard card SubmitCardResp Exn
    | None -> model |> noCmd

let replaceRecord record model =
    {model with Packages = record :: (model.Packages |> List.filter (fun q -> q.PackageId <> record.PackageId))}

let uploadFile packageId (api:IMainApi) respMsg fileType body model =
    if Array.length body > (1024*128) then
        model |> addError "max image size is 128K" |> noCmd
    else
        model |> loading packageId |> apiCmd api.uploadFile {|Cat = Question; FileType=fileType; FileBody=body|} respMsg Exn

let toggleAquiringForm model =
    {model with AquiringForm = (match model.AquiringForm with Some _ -> None | None -> Some {PackageId = 0; TransferToken = ""; IsSending = false})}

let updateAquiringForm (f : AquiringForm -> AquiringForm) (model:Model) =
    match model.AquiringForm with
    | Some form -> {model with AquiringForm = Some (f form)}
    | None -> model

let sendAquiringForm (api:IMainApi) model =
    match model.AquiringForm with
    | Some form when form.PackageId > 0 && not <| System.String.IsNullOrWhiteSpace(form.TransferToken) ->
        model
        |> updateAquiringForm (fun form -> {form with IsSending = true})
        |> apiCmd api.aquirePackage {|PackageId = form.PackageId; TransferToken = form.TransferToken.Trim()|} AquiringResp Exn
    | _ -> model |> noCmd

let toggleDeleteForm model =
    { model with
        DeleteForm =
            match model.DeleteForm with
            | Some _ -> None
            | None -> Some {ConfirmText = ""; Error = ""; IsSending = false}
    }

let updateDeleteForm (f : DeleteForm -> DeleteForm) model =
    match model.DeleteForm with
    | Some form -> {model with DeleteForm = Some (f form)}
    | None -> model

let afterPackageDeletion (packageId:int) model =
    {model with Card = None; DeleteForm = None; Packages = model.Packages |> List.filter (fun p -> p.PackageId <> packageId)}

let init api user : Model*Cmd<Msg> =
    {Errors = Map.empty; Packages = []; CardIsLoading = None; Card = None; AquiringForm = None; DeleteForm = None} |> apiCmd api.getProdPackages () GetPackagesResp Exn

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =

    match msg with
    | DeleteError id -> cm |> delError id |> noCmd
    | GetPackagesResp {Value = Ok res } -> {cm with Packages = res} |> noCmd
    | ToggleCard quizId -> cm |> toggleCard api quizId
    | GetPackageResp {Value = Ok res } -> {cm with Card = Some res} |> editing |> noCmd
    | CreatePackage -> cm |> apiCmd api.createPackage () CreatePackageResp Exn
    | CreatePackageResp {Value = Ok res} -> {cm with Packages = res.Record :: cm.Packages; Card = Some res.Card} |> noCmd
    | UpdateName txt -> cm |> updateCard (fun c -> {c with Name = txt}) |> noCmd
    | UpdateTransferToken txt -> cm |> updateCard (fun c -> {c with TransferToken = txt}) |> noCmd
    | CancelCard -> {cm with Card = None} |> noCmd
    | SubmitCard -> cm |> submitCard api
    | SubmitCardResp {Value = Ok res } -> {cm with Card = None} |> editing |> replaceRecord res |> noCmd
    | AppendQuiestion -> cm |> updateCard (fun c -> c.AddQuestion()) |> noCmd
    | DelQuestion idx -> cm |> updateCard (fun c -> c.DelQuestion idx) |> noCmd
    | QwTextChanged (idx,txt) -> cm |> updateQw idx (fun qw -> {qw with Text = txt}) |> noCmd
    | QwCommentChanged (idx,txt) -> cm |> updateQw idx (fun qw -> {qw with Comment = txt}) |> noCmd
    | QwAnswerChanged (idx,txt) -> cm |> updateQw idx (fun qw -> {qw with Answer = txt}) |> noCmd
    | QwImgChanged res -> cm |> uploadFile res.Tag.PackageId api (taggedMsg UploadQwImgResponse res.Tag) res.Type res.Body
    | QwImgClear qwKey -> cm |> updateQw qwKey.Idx (fun qw -> {qw with ImgKey = ""}) |> noCmd
    | UploadQwImgResponse {Tag = qwKey; Rsp = {Value = Ok res}} -> cm |> editing |> updateQw qwKey.Idx (fun qw -> {qw with ImgKey = res.BucketKey}) |> noCmd
    | CommentImgChanged res -> cm |> uploadFile res.Tag.PackageId api (taggedMsg UploadCommentImgResponse res.Tag) res.Type res.Body
    | CommentImgClear qwKey -> cm |> updateQw qwKey.Idx (fun qw -> {qw with CommentImgKey = ""}) |> noCmd
    | UploadCommentImgResponse {Tag = qwKey; Rsp = {Value = Ok res}} -> cm |> editing |> updateQw qwKey.Idx (fun qw -> {qw with CommentImgKey = res.BucketKey}) |> noCmd
    | ToggleAquiringForm -> cm |> toggleAquiringForm |> noCmd
    | AquiringFormUpdatePackageId txt-> cm |> updateAquiringForm (fun form -> {form with PackageId = System.Int32.Parse(txt)}) |> noCmd
    | AquiringFormUpdateTransferToken txt-> cm |> updateAquiringForm (fun form -> {form with TransferToken = txt.Trim()}) |> noCmd
    | AquiringFormCancel -> {cm with AquiringForm = None} |> noCmd
    | AquiringFormSend -> cm |> sendAquiringForm api
    | AquiringResp {Value = Ok record} -> {cm with Packages = record :: cm.Packages; AquiringForm = None} |> noCmd
    | AquiringResp {Value = Error txt} -> cm |> addError txt |> updateAquiringForm (fun form -> {form with IsSending = false}) |> noCmd
    | ToggleDeleteForm -> cm |> toggleDeleteForm |> noCmd
    | DeleteFormUpdateText txt -> cm |> updateDeleteForm (fun form -> {form with ConfirmText = txt; Error = ""}) |> noCmd
    | DeletePackage packageId -> cm |> updateDeleteForm (fun form -> {form with IsSending = true}) |> apiCmd api.deletePackage {|PackageId = packageId|} (taggedMsg DeletePackageResp packageId) Exn
    | DeletePackageResp {Tag = quizId; Rsp = {Value = Ok _}} -> cm |> afterPackageDeletion quizId |> noCmd
    | DeletePackageResp {Tag = _; Rsp = {Value = Error txt}} -> cm |> updateDeleteForm (fun form -> {form with IsSending = false; Error = txt}) |>  noCmd
    | Exn ex -> cm |> addError ex.Message |> editing |> noCmd
    | Err txt -> cm |> addError txt |> editing |> noCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
    div[][
        nav [Class "level"][
            div [Class "level-left"][

            ]
            div [Class "level-right"][
                if model.AquiringForm.IsNone then
                    p [Class "level-item"][
                        button [Class "button is-dark"; OnClick (fun _ -> dispatch ToggleAquiringForm)][str "Acquire Package"]
                    ]

                p [Class "level-item"][
                    button [Class "button is-dark"; OnClick (fun _ -> dispatch CreatePackage)][str "Create New Package"]
                ]
            ]
        ]

        for error in model.Errors do
            div [Class "notification is-danger is-light"][
                button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                str error.Value
            ]

        div[][
            match model.AquiringForm with
            | Some form -> yield aquiringForm dispatch form
            | None -> ()
        ]

        table [Class "table is-fullwidth is-hoverable"][
            thead[][
                tr[][
                    th [Style[Width "30px"]][str ""]
                    th [Style[Width "30px"]][str "Id"]
                    th [][str "Name"]
                ]
            ]
            tbody [][
                for package in model.Packages |> List.sortBy (fun q -> q.PackageId) |> List.rev do
                    let hasCard = match model.Card with Some card when card.PackageId = package.PackageId -> true | _ -> false
                    let isLoading = match model.CardIsLoading with Some id when id = package.PackageId -> true | _ -> false

                    tr[][
                        th [] [
                            button [classList ["button", true; "is-small", true; "is-loading", isLoading]; OnClick (fun _ -> dispatch <| ToggleCard package.PackageId)][
                                str <| if hasCard then "-" else "+"
                            ]
                        ]
                        td [] [str <| package.PackageId.ToString()]
                        td [] [str package.Name]
                    ]
                    if hasCard then
                        tr [][
                            th [][]
                            td [ColSpan 2] [ card dispatch model.Card.Value model.DeleteForm isLoading]
                        ]
            ]
        ]
    ]

let aquiringForm (dispatch : Msg -> unit) (form : AquiringForm) =
    div [Class "field has-addons"; Style[PaddingBottom "5px"]][
        p [Class "control"][
            input [Class "input"; Disabled form.IsSending; Type "number"; Placeholder "Package Id"; MaxLength 8.0;
                valueOrDefault (if form.PackageId > 0 then form.PackageId.ToString() else "");
                OnChange (fun ev -> dispatch <| AquiringFormUpdatePackageId ev.Value)]
        ]
        p [Class "control is-expanded"][
            input [Class "input"; Disabled form.IsSending; Type "text"; Placeholder "Transfer Token"; MaxLength 128.0;
                valueOrDefault form.TransferToken;
                OnChange (fun ev -> dispatch <| AquiringFormUpdateTransferToken ev.Value)]
        ]
        p [Class "control"][
            button [classList ["button", true; "has-text-danger", true; "has-text-weight-semibold", true; "is-loading", form.IsSending];
              Disabled (form.PackageId <= 0 || (System.String.IsNullOrWhiteSpace form.TransferToken));
              OnClick (fun _ -> dispatch AquiringFormSend)][str "Acquire"]
        ]
        p [Class "control"][
            button [classList ["button", true]; OnClick (fun _ -> dispatch AquiringFormCancel)][str "Cancel"]
        ]
    ]

let card (dispatch : Msg -> unit) (card : PackageCard) (deleteForm : DeleteForm option) isLoading =
    div[][
        nav [Class "level"][
            div [Class "level-left"][
                div [Class "level-item"][
                    div [Class "field"; Style [Width "320px"]][
                        label [Class "label"][str "Package Name"; span [Class "has-text-danger"][str "*"]]
                        let error = validateName card.Name
                        div [Class "control"][
                            input [classList ["input", true; "is-danger", error <> ""]; Disabled isLoading; Type "text"; Placeholder "PKG #3"; MaxLength 128.0;
                                valueOrDefault card.Name;
                                OnChange (fun ev -> dispatch <| UpdateName ev.Value)]
                        ]
                        p [Class "help is-danger"][str error]
                        label [Class "label"][str "Transfer token"]
                        div [Class "control"][
                            input [Class "input"; Disabled isLoading; Type "text"; MaxLength 128.0;
                                valueOrDefault card.TransferToken;
                                OnChange (fun ev -> dispatch <| UpdateTransferToken ev.Value)]
                        ]
                        br[]
                        div [Class "field"][
                            div [Class "control"][
                                button [Class "button "; Disabled isLoading; OnClick (fun _ -> dispatch AppendQuiestion)] [str "Append Question"]
                            ]
                        ]
                    ]
                ]
            ]
            div [Class "level-right"][
                match deleteForm with
                | Some form -> div [][deleteFormEl dispatch form card.PackageId card.Name]
                | None ->
                    div [Class "level-item"][
                        div [Class "control"][
                            let hasErrors = not (validate card |> List.isEmpty)
                            button [Class "button is-dark "; Disabled hasErrors; OnClick (fun _ -> dispatch SubmitCard)] [ str "Submit"]
                        ]
                        div [Class "control"; Style [MarginLeft "5px"]][
                            button [Class "button"; OnClick (fun _ -> dispatch CancelCard)] [ str "Cancel"]
                        ]
                    ]
                    button [Class "button is-danger"; OnClick (fun _ -> dispatch ToggleDeleteForm)] [ str "Delete Package"]
            ]
        ]

        table [Class "table is-fullwidth"][
            thead [][
                tr [][
                    th [Style [Width "30px"]] [str "#"]
                    th [] [str "Question"]
                    th [] [str "Answer"]
                    th [] [str "Comment"]
                ]
            ]

            let rows =
                card.Questions
                |> List.mapi (qwRow dispatch isLoading card.PackageId)
                |> List.rev

            tbody [] rows
        ]

    ]

let qwRow dispatch isLoading pkgId idx (qw: PackageQuestion) =

    let qwKey = {PackageId = pkgId; Idx=idx}

    tr[][
        td[] [
            str <| (idx + 1).ToString()
            br []
            br []
            br []
            button [Class "button is-small"; OnClick(fun _ -> dispatch <| DelQuestion (idx))][Fa.i [ Fa.Regular.TrashAlt ] [ ]]
        ]
        td[] [
            textarea [Class "textarea"; valueOrDefault qw.Text; MaxLength 512.0; OnChange (fun ev -> QwTextChanged (idx,ev.Value) |> dispatch)][]
            br[]
            yield! MainTemplates.imgArea qwKey isLoading (QwImgChanged >> dispatch) (fun _ -> QwImgClear qwKey |> dispatch) qw.ImgKey "" "Clear"
        ]
        td[] [
            textarea [Class "textarea"; valueOrDefault qw.Answer; MaxLength 512.0; OnChange (fun ev -> QwAnswerChanged (idx,ev.Value) |> dispatch)][]
        ]
        td[] [
            textarea [Class "textarea"; valueOrDefault qw.Comment; MaxLength 512.0; OnChange (fun ev -> QwCommentChanged (idx,ev.Value) |> dispatch)][]
            br[]
            yield! MainTemplates.imgArea qwKey isLoading (CommentImgChanged >> dispatch) (fun _ -> CommentImgClear qwKey |> dispatch) qw.CommentImgKey "" "Clear"
        ]
    ]

let deleteFormEl dispatch form quizId quizName=
    div[][
        div [Class "field has-addons"; Style[PaddingBottom "5px"]][
            p [Class "control is-expanded"][
                input [Class "input"; Disabled form.IsSending; Type "text"; Placeholder "Type name of the Package to confirm"; MaxLength 128.0;
                    valueOrDefault form.ConfirmText;
                    OnChange (fun ev -> dispatch <| DeleteFormUpdateText ev.Value)]
            ]
            p [Class "control"][
                button [classList ["button", true; "has-text-danger", true; "has-text-weight-semibold", true; "is-loading", form.IsSending];
                  Disabled (form.ConfirmText <> quizName);
                  OnClick (fun _ -> dispatch <| DeletePackage quizId)][str "Delete"]
            ]
            p [Class "control"][
                button [classList ["button", true]; OnClick (fun _ -> dispatch ToggleDeleteForm)][str "Cancel"]
            ]
        ]
        p [Class "help is-danger"][str form.Error]
    ]
