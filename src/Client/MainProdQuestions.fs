module rec MainProdQuestions

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.FontAwesome

open Shared
open Common

type PkgQwKey = {PackageId:int; TourIdx:int; QwIdx:int}
    with
        member x.Key = {TourIdx = x.TourIdx; QwIdx = x.QwIdx}
        member x.IdxOfQw (idx:int) = (x.TourIdx, idx)

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
    | AppendSingleSlip of qwCount : int
    | AppendMultipleSlip
    | AppendQwToMultiple of int
    | DelSlip of slipIdx:int
    | DelQwInMultiple of key:QwKey
    | UpdateTourName of tourIdx:int * txt:string
    | QwTextChanged of key:QwKey * txt:string
    | QwTextSplitChanged of key:QwKey * part:int * txt:string
    | QwAnswerChanged of key:QwKey * string
    | QwCommentChanged of key:QwKey * string
    | QwPointsChanged of key:QwKey * txt:string
    | QwJpdPointsChanged of key:QwKey * txt:string
    | QwWithChoiceChanged of key:QwKey * bool
    | QwImgChanged of {|Type:string; Body:byte[]; Tag:PkgQwKey|}
    | QwImgClear of key:QwKey
    | UploadQwImgResp of TRESP<QwKey, {|BucketKey:string|}>
    | CommentImgChanged of {|Type:string; Body:byte[]; Tag:PkgQwKey|}
    | CommentImgClear of key:QwKey
    | UploadCommentImgResp of TRESP<QwKey, {|BucketKey:string|}>
    | ToggleAquireForm
    | AquireFormUpdatePackageId of string
    | AquireFormUpdateTransferToken of string
    | AquireFormSend
    | AquireFormCancel
    | AquirePackageResp of RESP<PackageRecord>
    | ToggleDeleteForm
    | DeleteFormUpdateText of string
    | DeletePackage of int
    | DeletePackageResp of TRESP<int,unit>

type AquireForm = {
    PackageId : int
    TransferToken : string
    IsSending : bool
}

type Model = {
    Packages : PackageRecord list
    Errors : Map<string, string>
    CardIsLoading : int option // packageId
    Card : PackageCard option
    AquiringForm : AquireForm option
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

let updateSlip (qwKey:QwKey) (f:SingleAwSlip -> SingleAwSlip) (model:Model) =
    model |> updateCard (fun card ->
        match card.GetSlip qwKey.TourIdx with
        | Some (Single slip) -> f slip  |> Single |> card.UpdateSlip qwKey.TourIdx
        | Some (Multiple (name, slips)) ->
            (name, slips |> List.mapi (fun idx slip -> if idx = qwKey.QwIdx then f slip else slip)) |> Multiple
            |> card.UpdateSlip qwKey.TourIdx
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

let updateAquiringForm (f : AquireForm -> AquireForm) (model:Model) =
    match model.AquiringForm with
    | Some form -> {model with AquiringForm = Some (f form)}
    | None -> model

let sendAquiringForm (api:IMainApi) model =
    match model.AquiringForm with
    | Some form when form.PackageId > 0 && not <| System.String.IsNullOrWhiteSpace(form.TransferToken) ->
        model
        |> updateAquiringForm (fun form -> {form with IsSending = true})
        |> apiCmd api.aquirePackage {|PackageId = form.PackageId; TransferToken = form.TransferToken.Trim()|} AquirePackageResp Exn
    | _ -> model |> noCmd

let toggleDeleteForm model =
    { model with DeleteForm = DeleteForm.Toggle model.DeleteForm}

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
    | AppendSingleSlip qwCount -> cm |> updateCard (fun c -> c.AppendSlip (SingleAwSlip.InitEmpty qwCount |> Single)) |> noCmd
    | AppendMultipleSlip -> cm |> updateCard (fun c -> c.AppendSlip (("",[]) |> Multiple)) |> noCmd
    | DelSlip tourIdx -> cm |> updateCard (fun c -> c.DelSlip tourIdx) |> noCmd
    | UpdateTourName (tourIdx,txt) -> cm |> updateCard (fun c -> c.GetSlip tourIdx |> Option.bind (fun slip -> slip.SetMultipleName txt |>  c.UpdateSlip tourIdx |> Some) |> Option.defaultValue c) |> noCmd
    | AppendQwToMultiple tourIdx -> cm |> updateCard (fun c -> c.GetSlip tourIdx |> Option.bind (fun slip -> SingleAwSlip.InitEmpty 1 |> slip.AppendToMultiple |>  c.UpdateSlip tourIdx |> Some) |> Option.defaultValue c) |> noCmd
    | DelQwInMultiple key -> cm |> updateCard (fun c -> c.GetSlip key.TourIdx |> Option.bind (fun slip -> slip.RemoveFromMultiple key.QwIdx |>  c.UpdateSlip key.TourIdx |> Some) |> Option.defaultValue c) |> noCmd
    | QwTextChanged (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Question = Solid txt}) |> noCmd
    | QwTextSplitChanged (key,part,txt) -> cm |> updateSlip key (fun slip -> slip.SetQwText part txt) |> noCmd
    | QwAnswerChanged (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Answer = txt}) |> noCmd
    | QwCommentChanged (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Comment = txt}) |> noCmd
    | QwPointsChanged (key,txt) -> cm |> updateSlip key (fun slip -> {slip with Points = System.Decimal.Parse(txt)}) |> noCmd
    | QwJpdPointsChanged (key,txt) -> cm |> updateSlip key (fun slip -> {slip with JeopardyPoints = ofDecimal (Some txt)}) |> noCmd
    | QwWithChoiceChanged (key,v) -> cm |> updateSlip key (fun slip -> {slip with WithChoice = v}) |> noCmd
    | QwImgChanged res -> cm |> uploadFile res.Tag.PackageId api (taggedMsg UploadQwImgResp res.Tag.Key) res.Type res.Body
    | QwImgClear key -> cm |> updateSlip key (fun slip -> {slip with ImgKey = ""}) |> noCmd
    | UploadQwImgResp {Tag = key; Rsp = {Value = Ok res}} -> cm |> editing |> updateSlip key (fun slip -> {slip with ImgKey = res.BucketKey}) |> noCmd
    | CommentImgChanged res -> cm |> uploadFile res.Tag.PackageId api (taggedMsg UploadCommentImgResp res.Tag.Key) res.Type res.Body
    | CommentImgClear key -> cm |> updateSlip key (fun slip -> {slip with CommentImgKey = ""}) |> noCmd
    | UploadCommentImgResp {Tag = key; Rsp = {Value = Ok res}} -> cm |> editing |> updateSlip key (fun slip -> {slip with CommentImgKey = res.BucketKey}) |> noCmd
    | ToggleAquireForm -> cm |> toggleAquiringForm |> noCmd
    | AquireFormUpdatePackageId txt-> cm |> updateAquiringForm (fun form -> {form with PackageId = System.Int32.Parse(txt)}) |> noCmd
    | AquireFormUpdateTransferToken txt-> cm |> updateAquiringForm (fun form -> {form with TransferToken = txt.Trim()}) |> noCmd
    | AquireFormCancel -> {cm with AquiringForm = None} |> noCmd
    | AquireFormSend -> cm |> sendAquiringForm api
    | AquirePackageResp {Value = Ok record} -> {cm with Packages = record :: cm.Packages; AquiringForm = None} |> noCmd
    | AquirePackageResp {Value = Error txt} -> cm |> addError txt |> updateAquiringForm (fun form -> {form with IsSending = false}) |> noCmd
    | ToggleDeleteForm -> cm |> toggleDeleteForm |> noCmd
    | DeleteFormUpdateText txt -> cm |> updateDeleteForm (fun form -> {form with ConfirmText = txt; FormError = ""}) |> noCmd
    | DeletePackage packageId -> cm |> updateDeleteForm (fun form -> {form with IsSending = true}) |> apiCmd api.deletePackage {|PackageId = packageId|} (taggedMsg DeletePackageResp packageId) Exn
    | DeletePackageResp {Tag = quizId; Rsp = {Value = Ok _}} -> cm |> afterPackageDeletion quizId |> noCmd
    | DeletePackageResp {Tag = _; Rsp = {Value = Error txt}} -> cm |> updateDeleteForm (fun form -> {form with IsSending = false; FormError = txt}) |>  noCmd
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
                        button [Class "button is-dark"; OnClick (fun _ -> dispatch ToggleAquireForm)][str "Acquire Package"]
                    ]

                p [Class "level-item"][
                    button [Class "button is-dark"; OnClick (fun _ -> dispatch CreatePackage)][str "Create New Package"]
                ]
            ]
        ]

        MainTemplates.errors dispatch DeleteError model.Errors

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

let aquiringForm (dispatch : Msg -> unit) (form : AquireForm) =
    div [Class "field has-addons"; Style[PaddingBottom "5px"]][
        p [Class "control"][
            input [Class "input"; Disabled form.IsSending; Type "number"; Placeholder "Package Id"; MaxLength 8.0;
                valueOrDefault (if form.PackageId > 0 then form.PackageId.ToString() else "");
                OnChange (fun ev -> dispatch <| AquireFormUpdatePackageId ev.Value)]
        ]
        p [Class "control is-expanded"][
            input [Class "input"; Disabled form.IsSending; Type "text"; Placeholder "Transfer Token"; MaxLength 128.0;
                valueOrDefault form.TransferToken;
                OnChange (fun ev -> dispatch <| AquireFormUpdateTransferToken ev.Value)]
        ]
        p [Class "control"][
            button [classList ["button", true; "has-text-danger", true; "has-text-weight-semibold", true; "is-loading", form.IsSending];
              Disabled (form.PackageId <= 0 || (System.String.IsNullOrWhiteSpace form.TransferToken));
              OnClick (fun _ -> dispatch AquireFormSend)][str "Acquire"]
        ]
        p [Class "control"][
            button [classList ["button", true]; OnClick (fun _ -> dispatch AquireFormCancel)][str "Cancel"]
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
                        div [Class "field is-grouped"][
                            div [Class "control"][
                                button [Class "button "; Disabled isLoading; OnClick (fun _ -> AppendSingleSlip 1 |> dispatch)] [str "Append Question"]
                            ]
                            div [Class "control"][
                                button [Class "button "; Disabled isLoading; OnClick (fun _ -> AppendSingleSlip 2 |> dispatch)] [str "Append Doublet"]
                            ]
                            div [Class "control"][
                                button [Class "button "; Disabled isLoading; OnClick (fun _ -> AppendSingleSlip 3 |> dispatch)] [str "Append Blitz"]
                            ]
                            div [Class "control"][
                                button [Class "button "; Disabled isLoading; OnClick (fun _ -> AppendMultipleSlip |> dispatch)] [str "Append Multiple Slip"]
                            ]
                        ]
                    ]
                ]
            ]
            div [Class "level-right"][
                match deleteForm with
                | Some form ->
                    MainTemplates.deleteForm dispatch "Type name of the Package to confirm" card.Name form.ConfirmText form.IsSending form.FormError
                        ToggleDeleteForm DeleteFormUpdateText (DeletePackage card.PackageId)
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
                    th [Style [Width "90px"]] [str "Points"]
                    th [Style [Width "30px"]] [str "Del"]
                ]
            ]

            tbody [][
                for (slipIdx,slip) in card.Slips |> List.indexed |> List.rev do
                    match slip with
                    | Single s -> yield singleSlipRow dispatch isLoading card.PackageId slipIdx None s
                    | Multiple (name, slips) ->
                        yield multipleSlipRowHeader dispatch isLoading card.PackageId slipIdx name
                        for (qwIdx,slip) in slips |> List.indexed |> List.rev do
                            yield singleSlipRow dispatch isLoading card.PackageId slipIdx (Some qwIdx) slip
            ]
        ]
    ]

let idxCell tourIdx qwIdx =
    td[] [
        str <| (tourIdx + 1).ToString()
        match qwIdx with
        | Some idx -> str <| sprintf ".%i" (idx + 1)
        | None -> ()
    ]

let delTourCell dispatch tourIdx =
    td[] [
        button [Class "button is-small"; OnClick(fun _ -> dispatch <| DelSlip (tourIdx))][Fa.i [ Fa.Regular.TrashAlt ] [ ]]
    ]

let delQwCell dispatch qwKey =
    td[] [
        button [Class "button is-small"; OnClick(fun _ -> dispatch <| DelQwInMultiple qwKey)][Fa.i [ Fa.Regular.TrashAlt ] [ ]]
    ]

let qwCell dispatch (key:PkgQwKey) txt imgKey isLoading =
    td[] [
        textarea [Class "textarea"; valueOrDefault txt; MaxLength 512.0; OnChange (fun ev -> QwTextChanged (key.Key,ev.Value) |> dispatch)][]
        br[]
        yield! MainTemplates.imgArea key isLoading (QwImgChanged >> dispatch) (fun _ -> QwImgClear key.Key |> dispatch) imgKey "" "Clear"
    ]

let qwInput dispatch placeholder txt key partIdx  =
    div [Class "control"; Style [MarginBottom "3px"]][
        input [Class "input"; Type "text"; Placeholder placeholder;
            valueOrDefault txt; MaxLength 256.0; OnChange (fun ev -> QwTextSplitChanged (key, partIdx, ev.Value) |> dispatch)]
    ]

let awCell dispatch (key:PkgQwKey) txt isLoading =
    td[] [
        textarea [Class "textarea"; valueOrDefault txt; MaxLength 512.0; OnChange (fun ev -> QwAnswerChanged (key.Key,ev.Value) |> dispatch)][]
    ]

let cmntCell dispatch (key:PkgQwKey) txt imgKey isLoading =
    td[] [
        textarea [Class "textarea"; valueOrDefault txt; MaxLength 512.0; OnChange (fun ev -> QwCommentChanged (key.Key,ev.Value) |> dispatch)][]
        br[]
        yield! MainTemplates.imgArea key isLoading (CommentImgChanged >> dispatch) (fun _ -> CommentImgClear key.Key |> dispatch) imgKey "" "Clear"
    ]

let singleSlipRow dispatch isLoading pkgId tourIdx qwIdx (slip: SingleAwSlip) =

    let packageKey = {PackageId = pkgId; TourIdx=tourIdx; QwIdx = qwIdx |> Option.defaultValue 0}

    tr[][
        idxCell tourIdx qwIdx
        match slip.Question with
        | Solid qw -> qwCell dispatch packageKey qw slip.ImgKey isLoading
        | Split list ->
            td[] [
                for (idx,qw) in list |> List.indexed do
                    qwInput dispatch (sprintf "Question %i" (idx + 1)) qw packageKey.Key idx
                br[]
                yield! MainTemplates.imgArea packageKey isLoading (QwImgChanged >> dispatch) (fun _ -> (QwImgClear packageKey.Key) |> dispatch) slip.ImgKey "" "Clear"
            ]
        awCell dispatch packageKey slip.Answer isLoading
        cmntCell dispatch packageKey slip.Comment slip.CommentImgKey isLoading
        td[][
            div [Class "control"][
                input [Class "input"; Type "number";
                    valueOrDefault slip.Points; MaxLength 4.; ReadOnly isLoading; OnChange (fun ev -> QwPointsChanged (packageKey.Key,ev.Value) |> dispatch)]
            ]
            str "Jeopardy"
            div [Class "control"][
                input [Class "input"; Type "number";
                    valueOrDefault slip.JeopardyPoints; MaxLength 4.; ReadOnly isLoading; OnChange (fun ev -> QwJpdPointsChanged (packageKey.Key,ev.Value) |> dispatch)]
            ]
            label [Class "checkbox"][
                input [Type "checkbox";
                    Checked slip.WithChoice; ReadOnly isLoading; OnChange (fun ev -> QwWithChoiceChanged (packageKey.Key, (ev.Checked)) |> dispatch)]
                str " with choice"
            ]
        ]
        match qwIdx with
        | Some idx -> delQwCell dispatch {TourIdx = tourIdx; QwIdx = idx}
        | None -> delTourCell dispatch tourIdx
    ]

let multipleSlipRowHeader dispatch isLoading pkgId tourIdx (name:string) =
    tr[][
        idxCell tourIdx None
        td[ColSpan 3][
            div [Class "field has-addons"; Style[PaddingBottom "5px"]][
                p [Class "control is-expanded"][
                    input [Class "input"; Disabled isLoading; Type "text"; Placeholder "Tour Name"; MaxLength 128.0;
                        valueOrDefault name;
                        OnChange (fun ev -> dispatch <| UpdateTourName (tourIdx,ev.Value))]
                ]
                p [Class "control"][
                    button [classList ["button", true; "has-text-weight-semibold", true; "is-loading", isLoading];
                      OnClick (fun _ -> dispatch <| AppendQwToMultiple tourIdx)][str "Append Question To Slip"]
                ]
            ]
        ]
        td[][]
        delTourCell dispatch tourIdx
    ]