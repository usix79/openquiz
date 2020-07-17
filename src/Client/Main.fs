module rec Main

open Elmish
open Fable.React
open Fable.FontAwesome
open Fable.React.Props
open Elmish.React

open Shared
open Common


type Msg =
    | Reg of MainReg.Msg
    | Prod of MainProd.Msg
    | AcceptTermsOfUse
    | CancelTermsOfUse
    | BecomeProducerResp of RESP<unit>
    | Exn of exn

type Model = {
    Area : Area
}

type Area =
    | Public of MainReg.Model
    | Prod of MainProd.Model
    | TermOfUse of error:string


let init api (user:MainUser) : Model*Cmd<Msg> =
    let area,cmd =
        if user.IsPrivate then
           let subModel,subCmd = MainReg.init api user
           Public subModel, Cmd.map Msg.Reg subCmd
        else if user.IsProducer then
           let subModel,subCmd = MainProd.init api user
           Prod subModel, Cmd.map Msg.Prod subCmd
        else TermOfUse "" |> noCmd

    {Area = area}, cmd

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg, cm.Area with
    | CancelTermsOfUse, TermOfUse _ -> Infra.clearUserAndSettingsAndRedirect "/"; cm|> noCmd
    | AcceptTermsOfUse, TermOfUse _ -> cm |> apiCmd api.becomeProducer () BecomeProducerResp Exn
    | BecomeProducerResp {Value = Ok _}, TermOfUse _ ->
        let subModel,subCmd = MainProd.init api user
        {cm with Area = Prod subModel}, Cmd.map Msg.Prod subCmd
    | BecomeProducerResp {Value = Error txt}, TermOfUse _ -> {cm with Area = TermOfUse txt} |> noCmd
    | Exn ex, TermOfUse _ -> {cm with Area = TermOfUse ex.Message} |> noCmd
    | Msg.Reg subMsg, Public subModel ->
        let subModel,subCmd = MainReg.update api user subMsg subModel
        {cm with Area = Public subModel}, Cmd.map Msg.Reg subCmd
    | Msg.Prod subMsg, Prod subModel ->
        let subModel,subCmd = MainProd.update api user subMsg subModel
        {cm with Area = Prod subModel}, Cmd.map Msg.Prod subCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (settings:Settings) (model : Model) =
    match model.Area with
    | Public subModel ->
        let l10n = L10n.regL10n (Infra.getPreferableLangugage())
        MainReg.view (Msg.Reg >> dispatch) user settings subModel l10n
    | Prod subModel -> MainProd.view (Msg.Prod >> dispatch) user settings subModel
    | TermOfUse txt ->
        div[][
            if txt <> "" then
                h1 [Class "title is-1 has-text-centered has-text-danger"][str txt]
            else
                MainTemplates.termsOfUse dispatch AcceptTermsOfUse CancelTermsOfUse
        ]
