module AdminCP

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared
open Shared.AdminModels
open Common

type Msg =
    | Empty

type Model = {
    Error : string
}

let init (api:IAdminApi) user st : Model*Cmd<Msg> =
    {Error = ""}, Cmd.none

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    str "CP"