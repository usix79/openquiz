module Admin

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared
open Common

type Msg =
    | Empty

type Model = {
    Error : string
}

let init api user st : Model*Cmd<Msg> =
    {Error = ""}, Cmd.none

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    cm, Cmd.none

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    str "ADMIN"