module AdminResults

open Elmish
open Elmish.React

open Shared
open Common

type Msg =
    | Exn of exn

type Model = {
    Errors : Map<string, string>
}

let init (api:IAdminApi) user : Model*Cmd<Msg> =
    {Errors = Map.empty} |> noCmd

let update (api:IAdminApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    cm |> noCmd

let view (dispatch : Msg -> unit) (user:AdminUser) (model : Model) =
    MainTemplates.resultsViewEmb' true user.QuizId user.ListenToken None