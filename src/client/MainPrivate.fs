module MainPrivate

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
    | Empty

type Model = {
    Errors : Map<string, string>
}

let init api user : Model*Cmd<Msg> =
    {Errors = Map.empty}, Cmd.none

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    cm, Cmd.none