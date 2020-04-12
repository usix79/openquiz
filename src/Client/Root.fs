module Root

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Common

type CurrentPage =
    //| Admin of Admin.Model
    | MainPage of Main.Model
    //| Captain of Captain.Model
    | EmptyPage of string

type Msg =
    | LoginResponse of RESP<{|Token: string; RefreshToken: string; User: User|}>
    | Exn of exn
    // | Action of RootAction
    // | Admin of Admin.Msg
    | MainMsg of Main.Msg
//    | Captain of Captain.Msg

type Model = {
    CurrentUser : User option
    CurrentPage : CurrentPage
}

let apiFactory = Infra.ApiFactory(fun () -> Infra.redirect "/login")
let securityApi = apiFactory.CreateSecurityApi()
let mainApi = apiFactory.CreateMainApi()

let getUserFromStorage() =
    let user = Infra.loadFromSessionStorage<User> "USER"
    let start = Infra.loadFromSessionStorage<System.DateTime> "START"
    match user, start with
    | Some user, Some start -> Some (user, start)
    | _ -> None

let initChildPage user start cm =
    match user with
    // | AdminUser u ->
    //     let api = currentModel.ApiFactory.CreateAdminApi()
    //     let submodel, subcmd = Admin.init api u start
    //     {currentModel with CurrentPage = CurrentPage.Admin submodel; CurrentUser = Some user}, Cmd.map Admin subcmd
    // | TeamUser u ->
    //     let api = currentModel.ApiFactory.CreateTeamApi()
    //     let submodel, subcmd = Main.init api u
    //     {currentModel with CurrentPage = CurrentPage.Main submodel; CurrentUser = Some user}, Cmd.map Main subcmd
    | MainUser u ->
        ///let api = currentModel.ApiFactory.CreateCaptainApi()
        let subModel, subCmd = Main.init mainApi u
        {cm with CurrentPage = MainPage subModel; CurrentUser = Some user}, Cmd.map MainMsg subCmd
    | _ -> cm |> noCmd

let saveUser token refreshToken user serverTime =
    apiFactory.UpdateTokens token refreshToken |> ignore
    Infra.saveToSessionStorage "USER" user
    Infra.saveToSessionStorage "START" serverTime

    Infra.clearQueryString() |> ignore


let evaluateLoginCmd api =
    let query = Infra.currentQueryString()
    match query.TryFind "code" with
    | Some code -> Some <| apiCmd' api.loginAsMainUser {|Code = code|} LoginResponse Exn
    | _ -> None

let init (): Model * Cmd<Msg> =
    let cm =  {CurrentPage = EmptyPage "Initializing..."; CurrentUser = None}

    match evaluateLoginCmd securityApi with
    | Some cmd -> cm, cmd
    | None ->
        match getUserFromStorage() with
        | Some (user,start) -> cm |> initChildPage user start
        | _ ->
            Infra.redirect "/"
            cm |> noCmd

let update (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match cm.CurrentPage, cm.CurrentUser, msg with
    | EmptyPage _, _, LoginResponse {Value = Ok res; ST = serverTime} ->
            saveUser res.Token res.RefreshToken res.User serverTime
            cm |> initChildPage res.User serverTime
    | EmptyPage _, _, LoginResponse {Value = Error txt} -> {cm with CurrentPage = EmptyPage txt} |> noCmd
    | EmptyPage _,  _, Exn ex -> {cm with CurrentPage = EmptyPage ex.Message} |> noCmd
    | MainPage subModel, Some (MainUser user), MainMsg subMsg ->
        let user =
            match subMsg with
            | Main.Msg.Public (MainPub.Msg.BecomeProducerResp {Value = Ok _}) ->
                let user = {user with IsProducer = true}
                Infra.saveToSessionStorage "USER" (MainUser user)
                user
            | _ -> user

        let newModel,newCmd = Main.update mainApi user subMsg subModel
        {cm with CurrentPage = MainPage newModel; CurrentUser = Some (MainUser user)}, Cmd.map MainMsg newCmd
    | _, _, _ -> cm |> noCmd

    //match msg, currentModel.CurrentPage, currentModel.CurrentUser with
    // | Action action, _, _ -> execute action currentModel

    // | Login subMsg, CurrentPage.Login subModel, _ ->
    //     match subMsg with
    //     | Login.Msg.LoginResult {Value = Ok res; ServerTime = serverTime} ->

    //         // save all data we need after refresh
    //         currentModel.ApiFactory.UpdateTokens res.Token res.RefreshToken |> ignore
    //         saveToSessionStorage "USER" res.User
    //         saveToSessionStorage "START" serverTime

    //         initChildPage currentModel res.User serverTime
    //     | _ ->
    //         let api = currentModel.ApiFactory.CreateSecurityApi()
    //         let subModel, subCmd = Login.update api subMsg subModel
    //         {currentModel with CurrentPage = CurrentPage.Login subModel}, Cmd.map Login subCmd
    // | Admin subMsg, CurrentPage.Admin subState, Some (AdminUser _) ->
    //     let api = currentModel.ApiFactory.CreateAdminApi()
    //     let subModel, subCmd = Admin.update api subMsg subState
    //     {currentModel with CurrentPage = CurrentPage.Admin subModel}, Cmd.map Admin subCmd
    // | Main mainPageMsg, CurrentPage.Main mainPageState, _ ->
    //     let subModel, subCmd = Main.update mainPageMsg mainPageState
    //     {currentModel with CurrentPage = CurrentPage.Main subModel}, Cmd.map Main subCmd
    // | Captain subMsg, CurrentPage.Captain subState, _ ->
    //     let api = currentModel.ApiFactory.CreateCaptainApi()
    //     let subModel, subCmd = Captain.update api subMsg subState
    //     {currentModel with CurrentPage = CurrentPage.Captain subModel}, Cmd.map Captain subCmd
    //| _, _, _ -> currentModel, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let pageHtml =
        match model.CurrentPage, model.CurrentUser with
        | EmptyPage txt, None -> str txt
        | MainPage subModel, Some (MainUser user) -> Main.view (MainMsg >> dispatch) user subModel
        // | CurrentPage.Captain submodel, _ -> Captain.view (Captain >> dispatch) submodel
        // | CurrentPage.Main submodel, Some (TeamUser user) -> Main.view (Main >> dispatch) user submodel
        // | CurrentPage.Admin submodel, _ -> Admin.view (Admin >> dispatch) submodel
        | _ -> str "Oops"

    div [] [pageHtml]