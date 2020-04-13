module Root

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Common

type CurrentPage =
    | EmptyPage of string
    | MainPage of Main.Model
    | AdminPage of Admin.Model

type Msg =
    | LoginResponse of RESP<{|Token: string; RefreshToken: string; User: User|}>
    | Exn of exn
    | MainMsg of Main.Msg
    | AdminMsg of Admin.Msg

type Model = {
    CurrentUser : User option
    CurrentPage : CurrentPage
}

let apiFactory = Infra.ApiFactory(fun () -> Infra.redirect "/login")
let securityApi = apiFactory.CreateSecurityApi()
let mainApi = apiFactory.CreateMainApi()
let adminApi = apiFactory.CreateAdminApi()

let getUserFromStorage() =
    let user = Infra.loadFromSessionStorage<User> "USER"
    let start = Infra.loadFromSessionStorage<System.DateTime> "START"
    match user, start with
    | Some user, Some start -> Some (user, start)
    | _ -> None

let initChildPage user start cm =
    match user with
    | MainUser u ->
        let submodel, subCmd = Main.init mainApi u
        {cm with CurrentPage = MainPage submodel; CurrentUser = Some user}, Cmd.map MainMsg subCmd
    | AdminUser u ->
        let submodel, subcmd = Admin.init mainApi u start
        {cm with CurrentPage = AdminPage submodel; CurrentUser = Some user}, Cmd.map AdminMsg subcmd
    // | TeamUser u ->
    //     let api = currentModel.ApiFactory.CreateTeamApi()
    //     let submodel, subcmd = Main.init api u
    //     {currentModel with CurrentPage = CurrentPage.Main submodel; CurrentUser = Some user}, Cmd.map Main subcmd
    | _ -> cm |> noCmd

let saveUser token refreshToken user serverTime =
    apiFactory.UpdateTokens token refreshToken |> ignore
    Infra.saveToSessionStorage "USER" user
    Infra.saveToSessionStorage "START" serverTime

let evaluateLoginReq (query : Map<string,string>) =
    let qs x = query.TryFind x
    let qi x = qs x |> ofInt

    match qs "code" with
    | Some code -> Some <| LoginReq.MainUser {|Code = code|}
    | _ ->
        match qs "who", qi "quiz", qs "token", qi "team" with
        | Some "admin", Some quizId, Some token, _  ->
            Some <| LoginReq.AdminUser {|QuizId = quizId; Token = token|}
        | _ -> None

let isReqForSameUser (req:LoginReq) (user:User) =
    match req, user with
    | LoginReq.MainUser _, MainUser _ -> true
    | LoginReq.AdminUser data, AdminUser usr -> data.QuizId = usr.QuizId
    | _ -> false

let init (): Model * Cmd<Msg> =
    let cm =  {CurrentPage = EmptyPage "Initializing..."; CurrentUser = None}

    match getUserFromStorage(), evaluateLoginReq (Infra.currentQueryString()) with
    | Some (user,start), Some req when isReqForSameUser req user -> cm |> initChildPage user start
    | _, Some req -> cm |> apiCmd securityApi.login req LoginResponse Exn
    | Some (user,start), None -> cm |> initChildPage user start
    | _ ->
        Infra.redirect "/"
        cm |> noCmd

let update (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match cm.CurrentPage, cm.CurrentUser, msg with
    | EmptyPage _, _, LoginResponse {Value = Ok res; ST = serverTime} ->
            saveUser res.Token res.RefreshToken res.User serverTime

            match res.User with
            | MainUser _ -> Infra.clearQueryString()
            | _ -> ()

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
    | AdminPage subModel, Some (AdminUser user), AdminMsg subMsg ->
        let newModel,newCmd = Admin.update mainApi user subMsg subModel
        {cm with CurrentPage = AdminPage newModel}, Cmd.map AdminMsg newCmd
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
        | AdminPage subModel, Some (AdminUser user) -> Admin.view (AdminMsg >> dispatch) user subModel
        // | CurrentPage.Captain submodel, _ -> Captain.view (Captain >> dispatch) submodel
        // | CurrentPage.Main submodel, Some (TeamUser user) -> Main.view (Main >> dispatch) user submodel
        // | CurrentPage.Admin submodel, _ -> Admin.view (Admin >> dispatch) submodel
        | _ -> str "Oops"

    div [] [pageHtml]