module Root

open Elmish
open Fable.React

open Shared
open Common

type CurrentPage =
    | EmptyPage of string
    | MainPage of Main.Model
    | AdminPage of Admin.Model
    | TeamPage of Team.Model
    | RegPage of Reg.Model
    | AudPage of Aud.Model
    | PrivatePage

type Msg =
    | LoginResponse of RESP<{|Token: string; RefreshToken: string; User: User|}>
    | Exn of exn
    | MainMsg of Main.Msg
    | AdminMsg of Admin.Msg
    | TeamMsg of Team.Msg
    | RegMsg of Reg.Msg
    | AudMsg of Aud.Msg

type Model = {
    CurrentUser : User option
    CurrentPage : CurrentPage
}

let apiFactory = Infra.ApiFactory(fun () -> Infra.redirect "/login")
let securityApi = apiFactory.CreateSecurityApi()
let mainApi = apiFactory.CreateMainApi()
let adminApi = apiFactory.CreateAdminApi()
let teamApi = apiFactory.CreateTeamApi()
let regApi = apiFactory.CreateRegApi()
let audApi = apiFactory.CreateAudApi()

let getUserFromStorage() =
    Infra.loadFromSessionStorage<User> "USER"

let initChildPage user cm =
    match user with
    | RegUser _ ->
        let submodel, subcmd = Reg.init regApi
        {cm with CurrentPage = RegPage submodel; CurrentUser = Some user}, Cmd.map RegMsg subcmd
    | MainUser u ->
        let submodel, subCmd = Main.init mainApi u
        {cm with CurrentPage = MainPage submodel; CurrentUser = Some user}, Cmd.map MainMsg subCmd
    | AdminUser u->
        let submodel, subcmd = Admin.init adminApi u
        {cm with CurrentPage = AdminPage submodel; CurrentUser = Some user}, Cmd.map AdminMsg subcmd
    | TeamUser u ->
        let submodel, subcmd = Team.init teamApi u
        {cm with CurrentPage = TeamPage submodel; CurrentUser = Some user}, Cmd.map TeamMsg subcmd
    | AudUser u->
        let submodel, subcmd = Aud.init audApi
        {cm with CurrentPage = AudPage submodel; CurrentUser = Some user}, Cmd.map AudMsg subcmd

let saveUser token refreshToken user =
    apiFactory.UpdateTokens token refreshToken |> ignore
    Infra.saveToSessionStorage "USER" user

let evaluateLoginReq (query : Map<string,string>) =
    let qs x = query.TryFind x
    let qi x = qs x |> ofInt

    match qs "code" with
    | Some code -> Some <| LoginReq.MainUser {|Code = code|}
    | _ ->
        match qs "who", qi "quiz", qs "token", qi "team" with
        | Some "admin", Some quizId, Some token, _  -> LoginReq.AdminUser {|QuizId = quizId; Token = token|} |> Some
        | Some "reg", Some quizId, Some token, _  -> LoginReq.RegUser {|QuizId = quizId; Token = token|} |> Some
        | Some "aud", Some quizId, Some token, _  -> LoginReq.AudUser {|QuizId = quizId; Token = token|} |> Some
        | Some "team", Some quizId, Some token, Some teamId  -> LoginReq.TeamUser {|QuizId = quizId; TeamId = teamId; Token = token|} |> Some
        | _ -> None

let isReqForSameUser (req:LoginReq) (user:User) =
    match req, user with
    | LoginReq.MainUser _, MainUser _ -> true
    | LoginReq.AdminUser data, AdminUser usr -> data.QuizId = usr.QuizId
    | LoginReq.AudUser data, AudUser usr -> data.QuizId = usr.QuizId
    | LoginReq.TeamUser data, TeamUser usr -> data.QuizId = usr.QuizId && data.TeamId = usr.TeamId
    | _ -> false

let init (): Model * Cmd<Msg> =
    let cm =  {CurrentPage = EmptyPage "Initializing..."; CurrentUser = None}

    let u = getUserFromStorage()
    match u, evaluateLoginReq (Infra.currentQueryString()) with
    | Some user, Some req when isReqForSameUser req user -> cm |> initChildPage user
    | _, Some req -> cm |> apiCmd securityApi.login req LoginResponse Exn
    | Some user, None -> cm |> initChildPage user
    | _ ->
        Infra.redirect "/"
        cm |> noCmd

let update (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match cm.CurrentPage, cm.CurrentUser, msg with
    | EmptyPage _, _, LoginResponse {Value = Ok res} ->
            saveUser res.Token res.RefreshToken res.User

            match res.User with
            | MainUser _ -> Infra.clearQueryString()
            | _ -> ()

            cm |> initChildPage res.User
    | EmptyPage _, _, LoginResponse {Value = Error txt} -> {cm with CurrentPage = EmptyPage txt} |> noCmd
    | EmptyPage _,  _, Exn ex -> {cm with CurrentPage = EmptyPage ex.Message} |> noCmd
    | MainPage subModel, Some (MainUser user), MainMsg subMsg ->
        let user =
            match subMsg with
            | Main.Msg.BecomeProducerResp {Value = Ok _} ->
                let user = {user with IsProducer = true}
                Infra.saveToSessionStorage "USER" (MainUser user)
                user
            | _ -> user
        let newModel,newCmd = Main.update mainApi user subMsg subModel
        {cm with CurrentPage = MainPage newModel; CurrentUser = Some (MainUser user)}, Cmd.map MainMsg newCmd
    | AdminPage subModel, Some (AdminUser user), AdminMsg subMsg ->
        let newModel,newCmd = Admin.update adminApi user subMsg subModel
        {cm with CurrentPage = AdminPage newModel}, Cmd.map AdminMsg newCmd
    | TeamPage subModel, Some (TeamUser user), TeamMsg subMsg ->
        let newModel,newCmd = Team.update teamApi user subMsg subModel
        {cm with CurrentPage = TeamPage newModel}, Cmd.map TeamMsg newCmd
    | RegPage subModel, Some (RegUser _), RegMsg subMsg ->
        let newModel,newCmd = Reg.update regApi subMsg subModel
        {cm with CurrentPage = RegPage newModel}, Cmd.map RegMsg newCmd
    | AudPage subModel, Some (AudUser u), AudMsg subMsg ->
        let newModel,newCmd = Aud.update audApi u subMsg subModel
        {cm with CurrentPage = AudPage newModel}, Cmd.map AudMsg newCmd
    | _, _, _ -> cm |> noCmd

let view (model : Model) (dispatch : Msg -> unit) =
    let pageHtml =
        match model.CurrentPage, model.CurrentUser with
        | EmptyPage txt, None -> str txt
        | MainPage subModel, Some (MainUser user) -> Main.view (MainMsg >> dispatch) user subModel
        | AdminPage subModel, Some (AdminUser user) -> Admin.view (AdminMsg >> dispatch) user subModel
        | TeamPage subModel, Some (TeamUser user) -> Team.view (TeamMsg >> dispatch) user subModel
        | RegPage subModel, Some (RegUser user) -> Reg.view (RegMsg >> dispatch) subModel
        | AudPage subModel, Some (AudUser user) -> Aud.view (AudMsg >> dispatch) subModel
        | _ -> str "Oops"

    div [] [pageHtml]