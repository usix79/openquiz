module Root

open Elmish
open Fable.React

open OpenQuiz.Shared
open Common
open AppSync

type CurrentPage =
    | EmptyPage of string
    | MainPage of Main.Model
    | AdminPage of Admin.Model
    | TeamPage of Team.Model
    | RegPage of Reg.Model
    | AudPage of Aud.Model
    | PrivatePage

type Msg =
    | LoginResponse of
        RESP<{| Token: string
                RefreshToken: string
                User: User
                Settings: Settings |}>
    | Exn of exn
    | MainMsg of Main.Msg
    | AdminMsg of Admin.Msg
    | TeamMsg of Team.Msg
    | RegMsg of Reg.Msg
    | AudMsg of Aud.Msg

type Model =
    { CurrentUser: User option
      CurrentSettings: Settings option
      CurrentPage: CurrentPage }

let apiFactory = Infra.ApiFactory(fun () -> Infra.redirect "/login")
let securityApi = apiFactory.CreateSecurityApi()
let mainApi = apiFactory.CreateMainApi()
let adminApi = apiFactory.CreateAdminApi()
let teamApi = apiFactory.CreateTeamApi()
let regApi = apiFactory.CreateRegApi()
let audApi = apiFactory.CreateAudApi()

let initChildPage user settings cm =
    let cm =
        { cm with
            CurrentSettings = Some settings }

    match user with
    | RegUser _ ->
        let submodel, subcmd = Reg.init regApi

        { cm with
            CurrentPage = RegPage submodel
            CurrentUser = Some user },
        Cmd.map RegMsg subcmd
    | MainUser u ->
        let submodel, subCmd = Main.init mainApi u

        { cm with
            CurrentPage = MainPage submodel
            CurrentUser = Some user },
        Cmd.map MainMsg subCmd
    | AdminUser u ->
        let submodel, subcmd = Admin.init adminApi u

        { cm with
            CurrentPage = AdminPage submodel
            CurrentUser = Some user },
        Cmd.map AdminMsg subcmd
    | TeamUser u ->
        let submodel, subcmd = Team.init teamApi u

        { cm with
            CurrentPage = TeamPage submodel
            CurrentUser = Some user },
        Cmd.map TeamMsg subcmd
    | AudUser u ->
        let submodel, subcmd = Aud.init audApi u

        { cm with
            CurrentPage = AudPage submodel
            CurrentUser = Some user },
        Cmd.map AudMsg subcmd

let saveUser token refreshToken user settings =
    apiFactory.UpdateTokens token refreshToken |> ignore
    Infra.saveUser user
    Infra.saveSettings settings

let evaluateLoginReq (query: Map<string, string>) =
    let qs x = query.TryFind x
    let qi x = qs x |> ofInt

    match qs "code" with
    | Some code -> Some <| LoginReq.MainUser {| Code = code |}
    | _ ->
        match qs "who", qi "quiz", qs "token", qi "team" with
        | Some "admin", Some quizId, Some token, _ -> LoginReq.AdminUser {| QuizId = quizId; Token = token |} |> Some
        | Some "reg", Some quizId, Some token, _ -> LoginReq.RegUser {| QuizId = quizId; Token = token |} |> Some
        | Some "aud", Some quizId, Some token, _ -> LoginReq.AudUser {| QuizId = quizId; Token = token |} |> Some
        | Some "team", Some quizId, Some token, Some teamId ->
            LoginReq.TeamUser
                {| QuizId = quizId
                   TeamId = teamId
                   Token = token |}
            |> Some
        | _ -> None

let isReqForSameUser (req: LoginReq) (user: User) =
    match req, user with
    | LoginReq.MainUser _, MainUser _ -> true
    | LoginReq.AdminUser data, AdminUser usr -> data.QuizId = usr.QuizId
    | LoginReq.AudUser data, AudUser usr -> data.QuizId = usr.QuizId
    | LoginReq.TeamUser data, TeamUser usr -> data.QuizId = usr.QuizId && data.TeamId = usr.TeamId
    | _ -> false

let init () : Model * Cmd<Msg> =
    let cm =
        { CurrentPage = EmptyPage "Initializing..."
          CurrentUser = None
          CurrentSettings = None }

    let u = Infra.loadUser ()
    let s = Infra.loadSettings ()

    match u, s, evaluateLoginReq (Infra.currentQueryString ()) with
    | Some user, Some settings, Some req when isReqForSameUser req user -> cm |> initChildPage user settings
    | _, _, Some req -> cm |> apiCmd securityApi.login req LoginResponse Exn
    | Some user, Some settings, None -> cm |> initChildPage user settings
    | _ ->
        Infra.redirect "/"
        cm |> noCmd

let update (msg: Msg) (cm: Model) : Model * Cmd<Msg> =
    match cm.CurrentPage, cm.CurrentUser, msg with
    | EmptyPage _, _, LoginResponse { Value = Ok res } ->
        saveUser res.Token res.RefreshToken res.User res.Settings

        match res.User with
        | MainUser _ -> Infra.clearQueryString ()
        | _ -> ()

        cm |> initChildPage res.User res.Settings
    | EmptyPage _, _, LoginResponse { Value = Error txt } -> { cm with CurrentPage = EmptyPage txt } |> noCmd
    | EmptyPage _, _, Exn ex ->
        { cm with
            CurrentPage = EmptyPage ex.Message }
        |> noCmd
    | MainPage subModel, Some(MainUser user), MainMsg subMsg ->
        let user =
            match subMsg with
            | Main.Msg.BecomeProducerResp { Value = Ok _ } ->
                let user = { user with IsProducer = true }
                Infra.saveUser (MainUser user)
                user
            | _ -> user

        let newModel, newCmd = Main.update mainApi user subMsg subModel

        { cm with
            CurrentPage = MainPage newModel
            CurrentUser = Some(MainUser user) },
        Cmd.map MainMsg newCmd
    | AdminPage subModel, Some(AdminUser user), AdminMsg subMsg ->
        let newModel, newCmd = Admin.update adminApi user subMsg subModel

        { cm with
            CurrentPage = AdminPage newModel },
        Cmd.map AdminMsg newCmd
    | TeamPage subModel, Some(TeamUser user), TeamMsg subMsg ->
        let newModel, newCmd = Team.update teamApi user subMsg subModel

        { cm with
            CurrentPage = TeamPage newModel },
        Cmd.map TeamMsg newCmd
    | RegPage subModel, Some(RegUser _), RegMsg subMsg ->
        let newModel, newCmd = Reg.update regApi subMsg subModel

        { cm with
            CurrentPage = RegPage newModel },
        Cmd.map RegMsg newCmd
    | AudPage subModel, Some(AudUser u), AudMsg subMsg ->
        let newModel, newCmd = Aud.update audApi u subMsg subModel

        { cm with
            CurrentPage = AudPage newModel },
        Cmd.map AudMsg newCmd
    | _, _, _ -> cm |> noCmd

let view (model: Model) (dispatch: Msg -> unit) =
    let pageHtml =
        match model.CurrentPage, model.CurrentUser, model.CurrentSettings with
        | EmptyPage txt, None, None -> str txt
        | MainPage subModel, Some(MainUser user), Some settings ->
            Main.view (MainMsg >> dispatch) user settings subModel
        | AdminPage subModel, Some(AdminUser user), Some settings ->
            Admin.view (AdminMsg >> dispatch) user settings subModel
        | TeamPage subModel, Some(TeamUser user), Some settings ->
            let l10n = L10n.teamL10n (Infra.getPreferableLangugage ())
            Team.view (TeamMsg >> dispatch) user settings subModel l10n
        | RegPage subModel, Some(RegUser user), Some settings ->
            let l10n = L10n.regL10n (Infra.getPreferableLangugage ())
            Reg.view (RegMsg >> dispatch) settings subModel l10n
        | AudPage subModel, Some(AudUser user), Some settings ->
            let l10n = L10n.audienceL10n (Infra.getPreferableLangugage ())
            Aud.view (AudMsg >> dispatch) settings subModel user l10n
        | _ -> str "Oops"

    div [] [ pageHtml ]