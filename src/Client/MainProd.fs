module MainProd

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fulma

open Shared
open Common

type Msg =
    | ToggleBurger
    | Logout
    | Quizzes of MainProdQuizzes.Msg
    | Questions of MainProdQuestions.Msg
    | Settings of MainProdSettings.Msg
    | SwitchToQuizzes
    | SwitchToQuestions
    | SwitchToSettings
    | Exn of exn
    | DeleteError of string

type Area =
    | Quizzes of MainProdQuizzes.Model
    | Questions of MainProdQuestions.Model
    | Settings of MainProdSettings.Model

type Model = {
    Area : Area
    IsBurgerOpen : bool
    Errors : Map<string, string>
}

let addError txt model =
    {model with Errors = model.Errors.Add(System.Guid.NewGuid().ToString(),txt)}

let delError id model =
    {model with Errors = model.Errors.Remove id}

let init (api:IMainApi) user : Model*Cmd<Msg> =
    let subModel,subCmd = MainProdQuizzes.init api user
    {Area =Quizzes subModel; Errors = Map.empty; IsBurgerOpen = false}, Cmd.map Msg.Quizzes subCmd

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg, cm.Area with
    | SwitchToQuizzes, _ ->
        let subModel,subCmd = MainProdQuizzes.init api user
        {cm with Area = Quizzes subModel}, Cmd.map Msg.Quizzes subCmd
    | SwitchToQuestions, _ ->
        let subModel,subCmd = MainProdQuestions.init api user
        {cm with Area = Questions subModel}, Cmd.map Msg.Questions subCmd
    | SwitchToSettings, _ ->
        let subModel,subCmd = MainProdSettings.init api user
        {cm with Area = Settings subModel}, Cmd.map Msg.Settings subCmd
    | Msg.Quizzes subMsg, Quizzes subModel ->
        let subModel,subCmd = MainProdQuizzes.update api user subMsg subModel
        {cm with Area = Quizzes subModel}, Cmd.map Msg.Quizzes subCmd
    | Msg.Questions subMsg, Questions subModel ->
        let subModel,subCmd = MainProdQuestions.update api user subMsg subModel
        {cm with Area = Questions subModel}, Cmd.map Msg.Questions subCmd
    | Msg.Settings subMsg, Settings subModel ->
        let subModel,subCmd = MainProdSettings.update api user subMsg subModel
        {cm with Area = Settings subModel}, Cmd.map Msg.Settings subCmd
    | ToggleBurger, _ -> {cm with IsBurgerOpen = not cm.IsBurgerOpen} |> noCmd
    | Logout, _ ->  Infra.clearUserAndSettingsAndRedirect "/"; cm|> noCmd
    | Err txt, _ -> cm |> addError txt |> noCmd
    | Exn ex, _ -> cm |> addError ex.Message |> noCmd

    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (settings:Settings) (model : Model) =
    let isQuizzesActive = match model.Area with Quizzes _ -> true | _ -> false
    let isQuestionsActive = match model.Area with Questions _ -> true | _ -> false
    let isSettingsActive = match model.Area with Settings _ -> true | _ -> false
    let liClasses isActive =
        match isActive with
        | true -> classList ["has-background-light", true; "has-text-grey-dark", true]
        | false -> classList ["has-background-light", false; "has-text-grey-light", true]

    section [classList ["hero", true; "is-shadowless", true; "is-fullheight", true; "is-dark", true]] [
        div [Class "hero-head"] [
            div [Class "container"][
                nav [Class "navbar is-transparent is-spaced"; Role "navigation"; AriaLabel "dropdown navigation"] [
                    div [Class "navbar-brand"] [
                        a [Href "/app/"; Class "navbar-item navbar-logo"; Style [MarginRight "auto"]] [
                            figure [Class "image is-64x64"][
                                img [Src "/logo.png"; Alt "logo"; Style [Height "64px"; Width "64px"; MaxHeight "64px"]]
                            ]
                        ]

//                         let donateHtml = """
// <form action="https://www.paypal.com/donate" method="post" target="_top">
// <input type="hidden" name="hosted_button_id" value="MD3QKYNY98KHU" />
// <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" border="0" name="submit" title="Donate if you want to appreciate work open-quiz author did" alt="Donate" />
// <img alt="" border="0" src="https://www.paypal.com/en_UA/i/scr/pixel.gif" width="1" height="1" />
// </form>
// """
//                         div [Class "navbar-item"][
//                             div [Style[MarginTop "5px"]; DangerouslySetInnerHTML {__html = donateHtml}] []
//                             div [] [str "if you want to appreciate work open-quiz author did"]
//                         ]

                        a [Class "navbar-item is-paddingleft is-hidden-desktop"][str user.Name]

                        a [Role "button"; Class "navbar-burger has-text-light"; Style [MarginLeft "0"]; AriaLabel "menu"; AriaExpanded false; OnClick (fun _ -> dispatch ToggleBurger)][
                            span [AriaHidden true][]
                            span [AriaHidden true][]
                            span [AriaHidden true][]
                        ]
                    ]
                    div [classList ["navbar-menu", true; "is-active", model.IsBurgerOpen]] [
                        div [Class "navbar-start"] [
                            a [Class "navbar-item is-hidden-desktop"; OnClick (fun _ -> dispatch Logout)] [str "Logout"]
                        ]
                        div [Class "navbar-end"] [
                            div [Class "navbar-item has-dropdown is-hoverable is-hidden-touch"] [
                                a [Class "navbar-link"] [
                                    figure [Class "image"][
                                        img [Class "is-rounded"; Style [Height "50px"; Width "50px"; MaxHeight "50px"]; Src user.PictureUrl]
                                    ]
                                    span [Style [MarginLeft "5px"]][str user.Name]
                                ]
                                div [Class "navbar-dropdown is-boxed"][
                                    a [Class "navbar-item has-text-dark"; OnClick (fun _ -> dispatch Logout)] [str "Logout"]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        div [Class "hero-body"] [
            div [Class "container"; Style [MarginBottom "auto"]] [

                for error in model.Errors do
                    div [Class "notification is-danger"][
                        button [Class "delete"; OnClick (fun _ -> dispatch (DeleteError error.Key))][]
                        str error.Value
                    ]

                div [Class "columns"][
                    div [Class "column is-narrow"][
                        aside [Class "menu"][
                            p [Class "menu-label"][str "General"]
                            ul [Class "menu-list"][
                                li [][a [isQuizzesActive |> liClasses; OnClick (fun _ -> dispatch SwitchToQuizzes)][str "Quizzes"]]
                                li [][a [isQuestionsActive |> liClasses; OnClick (fun _ -> dispatch SwitchToQuestions)][str "Questions"]]
                                li [][a [isSettingsActive |> liClasses; OnClick (fun _ -> dispatch SwitchToSettings)][str "Settings"]]
                            ]
                        ]
                    ]
                    div [Class "column"][
                        match model.Area with
                        | Quizzes subModel -> MainProdQuizzes.view (Msg.Quizzes >> dispatch) user settings subModel
                        | Questions subModel -> MainProdQuestions.view (Msg.Questions >> dispatch) user settings subModel
                        | Settings subModel -> MainProdSettings.view (Msg.Settings >> dispatch) user settings subModel
                    ]
                ]
            ]
        ]

        MainTemplates.footerHero
    ]