module MainProd

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fulma

open Shared
open Common

type Msg =
    | Quizzes of MainProdQuizzes.Msg
    | Questions of MainProdQuestions.Msg
    | SwitchToQuizzes
    | SwitchToQuestions

type Model =
    | Quizzes of MainProdQuizzes.Model
    | Questions of MainProdQuestions.Model

let init (api:IMainApi) user : Model*Cmd<Msg> =
    let subModel,subCmd = MainProdQuizzes.init api user
    Quizzes subModel, Cmd.map Msg.Quizzes subCmd

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg, cm with
    | SwitchToQuizzes, _ ->
        let subModel,subCmd = MainProdQuizzes.init api user
        Quizzes subModel, Cmd.map Msg.Quizzes subCmd
    | SwitchToQuestions, _ ->
        let subModel,subCmd = MainProdQuestions.init api user
        Questions subModel, Cmd.map Msg.Questions subCmd
    | Msg.Quizzes subMsg, Quizzes subModel ->
        let subModel,subCmd = MainProdQuizzes.update api user subMsg subModel
        Quizzes subModel, Cmd.map Msg.Quizzes subCmd
    | Msg.Questions subMsg, Questions subModel ->
        let subModel,subCmd = MainProdQuestions.update api user subMsg subModel
        Questions subModel, Cmd.map Msg.Questions subCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =
    let isQuizzesActive = match model with Quizzes _ -> true | _ -> false
    let isQuestionsActive = match model with Questions _ -> true | _ -> false
    let liClasses isActive =
        match isActive with
        | true -> classList ["has-background-light", true; "has-text-grey-dark", true]
        | false -> classList ["has-background-light", false; "has-text-grey-light", true]

    div [Class "columns"][
        div [Class "column is-narrow"][
            aside [Class "menu"][
                p [Class "menu-label"][str "General"]
                ul [Class "menu-list"][
                    li [][a [isQuizzesActive |> liClasses; OnClick (fun _ -> dispatch SwitchToQuizzes)][str "Quizzes"]]
                    li [][a [isQuestionsActive |> liClasses; OnClick (fun _ -> dispatch SwitchToQuestions)][str "Questions"]]
                ]
            ]
        ]
        div [Class "column"][
            match model with
            | Quizzes subModel -> MainProdQuizzes.view (Msg.Quizzes >> dispatch) user subModel
            | Questions subModel -> MainProdQuestions.view (Msg.Questions >> dispatch) user subModel
        ]
    ]