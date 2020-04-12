module rec Main

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React

open Shared
open Common


type Msg =
    | ToggleBurger
    | Logout
    | Public of MainPub.Msg
    | Prod of MainProd.Msg
    | SwithToPublic
    | SwithToProd

type Model = {
    IsBurgerOpen : bool
    IsTermsOfUseOpen : bool
    Errors : Map<string, string>
    Area : Area
}

type Area =
    | Public of MainPub.Model
    | Prod of MainProd.Model

let init api user : Model*Cmd<Msg> =
    let subModel,subCmd = MainPub.init api user
    {IsBurgerOpen = false; IsTermsOfUseOpen = false; Errors = Map.empty; Area = Public subModel}, Cmd.map Msg.Public subCmd

let update (api:IMainApi) user (msg : Msg) (cm : Model) : Model * Cmd<Msg> =
    match msg, cm.Area with
    | ToggleBurger, _ -> {cm with IsBurgerOpen = not cm.IsBurgerOpen} |> noCmd
    | SwithToProd, _ ->
        let subModel,subCmd = MainProd.init api user
        {cm with Area = Prod subModel}, Cmd.map Msg.Prod subCmd
    | SwithToPublic, _ ->
        let subModel,subCmd = MainPub.init api user
        {cm with Area = Public subModel}, Cmd.map Msg.Public subCmd
    | Logout, _ ->  Infra.clearUserAndRedirect "/"; cm|> noCmd
    | Msg.Public subMsg, Public subModel ->
        let subModel,subCmd = MainPub.update api user subMsg subModel
        {cm with Area = Public subModel}, Cmd.map Msg.Public subCmd
    | Msg.Prod subMsg, Prod subModel ->
        let subModel,subCmd = MainProd.update api user subMsg subModel
        {cm with Area = Prod subModel}, Cmd.map Msg.Prod subCmd
    | _ -> cm |> noCmd

let view (dispatch : Msg -> unit) (user:MainUser) (model : Model) =

    let isProd = match model.Area with Public _  -> false | Prod _ -> true

    section [classList ["hero", true; "is-shadowless", true; "is-fullheight", true; "is-dark", isProd]] [
        div [Class "hero-head"] [
            div [Class "container"][
                nav [Class "navbar is-transparent is-spaced"; Role "navigation"; AriaLabel "dropdown navigation"] [
                    div [Class "navbar-brand"] [
                        a [Href "/app/"; Class "navbar-item navbar-logo"; Style [MarginRight "auto"]] [
                            figure [Class "image is-64x64"][
                                img [Src "/logo.png"; Alt "logo"; Style [Height "64px"; Width "64px"; MaxHeight "64px"]]
                            ]
                        ]

                        match user.IsProducer, model.Area with
                        | true, Public _ -> a [Class "navbar-item"; OnClick (fun _ -> dispatch SwithToProd)][str "to production area >>"]
                        | true, Prod _ -> a [Class "navbar-item"; OnClick (fun _ -> dispatch SwithToPublic)][str "<< to public area"]
                        | _ -> ()

                        a [Class "navbar-item is-paddingleft is-hidden-desktop"][str user.Name]

                        a [Role "button"; Class "navbar-burger"; Style [MarginLeft "0"]; AriaLabel "menu"; AriaExpanded false; OnClick (fun _ -> dispatch ToggleBurger)][
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
                                    a [Class "navbar-item"; OnClick (fun _ -> dispatch Logout)] [str "Logout"]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        div [Class "hero-body"] [
            div [Class "container"; Style [MarginBottom "auto"]] [
                match model.Area with
                | Public subModel -> MainPub.view (Msg.Public >> dispatch) user subModel
                | Prod subModel -> MainProd.view (Msg.Prod >> dispatch) user subModel
            ]
        ]
        div [Class "hero-foot has-background-dark has-text-grey-light"] [
            div [Class "container"; Style [TextAlign TextAlignOptions.Center]][
                str "\u00a9"
                span[Class "is-hidden-touch"][str " Serhii Sabirov"]
                str " 2020"
                str " - "
                a [Href "/terms.html"; Class "has-text-grey-light"] [str "Terms"]
                str " - "
                a [Href "/disclaimer.html"; Class "has-text-grey-light"] [str "Disclaimer"]
                str " - "
                a [Href "/privacy-policy-en.html"; Class "has-text-grey-light"] [str "Privacy"]
                str " - "
                a [Href "https://t.me/open_quiz"; Class "has-text-grey-light" ] [str "Contact"]
            ]
        ]
    ]