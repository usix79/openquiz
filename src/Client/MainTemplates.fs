module MainTemplates

open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.FontAwesome

open Common

let termsOfUse dispatch acceptMsg cancelMsg =
    div [Class "modal is-active"][
        div [Class "modal-background"][]
        div [Class "modal-card"; Style [Height "70%"; Width "80%"]][
            header [Class "modal-card-head"][
                p [Class "modal-card-title"] [str "Welcome to Open Quiz production area!"]
                button [Class "delete"; AriaLabel "close"; OnClick (fun _ ->dispatch cancelMsg)][]
            ]
            section [Class "modal-card-body"][
                iframe [Src "/terms.html"; Style [Width "100%"; Height "100%"]][]

            ]
            footer [Class "modal-card-foot"][
                button [Class "button is-black has-text-danger";OnClick (fun _ ->dispatch acceptMsg)][str"Accept"]
                button [Class "button";OnClick (fun _ ->dispatch cancelMsg)][str"Cancel"]
            ]
        ]
    ]

let inputModal dispatch title txt changeMsg okMsg cancelMsg =
    div [Class "modal is-active"][
        div [Class "modal-background"][]
        div [Class "modal-card"][
            header [Class "modal-card-head"][
                p [Class "modal-card-title"] [str title]
            ]
            section [Class "modal-card-body"][
                input [Class "input"; AutoFocus true; valueOrDefault txt;  OnChange (fun ev -> changeMsg ev.Value |> dispatch ) ]
            ]
            footer [Class "modal-card-foot"][
                button [Class "button"; Disabled (txt = "");  OnClick (fun _ ->dispatch okMsg)][str "Ok"]
                button [Class "button"; OnClick (fun _ ->dispatch cancelMsg)][str"Cancel"]
            ]
        ]
    ]

let imgArea tag disabled changeMsg clearMsg imgKey defaultImg clearText =
    [
        if not (System.String.IsNullOrWhiteSpace imgKey) then
            figure [Class "image"; Style[MaxWidth "320px"]][ img [Shared.Infra.urlForImg imgKey |> Src]]
        else if not (System.String.IsNullOrWhiteSpace defaultImg) then
            figure [Class "image"; Style[MaxWidth "320px"]][ img [defaultImg |> Src]]

        div [Class "file"; Style [MarginTop "8px"]][
            label [Class "file-label"][
                input [Class "file-input"; Type "file"; Name "picture"; Disabled disabled; OnChange(fileOnChange tag changeMsg)]
                span [Class "file-cta"][
                    span [Class "file-icon"][ Fa.i [Fa.Solid.Upload][]]
                    span [Class "file-label"][ str "Choose a file…"]
                ]
            ]
            button [Class "button"; Disabled disabled; Style [MarginLeft "5px"]; OnClick (fun _ -> clearMsg())] [str clearText]
        ]
    ]

let imgEl imgKey =
    seq {
        if not (System.String.IsNullOrWhiteSpace imgKey) then
            figure [Class "image"; Style[MaxWidth "320px"; Display DisplayOptions.InlineBlock]][ img [Shared.Infra.urlForImg imgKey |> Src]]
    }

let footer =
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
