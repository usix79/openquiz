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
        div [Class "modal-card"][
            header [Class "modal-card-head"][
                p [Class "modal-card-title"] [str "Welcome to Open Quiz production area!"]
                button [Class "delete"; AriaLabel "close"; OnClick (fun _ ->dispatch cancelMsg)][]
            ]
            section [Class "modal-card-body"][
                iframe [Src "/terms.html"; Style [Width "100%"]][]

            ]
            footer [Class "modal-card-foot"][
                button [Class "button is-black has-text-danger";OnClick (fun _ ->dispatch acceptMsg)][str"Accept"]
                button [Class "button";OnClick (fun _ ->dispatch cancelMsg)][str"Cancel"]
            ]
        ]
    ]

let imgArea tag disabled changeMsg clearMsg imgKey defaultImg clearText =
    [
        if not (System.String.IsNullOrWhiteSpace imgKey) then
            figure [Class "image is-128x128"; Style[MaxWidth "320px"]][ img [Shared.Infra.urlForImg imgKey |> Src]]
        else if not (System.String.IsNullOrWhiteSpace defaultImg) then
            figure [Class "image is-128x128"; Style[MaxWidth "320px"]][ img [defaultImg |> Src]]

        div [Class "file"; Style [MarginTop "8px"]][
            label [Class "file-label"][
                input [Class "file-input"; Type "file"; Name "picture"; OnChange(fileOnChange tag changeMsg)]
                span [Class "file-cta"][
                    span [Class "file-icon"][ Fa.i [Fa.Solid.Upload][]]
                    span [Class "file-label"][ str "Choose a file…"]
                ]
            ]
            button [Class "button"; Disabled disabled; Style [MarginLeft "5px"]; OnClick (fun _ -> clearMsg())] [str clearText]
        ]
    ]
