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

let playFooter dispatch history questions results current isCountdownActive secondsLeft =
        div [Style [Position PositionOptions.Fixed; Bottom "0"; Height "62px"; Width "100%"; BackgroundColor "#FFFFFF"; OverflowX OverflowOptions.Hidden]]  [
            div [Class "tabs is-white is-large is-toggle is-fullwidth"] [
                ul[][
                    li [classList ["has-text-weight-bold", current = history] ] [
                        a [OnClick (fun _ -> dispatch history)] [ str "History" ]
                    ]
                    li [classList ["has-text-weight-bold", current = questions; "has-background-danger", isCountdownActive && secondsLeft < 10]] [
                        a [OnClick (fun _ -> dispatch questions)] [
                            if isCountdownActive then str (secondsLeft.ToString()) else str "Question"
                        ]
                    ]
                    li [classList ["has-text-weight-bold", current = results ] ] [
                        a [OnClick (fun _ -> dispatch results)] [ str "Results" ]
                    ]
                ]
            ]
       ]
let playTitle quizName quizImg isConnectionOk =
    div [][
        if isConnectionOk then
            Fa.i [Fa.Solid.Wifi; Fa.Props [Style[Position PositionOptions.Absolute; Top "5px"; Left "5px"]]][str " connected"]
        else
            span [Class "has-text-danger"][Fa.i [Fa.Solid.Wifi; Fa.Props [Style[Position PositionOptions.Absolute; Top "5px"; Left "5px"]]][ str " disconnected"]]

        br []
        figure [ Class "image is-128x128"; Style [Display DisplayOptions.InlineBlock] ] [ img [ Src <| Shared.Infra.urlForImgSafe quizImg ] ]
        br []
        h3 [Class "title is-3"] [ str quizName ]
    ]

let playQuiz status msg =
    div [Class "notification is-white"][
        p [Class "subtitle is-5"][
            match status with
            | Shared.Draft | Shared.Published -> str "Coming soon ..."
            | Shared.Finished | Shared.Archived -> str "Finished"
            | _ -> ()
        ]
        p [] (splitByLines msg)
     ]

let playQuestion (quizQuestion:Shared.QuestionCard option) =
    div [] [
        h5 [Class "title is-5"] [ str <| "Question: " + match quizQuestion with Some qw -> qw.Cap | None -> "???" ]

        match quizQuestion with
        | Some q ->
            yield! imgEl q.Img
            if q.QQS = Shared.Settled then
                p [ Class "has-text-weight-bold" ] [ str "Answer" ]
            p [ Class "has-text-weight-semibold" ] (splitByLines q.Txt)
            if (q.Com <> "") then
                p [ Class "has-text-weight-bold" ] [ str "Comment" ]
                p [ ] (splitByLines q.Com)
            br[]
        | _ -> ()
    ]

let resultsRow (res:Shared.TeamResult) style =
    tr [Style style] [
        td [ ][ str <| res.TeamId.ToString()]
        td [ ][ str res.TeamName]
        td [ ][ str <| res.Points.ToString()]
        td [] [
            if res.PlaceFrom = res.PlaceTo
            then res.PlaceFrom.ToString()
            else sprintf "%i-%i" res.PlaceFrom res.PlaceTo
            |> str
        ]
    ]

let resultsView (currentRes:Shared.TeamResult option) (teamResults:Shared.TeamResult list) =
    table [Class "table is-hoverable is-fullwidth"] [
        thead [ ] [
            yield tr [ ] [
                th [ ] [ str "#" ]
                th [ ] [ str "Team" ]
                th [ ] [ str "Points" ]
                th [ ] [ str "Place" ]
            ]

            match currentRes with
            | Some res ->
                 yield resultsRow res [FontWeight "bold"]
                 yield tr[][td[][];td[][];td[][];td[][]]
            | None -> ()
        ]
        tbody [] [
            for res in teamResults |> List.sortByDescending (fun r -> r.Points, -r.TeamId) do
                let style =
                    match currentRes with
                    | Some r when r.TeamId = res.TeamId -> [FontWeight "bold"]
                    | _ -> []

                yield resultsRow res style
        ]
    ]

