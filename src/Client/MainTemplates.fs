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

let imgArea' style tag disabled changeMsg clearMsg mediaHost imgKey defaultImg clearText =
    [
        if not (System.String.IsNullOrWhiteSpace imgKey) then
            figure [classList ["image", true; style, true]; Style[MaxWidth "320px"]][ img [Shared.Infra.urlForMedia mediaHost imgKey |> Src]]
        else if not (System.String.IsNullOrWhiteSpace defaultImg) then
            figure [classList ["image", true; style, true]; Style[MaxWidth "320px"]][ img [defaultImg |> Src]]

        div [Class "file"; Style [MarginTop "8px"]][
            label [Class "file-label"][
                input [Class "file-input"; Type "file"; Name "picture"; Disabled disabled; OnChange(fileOnChangeS3 tag changeMsg)]
                span [Class "file-cta"][
                    span [Class "file-icon"][ Fa.i [Fa.Solid.Upload][]]
                    span [Class "file-label"][ str "Choose a file…"]
                ]
            ]
            button [Class "button"; Disabled disabled; Style [MarginLeft "5px"]; OnClick (fun _ -> clearMsg())] [str clearText]
        ]
    ]

let imgArea128 tag disabled changeMsg clearMsg mediaHost imgKey defaultImg clearText =
    imgArea' "is-128x128" tag disabled changeMsg clearMsg mediaHost imgKey defaultImg clearText

let imgEl mediaHost imgKey =
    seq {
        if not (System.String.IsNullOrWhiteSpace imgKey) then
            figure [Class "image"; Style[MaxWidth "320px"; Display DisplayOptions.InlineBlock]][
                img [Shared.Infra.urlForMedia mediaHost imgKey |> Src]
            ]
    }

let mediaArea tag disabled changeMsg clearMsg mediaHost (media:Shared.MediaDsc option) clearText =
    [
        match media with
        | Some media ->
            let url = Shared.Infra.urlForMedia mediaHost media.Key
            match media.Type with
            | Shared.Picture ->
                figure [Class "image"; Style[MaxWidth "320px"]][ img [Src url]]
            | Shared.Audio ->
                ReactPlayer.playerEx false false url
            | Shared.Video ->
                ReactPlayer.playerEx true false url
        | None -> ()

        div [Class "file"; Style [MarginTop "8px"]][
            label [Class "file-label"][
                input [Class "file-input"; Type "file"; Name "picture"; Disabled disabled; OnChange(fileOnChangeS3 tag changeMsg)]
                span [Class "file-cta"][
                    span [Class "file-icon"][ Fa.i [Fa.Solid.Upload][]]
                    span [Class "file-label"][ str "Choose a file…"]
                ]
            ]
            button [Class "button"; Disabled disabled; Style [MarginLeft "5px"]; OnClick (fun _ -> clearMsg())] [str clearText]
        ]
    ]

let mediaEl mediaHost (media:Shared.MediaDsc option) isPlaying =
    seq {
        match media with
        | Some media ->
            let url = Shared.Infra.urlForMedia mediaHost media.Key
            match media.Type with
            | Shared.Picture ->
                figure [Class "image"; Style[MaxWidth "320px"; Display DisplayOptions.InlineBlock]][ img [Src url]]
            | Shared.Audio ->
                ReactPlayer.playerEx false isPlaying url
            | Shared.Video ->
                ReactPlayer.playerEx true isPlaying url
        | None -> ()
    }


let footer =
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

let footerHero =
    div [Class "hero-foot has-background-dark has-text-grey-light"] [
        footer
    ]

let footerFixed =
    div [Class "has-background-dark has-text-grey-light"; Style [Position PositionOptions.Fixed; Bottom "0"; Height "22px"; Width "100%"; OverflowX OverflowOptions.Hidden]]  [
        footer
    ]

let playFooter dispatch history questions results current isCountdownActive secondsLeft (l10n:L10n.CommonL10n) =
        let isLastCall = isCountdownActive && secondsLeft <= 10
        div [Style [Position PositionOptions.Fixed; Bottom "0"; Height "62px"; Width "100%"; BackgroundColor "#FFFFFF"; OverflowX OverflowOptions.Hidden]]  [
            div [Class "tabs is-white is-large is-toggle is-fullwidth"] [
                ul[][
                    li [classList ["has-text-weight-bold", current = history] ] [
                        a [OnClick (fun _ -> dispatch history)] [ str l10n.MenuHistory ]
                    ]
                    li [classList ["has-text-weight-bold", current = questions]; Style [Height "100%"; Width "33%"]] [
                        a [Style [Height "100%"; JustifyContent (if isLastCall then "left" else "center"); Padding "0"]; OnClick (fun _ -> dispatch questions)] [
                            if isCountdownActive
                            then
                                if secondsLeft > 10 then str <| (secondsLeft - 10).ToString()
                                else
                                    div [Class "has-background-danger"; Style[Height "100%"; Width (sprintf "%i%%" ((10 - secondsLeft + 1) * 10))]][
                                        span [Class "is-overlay has-text-weight-bold"; Style [Padding "12px"]][str l10n.MenuLastCall]
                                    ]
                            else
                                str l10n.MenuQuestion
                        ]
                    ]
                    li [classList ["has-text-weight-bold", current = results ] ] [
                        a [OnClick (fun _ -> dispatch results)] [ str l10n.MenuResults ]
                    ]
                ]
            ]
        ]

let playSounds mediaHost secondsLeft =
    if secondsLeft = 11 then Infra.play (Shared.Infra.urlForMedia mediaHost "chgk2-sig2.mp3")
    if secondsLeft = 1 then Infra.play (Shared.Infra.urlForMedia mediaHost "chgk2-sig3.mp3")

let playTitle mediaHost quizImg mixlr url =
    div[Style [Margin "5px"]][
        match url with
        | Some url -> figure [Class "image is-16by9"][div[Class "has-ratio"][ReactPlayer.player url]]
        | None ->
            match mixlr with
            | Some mixlrUserId ->
                let src = sprintf "https://mixlr.com/users/%i/embed?autoplay=true" mixlrUserId
                iframe[Src src; Style[Width "100%"; Height "180px"]; Scrolling "no"; FrameBorder "no"; MarginHeight 0.0; MarginWidth 0.0][]
            | None ->
                br []
                figure [ Class "image is-128x128"; Style [Display DisplayOptions.InlineBlock] ] [ img [ Src <| Shared.Infra.urlForMediaImgSafe mediaHost quizImg ] ]
                br []
    ]

let playQuiz status msg (l10n:L10n.CommonL10n) =
    div [Class "notification is-white"][
        p [Class "subtitle is-5"][
            match status with
            | Shared.Setup -> str l10n.QuizIsNotStarted
            | Shared.Finished -> str l10n.QuizIsNotFinished
            | _ -> ()
        ]
        p [] (splitByLines msg)
     ]

let singleTourInfo mediaHost tourName (slip:Shared.SingleSlipCard) (l10n:L10n.CommonL10n) =
    div [] [
        h5 [Class "title is-5"] [ str <| l10n.Question +  " " + tourName]

        match slip with
        | Shared.X3 -> ()
        | Shared.QW slip ->
            if slip.Points <> 1m then
                p[][span [Class "tag"][str (sprintf "%s %s" (slip.Points.ToString()) l10n.PointsSmall)]]
            yield! mediaEl mediaHost slip.Media true
            p [ Class "has-text-weight-semibold" ] (splitByLines slip.Txt)
        | Shared.AW slip ->
            yield! mediaEl mediaHost slip.Media true
            match slip.Aw with
            | Shared.OpenAnswer txt ->
                p [ Class "has-text-weight-bold" ] [ str l10n.Answer ]
                p [ Class "has-text-weight-semibold" ] (splitByLines txt)
            | Shared.ChoiceAnswer _ -> ()
            if (slip.Com <> "") then
                p [ Class "has-text-weight-bold" ] [ str l10n.Comment ]
                p [ ] (splitByLines slip.Com)
        br[]
    ]


let resultsViewEmb' full quizId listenToken teamId mediaHost =
    div [Class "content"; Style []][
        let height = if full then "80vh" else "60vh"
        iframe [Src (Common.urlForResultsIframe quizId listenToken teamId mediaHost); Style [Width "100%"; Height height; OverflowX OverflowOptions.Visible; OverflowY OverflowOptions.Visible]][]
    ]

let resultsViewEmb = resultsViewEmb' false

let deleteForm dispatch placeholder validText inputText isSending error toggleMsg updateMsg deleteMsg =
    div[][
        div [Class "field has-addons"; Style[PaddingBottom "5px"]][
            p [Class "control is-expanded"][
                input [Class "input"; Disabled isSending; Type "text"; Placeholder placeholder; MaxLength 128.0;
                    valueOrDefault inputText;
                    OnChange (fun ev -> dispatch <| updateMsg ev.Value)]
            ]
            p [Class "control"][
                button [classList ["button", true; "has-text-danger", true; "has-text-weight-semibold", true; "is-loading", isSending];
                  Disabled (inputText <> validText);
                  OnClick (fun _ -> dispatch <| deleteMsg)][str "Delete"]
            ]
            p [Class "control"][
                button [classList ["button", true]; OnClick (fun _ -> dispatch toggleMsg)][str "Cancel"]
            ]
        ]
        p [Class "help is-danger"][str error]
    ]

let errors dispatch msg (errors : Map<string,string>) =
    div[][
        for error in errors do
            div [Class "notification is-danger is-light"][
                button [Class "delete"; OnClick (fun _ -> dispatch (msg error.Key))][]
                str error.Value
            ]
    ]