module rec Index

open Giraffe.GiraffeViewEngine
open Domain
open Shared

let layout (quizes : QuizDescriptor list) =
    html [] [
        head [] [
            meta [_charset "utf-8"]
            meta [_name "viewport"; _content "width=device-width, initial-scale=1" ]
            meta [_name "description"; _content "Free service for producing online quizzes."]
            meta [_name "keywords"; _content "quiz, quizzes, www, online quizzes, free quizzes, quizzes service, quizzes platform, квиз, чгк, что где когда, программа для чгк, онлайн-чгк, вопросы чгк, платформа для квизов, обработка ответов чгк"]
            meta [_name "author"; _content "Serhii Sabirov"]
            title [] [encodedText "Open Quiz"]
            link [_rel "stylesheet"; _href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css" ]
            link [_rel "icon"; _href "favicon.png"; _sizes "32x32"; _type "image/png" ]
        ]
        body [] [
            section [_class "hero is-shadowless is-fullheight"] [
                div [_class "hero-head"] [
                    div [_class "container"][
                        nav [_class "navbar is-transparent is-spaced"] [
                            div [_class "navbar-brand"] [
                                a [_href "/"; _class "navbar-item navbar-logo"; _style "margin-right: auto;"] [
                                    figure [_class "image is-64x64"][
                                        img [_src "/logo.png"; _alt "logo"; _style "height: 64px; width 64px; max-height: 64px"]
                                    ]
                                ]
                                a [_href "/login"; _class "navbar-item button thickbox is-uppercase is-hidden-desktop"; _style "margin: 5px"; _target "_self"] [rawText "Login"]
                            ]
                            div [_class "navbar-menu"] [
                                div [_class "navbar-end"] [
                                    div [_class "navbar-item is-hidden-touch"][
                                        a [_href "/login"; _class "button thickbox is-uppercase"; _target "_self"] [rawText "Login"]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                div [_class "hero-body"] [
                    div [_class "container"; _style "margin-bottom: auto"] [
                        yield h1 [_class "title"] [rawText "Welcome to the Open Quiz!"]
                        yield
                            h2 [_class "subtitle"] [
                                p[][
                                    rawText "Here you can compete in open quizzes as well as produce new quizzes. "
                                    rawText "The service is "
                                    strong [_class "has-text-danger"] [rawText "completely free"]
                                    rawText " for commercial and noncommercial use."
                                ]
                            ]

                        yield br[]

                        let featureQuizzes = quizes |> List.filter (fun q -> q.Status <> Domain.Finished) |> List.sortBy (fun q -> q.StartTime)
                        if not (List.isEmpty featureQuizzes) then
                            yield h3 [_class "title"] [rawText "Featured quizzes"]
                            for quizzes in featureQuizzes |> List.chunkBySize 3 do
                                yield quizzesRow quizzes

                        let finishedQuizzes = quizes |> List.filter (fun q -> q.Status = Domain.Finished) |> List.sortBy (fun q -> q.StartTime)
                        if not (List.isEmpty finishedQuizzes) then
                            yield h3 [_class "title"] [rawText "Finished quizzes"]
                            for quizzes in finishedQuizzes |> List.chunkBySize 3 do
                                yield quizzesRow quizzes
                    ]
                ]
                div [_class "hero-foot has-background-dark has-text-grey-light"] [
                    div [_class "container"; _style "text-align: center"][
                        str "\u00a9"
                        span[_class "is-hidden-touch"][str " Serhii Sabirov"]
                        str " 2020"
                        str " - "
                        a [_href "/terms.html"; _class "has-text-grey-light"] [str "Terms"]
                        str " - "
                        a [_href "/disclaimer.html"; _class "has-text-grey-light"] [str "Disclaimer"]
                        str " - "
                        a [_href "/privacy-policy-en.html"; _class "has-text-grey-light"] [str "Privacy"]
                        str " - "
                        a [_href "https://t.me/open_quiz"; _class "has-text-grey-light" ] [str "Contact"]
                    ]
                ]
            ]
        ]
        script [][rawText
"""
    var list = document.getElementsByClassName("convertToLocalTime");
    for (var i=0; i < list.length; i++){
        var utcDate = new Date(list[i].innerHTML + "Z");
        var month = new String(utcDate.getMonth() + 1)
        if (month.length == 1){
            month = '0' + month;
        }
        var hours = new String(utcDate.getHours())
        if (hours.length == 1){
            hours = '0' + hours;
        }
        var date = new String(utcDate.getDate())
        if (date.length == 1){
            date = '0' + date;
        }
        var minutes = new String(utcDate.getMinutes())
        if (minutes.length == 1){
            minutes = '0' + minutes;
        }

        list[i].innerHTML = utcDate.getFullYear() + "-" +  month + "-" + date + " " + hours + ":" + minutes
    }
"""
        ]
    ]

let quizzesRow quizzes =
    div [_class "columns"][
        for quiz in quizzes do
            div [_class "column is-one-third"][
                yield quizBox quiz
            ]
    ]

let quizBox quiz =
    div [_class "box is-shadowless"][
        article [_class "media"][
            div [_class "media-left"][
                figure [_class "image is-64x64"][
                    let url = if quiz.ImgKey <> "" then Infra.urlForImg quiz.ImgKey else Infra.defaultImg
                    img [_src url; _alt "Image"]
                ]
            ]
            div [_class "media-content"][
                div [_class "content"][
                    p[][
                        strong [][str quiz.Brand]
                        str "   "
                        match quiz.StartTime with
                            | Some dt -> span [_class "convertToLocalTime"][ str (dt.ToString("yyyy-MM-dd HH:mm"))]
                            | None -> str "???"
                        if quiz.Status = Domain.Live then
                            str " "
                            span [_class "tag is-danger is-light"][str "live"]
                        br[]
                        strong [][str quiz.Name]
                        br[]
                        small [][
                            for l in (Quizzes.getDescription quiz).Split ("\n") do
                                str l
                                br[]
                        ]
                    ]
                ]
                if (quiz.Status <> Domain.Finished) then
                    nav [_class "level is-mobile"][
                        div [_class "level-left"][
                        ]
                        div [_class "level-right"][
                            p [_class "level-item is-light is-italic"][
                                small [][str "please login to register"]
                            ]
                        ]
                    ]
            ]

        ]
    ]
