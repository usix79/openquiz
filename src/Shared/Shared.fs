namespace rec Shared

module Errors =
    let SessionIsNotActive = "SESSION IS NOT ACTIVE"

type REQ<'T> = {
    Token : string
    Arg: 'T
}

type ServerResponseStatus =
    | Executed
    | SecurityTokenInvalid
    | SecurityTokenExpired
    | Unauthorized

type RESP<'T> = {
    Status : ServerResponseStatus
    Value :  Result<'T, string>
    ST : System.DateTime
}

type ARESP<'T> = Async<RESP<'T>>

type TRESP<'T, 'P> = {
    Tag : 'T
    Rsp : RESP<'P>
}

type LoginReq =
    | MainUser of {|Code: string|}
    | AdminUser of {|QuizId:int; Token: string|}
    | TeamUser of {|QuizId: int; TeamId: int; Token: string|}
    | RegUser of {|QuizId:int; Token: string|}
    | AudUser of {|QuizId:int; Token: string|}

type User =
    | MainUser of MainUser
    | AdminUser of AdminUser
    | TeamUser of TeamUser
    | RegUser of RegUser
    | AudUser of AudUser

type MainUser = {
    Sub : string
    Username : string
    Name : string
    PictureUrl : string
    IsProducer : bool
    IsPrivate : bool
}

type TeamUser = {
    QuizId : int
    QuizName : string
    TeamId : int
    TeamName : string
}

type AdminUser = {
    QuizId : int
    QuizName : string
    QuizImg : string
}

type AudUser = {
    QuizId : int
}

type RegUser = {
    QuizId : int
}

type ImgCategory =
    | Quiz
    | Question
with
    member x.Prefix =
        match x with
        | Quiz -> "qz"
        | Question -> "qw"

type QuizStatus =
    | Draft
    | Published
    | Live
    | Finished
    | Archived

type TeamStatus =
    | New
    | Admitted
    | Rejected

type TourStatus =
    | Announcing
    | Countdown
    | Settled

type SingleAwSlipCard = {
    Txt : string
    Img : string
    Com : string
}

type SlipCard =
    | SingleSlipCard of SingleAwSlipCard

type TourCard = {
    Idx : int
    Cap : string
    Sec : int
    TS : TourStatus
    Slip : SlipCard
    ST : System.DateTime option
} with
    member x.SecondsLeft now =
        match x.ST with
        | Some st ->
            match  x.Sec - int((now - st).TotalSeconds) with
            | seconds when seconds > 0 -> seconds
            | _ -> 0
        | _ -> x.Sec

    member x.IsCountdownActive now =
        match x.ST with
        | Some _ when x.TS = Countdown -> x.SecondsLeft now > 0
        | _ -> false

type QuizChangedEvent = {
    Id : int
    QS : QuizStatus
    T : TourCard option
}

type PackageRecord = {
    PackageId : int
    Name : string
}

type PackageCard = {
    PackageId : int
    Name : string
    TransferToken : string
    Slips : Slip list
}
 with
    member x.GetSlip idx =
        if idx >= 0 && idx <x.Slips.Length then
            Some (x.Slips.Item(idx))
        else None
    member x.UpdateSlip idx slip =
        {x with
            Slips = x.Slips |> List.mapi (fun i q -> if idx = i then slip else q)
        }
    member x.AddSingleSlip qwCount =
        {x with
            Slips = x.Slips @ [Single{Questions= (List.init qwCount (fun i -> ""));ImgKey="";Answer="";Comment="";CommentImgKey=""}]
        }
    member x.DelSlip idx =
        {x with
            Slips = x.Slips
                |> List.indexed
                |> List.choose (fun (i, s) -> if idx = i then None else Some s)
        }

type Slip =
    | Single of SingleAwSlip
with
    member x.Text =
        match x with
        | Single slip -> slip.Questions |> String.concat "\n"
    member x.LastQwIdx =
        match x with
        | Single slip -> (slip.Questions |> List.length) - 1

type SingleAwSlip = {
    Questions : string list
    ImgKey : string
    Answer : string
    Comment : string
    CommentImgKey : string
} with
    member x.SetQwText idx txt =
        {x with Questions = x.Questions |> List.mapi (fun i qw -> if i = idx then txt else qw)}
    member x.GetQwText idx =
        x.Questions |> List.tryItem idx |> Option.defaultValue ""

type TeamResult = {
    TeamId : int
    TeamName : string
    Points : decimal
    PlaceFrom : int
    PlaceTo : int
    History : Map<int, decimal>
}

type QuestionResult = {
    Idx : int
    Name : string
}

module RegModels =
    type QuizRecord = {
        QuizId : int
        StartTime : System.DateTime option
        Brand : string
        Name : string
        Status : QuizStatus
        Description : string
        ImgKey : string
        EventPage : string
    }

module MainModels =

    type ExpertCompetition = {
        QuizId : int
        TeamId : int
        TeamName : string
        TeamStatus : TeamStatus
        EntryToken : string
    }

    type ExpertProfile = {
        Competitions : Map<int, ExpertCompetition>
    }
    with
        member x.UpdateCompetition (comp:ExpertCompetition) =
            {x with Competitions = x.Competitions.Add(comp.QuizId, comp)}

    type QuizPubRecord = {
        QuizId : int
        StartTime : System.DateTime option
        Brand : string
        Name : string
        Status : QuizStatus
        Description : string
        ImgKey : string
        EventPage : string
    }

    type QuizProdRecord = {
        QuizId : int
        StartTime : System.DateTime option
        Brand : string
        Name : string
        Status : QuizStatus
        AdminToken : string
    }

    type QuizProdCard = {
        QuizId : int
        StartTime : System.DateTime option
        Brand : string
        Name : string
        Status : QuizStatus
        ImgKey : string
        ListenToken : string
        AdminToken : string
        RegToken : string
        WelcomeText : string
        FarewellText : string
        IsPrivate :  bool
        WithPremoderation : bool
        EventPage : string
        MixlrCode : int option
    }

module AdminModels =

    type TeamRecord = {
        TeamId : int
        TeamName : string
        TeamStatus : TeamStatus
        EntryToken : string
    }

    type TeamCard = {
        TeamId : int
        TeamName : string
        TeamStatus : TeamStatus
        EntryToken : string
        RegistrationDate : System.DateTime
    }

    type TourControlCard = {
        Name : string
        Seconds : int
        Status : TourStatus
        NextQwIdx : int
        Slip : Slip
        StartTime : System.DateTime option
    }
    with
        member x.SecondsLeft now =
            match x.StartTime with
            | Some st ->
                match  x.Seconds - int((now - st).TotalSeconds) with
                | seconds when seconds > 0 -> seconds
                | _ -> 0
            | _ -> x.Seconds

        member x.IsCoundownActive now =
            x.Status = Countdown && x.SecondsLeft now > 0
        member x.IsLastQuestion =
            x.NextQwIdx >= x.Slip.LastQwIdx


    type QuizControlCard = {
        QuizStatus : QuizStatus
        PackageId : int option
        PackageSlipIdx : int option
        CurrentTour : TourControlCard option
    }

    type Answer = {
        Txt : string
        RT : System.DateTime
        Res : decimal option
        IsA : bool
        UT : System.DateTime option
    }

    type QuestionRecord = {
        Idx : int
        Nm : string
        Sec : int
        QQS : TourStatus
        ST : System.DateTime option
    }

    type TeamAnswersRecord = {
        Id : int
        Nm : string
        Awrs : Map<int,Answer>
    } with
        member x.GetAw idx =
            match x.Awrs.TryGetValue idx with
            | true, aw -> Some aw
            | _ -> None
        member x.UpdateAwr idx aw =
            { x with Awrs = x.Awrs.Add(idx, aw)}

    type AnswersBundle = {
        Questions : QuestionRecord list
        Teams : TeamAnswersRecord list
    } with
        member x.GetAw teamId idx =
            match x.Teams |> List.tryFind (fun t -> t.Id = teamId) with
            | Some team -> team.GetAw idx
            | None -> None
        member x.FindAnswers idx txt =
            x.Teams
            |> List.map (fun t -> t.Id, (t.GetAw idx))
            |> List.filter (fun (_,a) -> a.IsSome && a.Value.Txt = txt)
            |> List.map (fun (teamId,a) -> teamId,a.Value)
        member x.UpdateAnswers idx (answersToUpdate : Map<int,Answer>) =
            { x with
                Teams = x.Teams |> List.map ( fun team ->
                    match answersToUpdate.TryGetValue team.Id with
                    | true, aw -> team.UpdateAwr idx aw
                    | _ -> team
                )
            }

module TeamModels =
    type QuizCard = {
        QS : QuizStatus
        TS : TeamStatus
        Img : string
        Wcm : string
        Fwl : string
        TC : TourCard option
        Aw : string option
        LT : string
        Mxlr : int option
        V : int
    } with
        member x.Msg =
            match x.QS with
            | Draft | Published | Live -> x.Wcm
            | Finished | Archived -> x.Fwl

    type TeamHistoryRecord = {
        QwIdx : int
        QwName : string
        QwAw : string
        AwTxt : string option
        Result : decimal option
    }

module AudModels =
    type QuizCard = {
        QS : QuizStatus
        QN : string
        Img : string
        Wcm : string
        Fwl : string
        TC : TourCard option
        LT : string
        Mxlr : int option
        V : int
    } with
        member x.Msg =
            match x.QS with
            | Draft | Published | Live -> x.Wcm
            | Finished | Archived -> x.Fwl

    type HistoryRecord = {
        QwIdx : int
        QwName : string
        QwAw : string
    }

module Infra =
    let routeBuilder clientPath (typeName: string) (methodName: string) =
        sprintf "%s/app/api/%s/%s" clientPath typeName methodName

    let sseUrl quizId lastQuizVersion listenToken =
        sprintf "/sse?quiz=%i&start=%i&token=%s" quizId lastQuizVersion listenToken

    let urlForImg key =
        sprintf "/img/%s" key

    let urlForImgSafe key =
        if key <> "" then sprintf "/img/%s" key else defaultImg

    let defaultImg = "/logo256.png"

type ISecurityApi = {
    login : REQ<LoginReq> -> ARESP<{|Token: string; RefreshToken: string; User: User|}>
    refreshToken : REQ<{|RefreshToken: string|}> -> ARESP<{|Token: string; RefreshToken: string|}>
}

type IMainApi = {
    becomeProducer : REQ<unit> -> ARESP<unit>
    createQuiz : REQ<unit> -> ARESP<{|Record : MainModels.QuizProdRecord; Card:MainModels.QuizProdCard|}>
    getProdQuizzes : REQ<unit> -> ARESP<MainModels.QuizProdRecord list>
    getProdQuizCard : REQ<{|QuizId:int|}> -> ARESP<MainModels.QuizProdCard>
    updateProdQuizCard : REQ<MainModels.QuizProdCard> -> ARESP<MainModels.QuizProdRecord>
    deleteQuiz : REQ<{|QuizId : int|}> -> ARESP<unit>
    uploadFile : REQ<{|Cat:ImgCategory; FileType : string; FileBody : byte[]|}> -> ARESP<{|BucketKey: string|}>
    getPubModel : REQ<unit> -> ARESP<{|Profile : MainModels.ExpertProfile; Quizzes : MainModels.QuizPubRecord list|}>
    registerTeam : REQ<{|QuizId: int; TeamName: string|}> -> ARESP<MainModels.ExpertCompetition>
    getProdPackages : REQ<unit> -> ARESP<PackageRecord list>
    getProdPackageCard : REQ<{|PackageId : int|}> -> ARESP<PackageCard>
    createPackage : REQ<unit> -> ARESP<{|Record : PackageRecord; Card: PackageCard|}>
    updateProdPackageCard : REQ<PackageCard> -> ARESP<PackageRecord>
    aquirePackage : REQ<{|PackageId:int; TransferToken:string|}> -> ARESP<PackageRecord>
    deletePackage: REQ<{|PackageId : int|}> -> ARESP<unit>
}

type IAdminApi = {
    getTeams : REQ<unit> -> ARESP<AdminModels.TeamRecord list>
    createTeam : REQ<{|TeamName : string|}> -> ARESP<{|Record : AdminModels.TeamRecord|}>
    getTeamCard : REQ<{|TeamId : int|}> -> ARESP<AdminModels.TeamCard>
    updateTeamCard : REQ<AdminModels.TeamCard> -> ARESP<AdminModels.TeamRecord>
    changeTeamStatus : REQ<{|TeamId : int; TeamStatus : TeamStatus|}> -> ARESP<AdminModels.TeamRecord>
    getQuizCard : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    changeQuizStatus : REQ<{|QuizStatus : QuizStatus|}> -> ARESP<AdminModels.QuizControlCard>
    getPackages : REQ<unit> -> ARESP<PackageRecord list>
    setPackage : REQ<{|PackageId: int option|}> -> ARESP<AdminModels.QuizControlCard>
    getPackageCard : REQ<{|PackageId: int|}> -> ARESP<PackageCard option>
    uploadFile : REQ<{|Cat:ImgCategory; FileType : string; FileBody : byte[]|}> -> ARESP<{|BucketKey: string|}>
    startCountDown : REQ<AdminModels.QuizControlCard> -> ARESP<AdminModels.QuizControlCard>
    pauseCountDown : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    settleTour : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    nextTour : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    nextQuestion : REQ<AdminModels.QuizControlCard> -> ARESP<AdminModels.QuizControlCard>
    getAnswers : REQ<unit> -> ARESP<AdminModels.AnswersBundle>
    updateResults : REQ<{|TeamId: int; Idx: int; Res: decimal option |} list> -> ARESP<unit>
    getResults : REQ<unit> -> ARESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
}

type ITeamApi = {
    getState : REQ<unit> -> ARESP<TeamModels.QuizCard>
    takeActiveSession : REQ<unit> -> ARESP<TeamModels.QuizCard>
    answer : REQ<{|QwIndex:int; Answer:string|}> -> ARESP<unit>
    getHistory : REQ<unit> -> ARESP<TeamModels.TeamHistoryRecord list>
    getResults : REQ<unit> -> ARESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
}

type IRegApi = {
    getRecord : REQ<unit> -> ARESP<RegModels.QuizRecord>
}

type IAudApi = {
    getQuiz : REQ<unit> -> ARESP<AudModels.QuizCard>
    getHistory : REQ<unit> -> ARESP<AudModels.HistoryRecord list>
    getResults : REQ<unit> -> ARESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
}
