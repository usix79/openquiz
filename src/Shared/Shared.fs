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

type AppSyncConfig = {
    Endpoint: string
    Region: string
    ApiKey : string
}
type TeamUser = {
    QuizId : int
    QuizName : string
    TeamId : int
    TeamName : string
    AppSyncCfg : AppSyncConfig
}

type AdminUser = {
    QuizId : int
    QuizName : string
    QuizImg : string
}

type AudUser = {
    QuizId : int
    AppSyncCfg : AppSyncConfig
}

type RegUser = {
    QuizId : int
}

type Settings = {
    MediaHost : string
}

type MediaCategory =
    | QuizImg
    | QuestionImg
    | QuestionVideo
with
    member x.Prefix =
        match x with
        | QuizImg -> "qz"
        | QuestionImg -> "qw"
        | QuestionVideo -> "qv"

type QuizStatus =
    | Setup
    | Live
    | Finished

type TeamStatus =
    | New
    | Admitted
    | Rejected

type TourStatus =
    | Announcing
    | Countdown
    | Settled

type SlipQwCard = {
    Txt : string
    Img : string
    Ch : bool
}

type SlipAwCard = {
    Txt : string
    Img : string
    Com : string
    Ch : bool
}
type SingleSlipCard =
    | X3
    | QW of SlipQwCard
    | AW of SlipAwCard

type QwKey  = {
    TourIdx : int
    QwIdx : int
}

type SlipCard =
    | SS of SingleSlipCard
    | MS of name:string * slips:SingleSlipCard list
    with
        member x.QwCount =
            match x with
            | SS _ -> 1
            | MS (_,slips) -> slips.Length

type TourCard = {
    Idx : int
    Name : string
    Sec : int
    TS : TourStatus
    Slip : SlipCard
    ST : System.DateTime option
} with
    member x.SecondsLeft now =
        match x.ST with
        | Some st ->
            match  (x.Sec + 10) - int((now - st).TotalSeconds) with
            | seconds when seconds > 0 -> seconds
            | _ -> 0
        | _ -> x.Sec

    member x.IsCountdownActive now =
        match x.ST with
        | Some _ when x.TS = Countdown -> x.SecondsLeft now > 0
        | _ -> false

    member x.IsCountdownFinished now =
        match x.ST with
        | Some _ when x.TS = Countdown -> x.SecondsLeft now = 0
        | _ when x.TS = Settled -> true
        | _ -> false

type QuizChangedEvent = {
    Id : int
    QS : QuizStatus
    Url : string option
    T : TourCard option
}

type Slip =
    | Single of SingleAwSlip
    | Multiple of string * SingleAwSlip list
with
    member x.Annotation =
        match x with
        | Single slip -> slip.GetQwText 0
        | Multiple (name,_) -> name
    member x.LastQwIdx =
        match x with
        | Single slip -> 0
        | Multiple (_,slips) -> slips.Length - 1
    member x.LastQwPartIdx qwIdx=
        match x with
        | Single slip -> slip.LastPartIdx
        | Multiple (_,slips) -> slips |> List.tryItem qwIdx |> Option.map (fun slip -> slip.LastPartIdx) |> Option.defaultValue 0
    member x.SetMultipleName name =
        match x with
        | Single _ -> x
        | Multiple (_,slips) -> (name,slips) |> Multiple
    member x.AppendToMultiple slip =
        match x with
        | Single _ -> x
        | Multiple (name,slips) -> (name,slips @ [slip]) |> Multiple
    member x.RemoveFromMultiple idx =
        match x with
        | Single _ -> x
        | Multiple (name,slips) ->
            let slips = slips |> List.indexed |> List.choose (fun (i,s) -> if idx <> i then Some s else None)
            (name, slips) |> Multiple


type QuestionText =
    | Solid of string
    | Split of string list

type SingleAwSlip = {
    Question : QuestionText
    ImgKey : string
    Answer : string
    Comment : string
    CommentImgKey : string
    Points : decimal
    JeopardyPoints : decimal option
    WithChoice : bool
} with
    member x.SetQwText idx txt =
        {x with
            Question =
                match x.Question with
                | Solid _ -> Solid txt
                | Split list -> list |> List.mapi (fun i qw -> if i = idx then txt else qw) |> Split
         }
    member x.GetQwText idx =
        match x.Question with
        | Solid qw -> qw
        | Split list -> list |> List.tryItem idx |> Option.defaultValue ""
    member x.LastPartIdx =
        match x.Question with
        | Solid _ -> 0
        | Split list -> list.Length - 1
    static member InitEmpty qwCount =
        {
            Question =
                match qwCount with
                | 1 -> Solid ""
                | n -> List.init n (fun i -> "") |> Split
            ImgKey = ""
            Answer = ""
            Comment = ""
            CommentImgKey = ""
            Points = 1m
            JeopardyPoints = None
            WithChoice = false
        }


type PackageRecord = {
    PackageId : int
    Producer : string
    Name : string
}

type PackageCard = {
    PackageId : int
    Name : string
    Slips : Slip list
}
 with
    member x.GetSlip idx = x.Slips |> List.tryItem idx

type TeamResult = {
    TeamId : int
    TeamName : string
    Points : decimal
    PlaceFrom : int
    PlaceTo : int
    History : Map<QwKey, decimal>
}

type QuestionResult = {
    Key : QwKey
    Name : string
}

module RegModels =
    type QuizRecord = {
        QuizId : int
        StartTime : System.DateTime option
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

    type QuizRegRecord = {
        QuizId : int
        StartTime : System.DateTime option
        Name : string
        Status : QuizStatus
        Description : string
        ImgKey : string
        EventPage : string
        Comp : ExpertCompetition option
    }

    type QuizProdRecord = {
        QuizId : int
        StartTime : System.DateTime option
        Name : string
        Status : QuizStatus
        AdminToken : string
    }

    type QuizProdCard = {
        QuizId : int
        StartTime : System.DateTime option
        Name : string
        ImgKey : string
        ListenToken : string
        AdminToken : string
        RegToken : string
        WelcomeText : string
        FarewellText : string
        WithPremoderation : bool
        EventPage : string
        MixlrCode : int option
    }

    type SettingsCard = {
        UserId : string
        DefaultImg : string
        DefaultMixlr : int option
    }

    type ExpertRecord = {
        Id : string
        Name : string
    }
    type PackageCard = {
        PackageId : int
        Producer : string
        Name : string
        TransferToken : string
        SharedWith : ExpertRecord list
        Slips : Slip list
    }
     with
        member x.GetSlip idx = x.Slips |> List.tryItem idx
        member x.AppendSlip slip = {x with Slips = x.Slips @ [slip]}
        member x.UpdateSlip idx slip = {x with Slips = x.Slips |> List.mapi (fun i q -> if idx = i then slip else q)}
        member x.DelSlip idx = {x with Slips = x.Slips |> List.indexed |> List.choose (fun (i, s) -> if idx = i then None else Some s) }

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
        QwIdx : int
        QwPartIdx : int
        Slip : Slip
        StartTime : System.DateTime option
    }
    with
        member x.SecondsLeft now =
            match x.StartTime with
            | Some st ->
                match  (x.Seconds + 10) - int((now - st).TotalSeconds) with
                | seconds when seconds > 0 -> seconds
                | _ -> 0
            | _ -> x.Seconds

        member x.IsCoundownActive now =
            x.Status = Countdown && x.SecondsLeft now > 0

        member x.IsLastPart =
            x.QwPartIdx >= (x.Slip.LastQwPartIdx x.QwIdx)

        member x.IsLastQuestionAndPart =
            x.QwIdx >= x.Slip.LastQwIdx && x.QwPartIdx >= (x.Slip.LastQwPartIdx x.QwIdx)

        member x.IsReadyForCountdown =
            match x.Slip with
            | Single slip -> x.QwPartIdx >= slip.LastPartIdx
            | Multiple _ -> x.QwIdx > x.Slip.LastQwIdx


    type QuizControlCard = {
        QuizStatus : QuizStatus
        PackageId : int option
        PackageSlipIdx : int option
        CurrentTour : TourControlCard option
        StreamUrl : string option
    }

    type Answer = {
        Txt : string
        Jpd : bool
        RT : System.DateTime
        Res : decimal option
        IsA : bool
        UT : System.DateTime option
    }

    type QuestionRecord = {
        Key : QwKey
        Nm : string
        Sec : int
        TS : TourStatus
        ST : System.DateTime option
        Pt : decimal
        JpdPt : decimal option
        Ch : bool
        Ann : string
        Awr : string
    }

    type TeamAnswersRecord = {
        Id : int
        Nm : string
        Awrs : Map<QwKey,Answer>
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
        member x.GetQw idx =
            x.Questions |> List.tryFind (fun qw -> qw.Key = idx)
        member x.GetAw teamId idx =
            match x.Teams |> List.tryFind (fun t -> t.Id = teamId) with
            | Some team -> team.GetAw idx
            | None -> None
        member x.FindAnswers idx txt jpd =
            x.Teams
            |> List.map (fun t -> t.Id, (t.GetAw idx))
            |> List.filter (fun (_,a) -> a.IsSome && a.Value.Txt = txt && a.Value.Jpd = jpd)
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
        Aw : Map<int,(string*bool)>
        LT : string
        Mxlr : int option
        Url : string option
        V : int
    } with
        member x.Msg =
            match x.QS with
            | Setup | Live -> x.Wcm
            | Finished -> x.Fwl

    type TeamHistoryRecord = {
        QwIdx : int
        QwName : string
        QwAw : string
        AwTxt : string option
        AwJpd : bool
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
        Url : string option
        V : int
    } with
        member x.Msg =
            match x.QS with
            | Setup | Live -> x.Wcm
            | Finished -> x.Fwl

    type HistoryRecord = {
        QwKey : QwKey
        QwName : string
        QwAw : string
    }

module Infra =
    let private prefix = "media"

    let routeBuilder clientPath (typeName: string) (methodName: string) =
        sprintf "%s/app/api/%s/%s" clientPath typeName methodName

    let sseUrl quizId lastQuizVersion listenToken =
        sprintf "/sse?quiz=%i&start=%i&token=%s" quizId lastQuizVersion listenToken

    let s3KeyForMedia key =
        sprintf "%s/%s" prefix key

    let urlForMedia mediaHost key =
        sprintf "%s/%s/%s" mediaHost prefix key

    let urlForMediaOrDefault mediaHost key defaultKey=
        urlForMedia mediaHost (if key <> "" then key else defaultKey)

    let urlForMediaImgSafe mediaHost key =
        urlForMedia mediaHost (if key <> "" then key else defaultMediaImg256)

    let defaultMediaImg256 = "logo256.png"
    let defaultMediaImg = "logo.png"

type ISecurityApi = {
    login : REQ<LoginReq> -> ARESP<{|Token: string; RefreshToken: string; User: User; Settings: Settings|}>
    refreshToken : REQ<{|RefreshToken: string|}> -> ARESP<{|Token: string; RefreshToken: string|}>
}

type GetUrlMethod = REQ<{|Cat:MediaCategory|}> -> ARESP<{|BucketKey:string; Url: string|}>

type IMainApi = {
    becomeProducer : REQ<unit> -> ARESP<unit>
    createQuiz : REQ<unit> -> ARESP<{|Record : MainModels.QuizProdRecord; Card: MainModels.QuizProdCard|}>
    getProdQuizzes : REQ<unit> -> ARESP<MainModels.QuizProdRecord list>
    getProdQuizCard : REQ<{|QuizId:int|}> -> ARESP<MainModels.QuizProdCard>
    updateProdQuizCard : REQ<MainModels.QuizProdCard> -> ARESP<MainModels.QuizProdRecord>
    deleteQuiz : REQ<{|QuizId : int|}> -> ARESP<unit>
    getRegModel : REQ<unit> -> ARESP<MainModels.QuizRegRecord>
    registerTeam : REQ<{|TeamName: string|}> -> ARESP<MainModels.QuizRegRecord>
    getProdPackages : REQ<unit> -> ARESP<PackageRecord list>
    getProdPackageCard : REQ<{|PackageId : int|}> -> ARESP<MainModels.PackageCard>
    createPackage : REQ<unit> -> ARESP<{|Record : PackageRecord; Card: MainModels.PackageCard|}>
    updateProdPackageCard : REQ<MainModels.PackageCard> -> ARESP<PackageRecord>
    aquirePackage : REQ<{|PackageId:int; TransferToken:string|}> -> ARESP<PackageRecord>
    deletePackage: REQ<{|PackageId : int|}> -> ARESP<unit>
    getSettings : REQ<unit> -> ARESP<MainModels.SettingsCard>
    updateSettings : REQ<MainModels.SettingsCard> -> ARESP<MainModels.SettingsCard>
    sharePackage : REQ<{|PackageId : int; UserId:string|}> -> ARESP<MainModels.ExpertRecord>
    removePackageShare : REQ<{|PackageId : int; UserId:string|}> -> ARESP<unit>
    getUploadUrl : GetUrlMethod
}

type IAdminApi = {
    getTeams : REQ<unit> -> ARESP<AdminModels.TeamRecord list>
    createTeam : REQ<{|TeamName : string|}> -> ARESP<{|Record : AdminModels.TeamRecord|}>
    createTeamBatch : REQ<{|TeamNames : string list|}> -> ARESP<unit>
    getTeamCard : REQ<{|TeamId : int|}> -> ARESP<AdminModels.TeamCard>
    updateTeamCard : REQ<AdminModels.TeamCard> -> ARESP<AdminModels.TeamRecord>
    changeTeamStatus : REQ<{|TeamId : int; TeamStatus : TeamStatus|}> -> ARESP<AdminModels.TeamRecord>
    getQuizCard : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    changeQuizStatus : REQ<{|QuizStatus : QuizStatus|}> -> ARESP<AdminModels.QuizControlCard>
    changeStreamUrl : REQ<string> -> ARESP<AdminModels.QuizControlCard>
    getPackages : REQ<unit> -> ARESP<PackageRecord list>
    setPackage : REQ<{|PackageId: int option|}> -> ARESP<AdminModels.QuizControlCard>
    getPackageCard : REQ<{|PackageId: int|}> -> ARESP<PackageCard>
    startCountDown : REQ<AdminModels.QuizControlCard> -> ARESP<AdminModels.QuizControlCard>
    pauseCountDown : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    settleTour : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    nextTour : REQ<unit> -> ARESP<AdminModels.QuizControlCard>
    nextQuestion : REQ<AdminModels.QuizControlCard> -> ARESP<AdminModels.QuizControlCard>
    nextQuestionPart : REQ<AdminModels.QuizControlCard> -> ARESP<AdminModels.QuizControlCard>
    getAnswers : REQ<unit> -> ARESP<AdminModels.AnswersBundle>
    updateResults : REQ<{|TeamId: int; QwKey: QwKey; Res: decimal option |} list> -> ARESP<unit>
    getResults : REQ<unit> -> ARESP<{|Teams: TeamResult list; Questions : QuestionResult list|}>
    getListenToken : REQ<unit> -> ARESP<string>
}

type ITeamApi = {
    getState : REQ<unit> -> ARESP<TeamModels.QuizCard>
    takeActiveSession : REQ<unit> -> ARESP<TeamModels.QuizCard>
    answers : REQ<Map<QwKey,(string*bool)>> -> ARESP<unit>
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