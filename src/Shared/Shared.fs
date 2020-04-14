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

type User =
    | MainUser of MainUser
    | AdminUser of AdminUser
    | QuizUser of QuizUser

type MainUser = {
    Sub : string
    Name : string
    PictureUrl : string
    IsProducer : bool
}

type QuizUser = {
    QuizId : int
    QuizName : string
    CompetitorId : int
    CompetitorName : string
}

type AdminUser = {
    QuizId : int
    QuizName : string
    QuizImg : string
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
    }

    type PackageProdRecord = {
        PackageId : int
        Name : string
    }

    type PackageProdCard = {
        PackageId : int
        Name : string
        Questions : PackageProdQuestion list
    }
     with
        member x.GetQuestion idx =
            if idx >= 0 && idx <x.Questions.Length then
                Some (x.Questions.Item(idx))
            else None
        member x.UpdateQuestion idx qw =
            {x with
                Questions = x.Questions |> List.mapi (fun i q -> if idx = i then qw else q)
            }
        member x.AddQuestion () =
            {x with
                Questions = x.Questions @ [{Text="";ImgKey="";Answer="";Comment="";CommentImgKey=""}]
            }
        member x.DelQuestion idx =
            {x with
                Questions = x.Questions
                    |> List.mapi (fun i q -> if idx = i then None else Some q)
                    |> List.filter (fun v -> v.IsSome)
                    |> List.map (fun v -> v.Value)
            }

    type PackageProdQuestion = {
        Text : string
        ImgKey : string
        Answer : string
        Comment : string
        CommentImgKey : string
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

module Infra =
    let routeBuilder clientPath (typeName: string) (methodName: string) =
        sprintf "%s/app/api/%s/%s" clientPath typeName methodName

    let sseUrl gameId lastGameVersion listenToken =
        sprintf "api/sse?gameId=%i&startVersion=%i&listenToken=%s" gameId lastGameVersion listenToken

    let urlForImg key =
        sprintf "/img/%s" key

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
    uploadFile : REQ<{|Cat:ImgCategory; FileType : string; FileBody : byte[]|}> -> ARESP<{|BucketKey: string|}>
    getPubModel : REQ<unit> -> ARESP<{|Profile : MainModels.ExpertProfile; Quizzes : MainModels.QuizPubRecord list|}>
    registerTeam : REQ<{|QuizId: int; TeamName: string|}> -> ARESP<MainModels.ExpertCompetition>
    getProdPackages : REQ<unit> -> ARESP<MainModels.PackageProdRecord list>
    getProdPackageCard : REQ<{|PackageId : int|}> -> ARESP<MainModels.PackageProdCard>
    createPackage : REQ<unit> -> ARESP<{|Record : MainModels.PackageProdRecord; Card:MainModels.PackageProdCard|}>
    updateProdPackageCard : REQ<MainModels.PackageProdCard> -> ARESP<MainModels.PackageProdRecord>
}

type IAdminApi = {
    getTeams : REQ<unit> -> ARESP<AdminModels.TeamRecord list>
    createTeam : REQ<{|TeamName : string|}> -> ARESP<{|Record : AdminModels.TeamRecord|}>
    getTeamCard : REQ<{|TeamId : int|}> -> ARESP<AdminModels.TeamCard>
    updateTeamCard : REQ<AdminModels.TeamCard> -> ARESP<AdminModels.TeamRecord>
    changeTeamStatus : REQ<{|TeamId : int; TeamStatus : TeamStatus|}> -> ARESP<AdminModels.TeamRecord>
}