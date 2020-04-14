module rec Domain

open System
open Common

type RefreshToken = {
    Value : string
    Expired : System.DateTime
}

type Expert = {
    Id : string
    IsProducer : bool
    Competitions : Map<int,int>    // quizId => teamId
    Quizes : int list   // quizId
    Packages : int list   // packageId
}

module Experts =
    let createNew id =
        {Id = id; IsProducer = false; Competitions = Map.empty; Quizes = []; Packages = []}

    let becomeProducer (expert:Expert) =
        {expert with IsProducer = true}

    let addQuiz quizId (expert:Expert)  =
        {expert with Quizes = quizId :: expert.Quizes}

    let addPackage packageId (expert:Expert)  =
        {expert with Packages = packageId :: expert.Packages}

    let getComp quizId (expert:Expert) =
        match expert.Competitions.TryGetValue quizId with
        | true, teamId -> Some teamId
        | _ -> None

    let addComp quizId teamId (expert:Expert) =
        {expert with Competitions = expert.Competitions.Add(quizId, teamId)}


type QuizStatus =
    | Draft
    | Published
    | Live
    | Finished
    | Archived

type QuizQuestionStatus =
    | Unknown
    | Announcing
    | Countdown
    | Settled

type QuizQuestion = {
    Name : string
    Seconds : int
    Status : QuizQuestionStatus
    Text : string
    ImgKey : string
    Answer : string
    Comment : string
    CommentImgKey : string
    StartTime : System.DateTime option
}

type QuizDescriptor = {
    QuizId : int
    Producer : string
    StartTime : System.DateTime option
    Brand : string
    Name : string
    Status : QuizStatus
    WelcomeText : string
    FarewellText : string
    IsPrivate : bool
    ImgKey : string
    WithPremoderation : bool
    ListenToken : string
    AdminToken : string
    RegToken : string
}

type Quiz = {
    Dsc : QuizDescriptor
    Questions : QuizQuestion list
    Version : int
} with
    member this.CurrentQuestion =
        List.tryHead this.Questions
    member this.CurrentQuestionIndex =
        List.length this.Questions
    member this.GetQuestion index =
        if index > 0 && index <= this.Questions.Length then
            Some (this.Questions.Item(this.Questions.Length - index))
        else None

module Quizzes =
    let createNew quizId producerId : Quiz=
        {
            Dsc = {
                QuizId = quizId
                Producer = producerId
                StartTime = None
                Brand = ""
                ImgKey = ""
                Name = sprintf "QUIZ-%i" quizId
                Status = Draft
                WelcomeText = ""
                FarewellText = ""
                IsPrivate = false
                WithPremoderation = false
                ListenToken = Common.generateRandomToken()
                AdminToken = Common.generateRandomToken()
                RegToken = Common.generateRandomToken()
            }
            Questions = []
            Version = 0
        }

    let isPubQuiz (quiz:QuizDescriptor) =
        match quiz.Status with
        | Published | Live | Finished when not quiz.IsPrivate -> true
        | _ -> false

    let getDescription (quiz:QuizDescriptor) =
        match quiz.Status with
            | Draft | Published | Live -> quiz.WelcomeText
            | Finished | Archived -> quiz.FarewellText


type TeamStatus =
    | New
    | Admitted
    | Rejected

type TeamAnswer = {
    Text : string
    RecieveTime : DateTime
    Result : decimal option
    IsAutoResult : bool
    UpdateTime : DateTime option
}

type TeamKey = {
    QuizId : int
    TeamId : int
}

type TeamDescriptor = {
    QuizId : int
    TeamId : int
    Name : string
    Status : TeamStatus
    EntryToken : string
    RegistrationDate : DateTime
} with
    member x.Key = {QuizId = x.QuizId; TeamId = x.TeamId}


type Team = {
    Dsc : TeamDescriptor
    Answers : Map<int, TeamAnswer>
    ActiveSessionId : int
    Version : int
} with
    member x.GetAnswer index =
        x.Answers |> Map.tryFind index

    member x.Points =
        x.Answers |> Map.fold (fun s _ aw -> match aw.Result with Some d -> s + d | None -> s) 0m

    member x.Key = x.Dsc.Key

module Teams =

    let createNew teamId (teamName:string) (quiz:QuizDescriptor) : Team =
        let dsc = {
            QuizId = quiz.QuizId
            TeamId = teamId
            Name = teamName.Trim()
            Status = if quiz.WithPremoderation then New else Admitted
            EntryToken = generateRandomToken()
            RegistrationDate = DateTime.UtcNow
        }

        {Dsc = dsc; Answers = Map.empty; ActiveSessionId = 0; Version = 0}

    let validatePublicTeamUpdate isNewTeam (teamName: string) (teamsInQuiz : TeamDescriptor list) (quiz : QuizDescriptor) =
        let teamName = teamName.Trim()
        match isNewTeam with
        | _ when String.IsNullOrWhiteSpace (teamName) -> Some "Empty name is not allowed"
        | true when quiz.Status <> Published && quiz.Status <> Live -> Some  "Registration is not allowed"
        | false when quiz.Status <> Published -> Some  "Changing name is not allowed"
        | _ when quiz.IsPrivate -> Some  "Public registration is not allowed"
        | _ when teamsInQuiz |> List.exists (fun t -> t.Name.Equals(teamName, StringComparison.InvariantCultureIgnoreCase)) -> Some "Team with such name is alreay registered"
        | _ -> None

    let changeName newName (team:Team) =
        {team with Dsc = {team.Dsc with Name = newName}}

type PackageDescriptor = {
    PackageId : int
    Producer : string
    Name : string
}

type Package = {
    Dsc : PackageDescriptor
    Questions : PackageQuestion list
    Version : int
} with
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

type PackageQuestion = {
    Text : string
    ImgKey : string
    Answer : string
    Comment : string
    CommentImgKey : string
}

module Packages =

    let createNew packageId producerId : Package =
        {
            Dsc = {PackageId = packageId; Producer = producerId; Name = (sprintf "PKG #%i" packageId)}
            Questions = []
            Version = 0
        }
