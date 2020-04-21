module rec Domain

open System
open System.Text.RegularExpressions

open Common

type Provider<'TKey, 'T> = 'TKey -> 'T option

type RefreshToken = {
    Value : string
    Expired : System.DateTime
}

type Expert = {
    Id : string
    Username : string
    IsProducer : bool
    Competitions : Map<int,int>    // quizId => teamId
    Quizes : int list   // quizId
    Packages : int list   // packageId
    Version : int
}

module Experts =
    let createNew id username =
        {Id = id; Username = username; IsProducer = false; Competitions = Map.empty; Quizes = []; Packages = []; Version = 0}

    let becomeProducer (expert:Expert) =
        {expert with IsProducer = true}

    let addQuiz quizId (expert:Expert)  =
        {expert with Quizes = quizId :: expert.Quizes}

    let addPackage packageId (expert:Expert)  =
        {expert with Packages = packageId :: expert.Packages} |> Ok

    let removePackage packageId (expert:Expert)  =
        {expert with Packages = expert.Packages |> List.filter (fun id -> id <> packageId)} |> Ok

    let getComp quizId (expert:Expert) =
        match expert.Competitions.TryGetValue quizId with
        | true, teamId -> Some teamId
        | _ -> None

    let addComp quizId teamId (expert:Expert) =
        {expert with Competitions = expert.Competitions.Add(quizId, teamId)}

type PackageDescriptor = {
    PackageId : int
    Producer : string
    Name : string
}

type Package = {
    Dsc : PackageDescriptor
    TransferToken : string
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
            Dsc = {
                PackageId = packageId
                Producer = producerId
                Name = (sprintf "PKG #%i" packageId)
            }
            TransferToken = generateRandomToken()
            Questions = []
            Version = 0
        }

    let transfer  expertId (token:string) (package:Package) =
        match token.Trim() with
        | "" -> Error "Transfer Token Is Empty"
        | token when token <> package.TransferToken.Trim() -> Error "Invalid Transfer Token"
        | _ -> {package with
                    Dsc = {package.Dsc with Producer = expertId}
                    TransferToken = generateRandomToken()
               }|> Ok


type QuizStatus =
    | Draft
    | Published
    | Live
    | Finished
    | Archived

type QuizQuestionStatus =
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
    StartTime : DateTime option
}

type QuizDescriptor = {
    QuizId : int
    Producer : string
    StartTime : DateTime option
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
    PkgId : int option
    PkgQwIdx : int option
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
                PkgId = None
                PkgQwIdx = None
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

    let setPackageId (packageId : int option) (quiz : Quiz) =
        if (quiz.Dsc.PkgId <> packageId) then {quiz with Dsc = {quiz.Dsc with PkgId = packageId; PkgQwIdx = None}}
        else quiz

    let addQuestion (qw:PackageQuestion option) (quiz:Quiz) =
        let name, seconds =
            match quiz.CurrentQuestion with
            | Some qw ->
                let newName =
                    let m = Regex.Match (qw.Name, "([^\\d]*)(\\d+)")
                    if m.Success then ((m.Groups.Item 1).Value) + (System.Int32.Parse((m.Groups.Item 2).Value) + 1).ToString()
                    else qw.Name  + "1"
                newName, qw.Seconds
            | None -> "1", 60

        {quiz with Questions = {
                    Name = name;
                    Seconds = seconds;
                    Status = Announcing
                    Text = if qw.IsSome then qw.Value.Text else "";
                    ImgKey = if qw.IsSome then qw.Value.ImgKey else "";
                    Answer = if qw.IsSome then qw.Value.Answer else "";
                    Comment = if qw.IsSome then qw.Value.Comment else "";
                    CommentImgKey = if qw.IsSome then qw.Value.CommentImgKey else "";
                    StartTime = None}
                    :: quiz.Questions}

    let changeStatus status  (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        match {quiz with Dsc = {quiz.Dsc with Status = status}} with
        | quiz when status = Live && quiz.Questions.Length = 0 && quiz.Dsc.PkgId.IsSome ->
            match pkgProvider quiz.Dsc.PkgId.Value with
            | Some pkg -> addQuestion (pkg.GetQuestion (defaultArg quiz.Dsc.PkgQwIdx 0)) quiz
            | None -> quiz
        | quiz -> quiz

    let private updateCurrentQuestion (f : QuizQuestion -> QuizQuestion) (quiz:Quiz) =
        match quiz.Dsc.Status with
        | Live ->
            let newList =
                match quiz.Questions with
                | qw :: tail -> (f qw) :: tail
                | _ -> quiz.Questions

            {quiz with Questions = newList}
        | _ ->
            quiz

    let startCountdown qwName seconds qwText qwImgKey qwAnswer qwComment qwCommentImgKey pkgQwIdx now (quiz:Quiz) =
        {quiz with Dsc = {quiz.Dsc with PkgQwIdx = pkgQwIdx}}
        |> updateCurrentQuestion (fun qw ->
                    {qw with
                        Name = qwName
                        Seconds = seconds
                        Status = Countdown
                        StartTime = Some now
                        Text = qwText
                        ImgKey = qwImgKey
                        Answer = qwAnswer
                        Comment = qwComment
                        CommentImgKey = qwCommentImgKey
                    }
        )
        |> Ok

    let pauseCountdown (quiz:Quiz) =
        quiz |> updateCurrentQuestion (fun qw -> {qw with Status = Announcing; StartTime = None}) |> Ok

    let settle (quiz:Quiz) =
        quiz |> updateCurrentQuestion (fun qw -> {qw with Status = Settled}) |> Ok

    let next (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        match quiz.Dsc.PkgId with
        | Some pkgId ->
            let qwIdx = (defaultArg quiz.Dsc.PkgQwIdx 0) + 1
            let qw =
                match pkgProvider pkgId with
                | Some pkg -> pkg.GetQuestion qwIdx
                | None -> None
            {quiz with Dsc = {quiz.Dsc with PkgQwIdx = Some qwIdx}} |> addQuestion qw
        | None -> quiz |> addQuestion None

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
    ActiveSessionId : int
} with
    member x.Key = {QuizId = x.QuizId; TeamId = x.TeamId}


type Team = {
    Dsc : TeamDescriptor
    Answers : Map<int, TeamAnswer>
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
            ActiveSessionId = 0
        }

        {Dsc = dsc; Answers = Map.empty; Version = 0}

    let dsc (f : TeamDescriptor -> Result<TeamDescriptor,string>) (team:Team) =
        result {
            let! dsc = f team.Dsc
            return {team with Dsc = dsc}
        }

    let validateTeamUpdate isNewTeam (teamName: string) (teamsInQuiz : TeamDescriptor list) (quiz : QuizDescriptor) =
        let teamName = teamName.Trim()
        match isNewTeam with
        | _ when String.IsNullOrWhiteSpace (teamName) -> Some "Empty name is not allowed"
        | true when quiz.Status <> Published && quiz.Status <> Live -> Some  "Registration is not allowed"
        | false when quiz.Status <> Published -> Some  "Changing name is not allowed"
        | _ when teamsInQuiz |> List.exists (fun t -> t.Name.Equals(teamName, StringComparison.InvariantCultureIgnoreCase)) -> Some "Team with such name is alreay registered"
        | _ -> None

    let changeName newName (team:Team) =
        {team with Dsc = {team.Dsc with Name = newName}}

    let updateAnswer qwIdx (f : TeamAnswer -> TeamAnswer) (team: Team) =
        match team.GetAnswer qwIdx with
        | Some aw -> {team with Answers = team.Answers |> Map.add qwIdx (f aw)}
        | None -> team

    let settleAnswer qwIdx (jury : string -> bool) now (team: Team) =
        team |> updateAnswer qwIdx (fun aw ->
            if (aw.IsAutoResult || aw.Result.IsNone) && (jury aw.Text) then
                {aw with
                    Result = Some 1m
                    IsAutoResult = true
                    UpdateTime = Some now
                }
            else aw
        )

    let registerAnswer qwIndex awText now (team:Team) =
        if String.IsNullOrWhiteSpace awText then Error "Answer is empty"
        else
            let awText = if awText.Length <= 256 then awText else awText.Substring(0, 256)

            match team.Answers.TryFind qwIndex with
            | None ->
                Ok {team with Answers = team.Answers.Add (qwIndex, {Text = awText; RecieveTime = now; Result = None; IsAutoResult = false; UpdateTime = Some now})}
            | Some aw -> Error <| "Answer is alredy registered: " + aw.Text

    let updateResult qwIdx res now (team:Team) =
        team |> updateAnswer qwIdx (fun aw ->
            {aw with Result = res; IsAutoResult = false; UpdateTime = Some now}
        )