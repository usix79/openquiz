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
    Slips : Slip list
    Version : int
} with
    member x.GetSlip idx =
        if idx >= 0 && idx <x.Slips.Length then
            Some (x.Slips.Item(idx))
        else None

type Slip =
    | Single of SingeAwSlip
with
    member x.Answers =
        match x with
        | Single slip -> [slip.Answer]
    member x.QuestionsCount =
        match x with
        | Single slip -> slip.Questions |> List.length

type SingeAwSlip = {
    Questions : string list
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
            Slips = []
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

type QuizTourStatus =
    | Announcing
    | Countdown
    | Settled

type QuizTour = {
    Name : string
    Seconds : int
    Status : QuizTourStatus
    StartTime : DateTime option
    NextQwIdx : int
    Slip : Slip
} with
    member x.NextQw () =
        {x with NextQwIdx = if x.NextQwIdx + 1 < x.Slip.QuestionsCount then x.NextQwIdx + 1 else x.Slip.QuestionsCount}

    member x.SetNextQwIndex idx =
        {x with NextQwIdx = if idx < x.Slip.QuestionsCount then idx else x.Slip.QuestionsCount}

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
    PkgSlipIdx : int option
    EventPage : string
    MixlrCode : int option
}

type Quiz = {
    Dsc : QuizDescriptor
    Tours : QuizTour list
    Version : int
} with
    member this.CurrentTour =
        List.tryHead this.Tours
    member this.CurrentTourIndex =
        List.length this.Tours
    member this.GetTour index =
        if index > 0 && index <= this.Tours.Length then
            Some (this.Tours.Item(this.Tours.Length - index))
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
                PkgSlipIdx = None
                EventPage = ""
                MixlrCode = None
            }
            Tours = []
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
        if (quiz.Dsc.PkgId <> packageId) then {quiz with Dsc = {quiz.Dsc with PkgId = packageId; PkgSlipIdx = None}}
        else quiz

    let setSlipIndex slipIdx (quiz:Quiz) =
        match quiz.Dsc.PkgId with
        | Some _ -> {quiz with Dsc = {quiz.Dsc with PkgSlipIdx = Some slipIdx}}
        | None -> quiz

    let addSlip (slip:Slip option) (quiz:Quiz) =
        match slip with
        | Some slip ->
            match slip with
            | Single s -> quiz |> addSingleSlip (Some s)
        | None -> quiz |> addSingleSlip None

    let addSingleSlip (slip:SingeAwSlip option) (quiz:Quiz) =
        let name, seconds =
            match quiz.CurrentTour with
            | Some qw ->
                let newName =
                    let m = Regex.Match (qw.Name, "([^\\d]*)(\\d+)")
                    if m.Success then ((m.Groups.Item 1).Value) + (System.Int32.Parse((m.Groups.Item 2).Value) + 1).ToString()
                    else qw.Name  + "1"
                newName, qw.Seconds
            | None -> "Question 1", 60

        {quiz with Tours = {
                    Name = name
                    Seconds = seconds
                    Status = Announcing
                    Slip = Single {
                        Questions = if slip.IsSome then slip.Value.Questions else [""];
                        ImgKey = if slip.IsSome then slip.Value.ImgKey else "";
                        Answer = if slip.IsSome then slip.Value.Answer else "";
                        Comment = if slip.IsSome then slip.Value.Comment else "";
                        CommentImgKey = if slip.IsSome then slip.Value.CommentImgKey else "";
                    }
                    NextQwIdx = 0
                    StartTime = None}
                    :: quiz.Tours}

    let changeStatus status  (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        match {quiz with Dsc = {quiz.Dsc with Status = status}} with
        | quiz when status = Live && quiz.Tours.Length = 0 ->
            match quiz.Dsc.PkgId with
            | Some pkgId ->
                match pkgProvider pkgId with
                | Some pkg ->
                    let qwIdx = defaultArg quiz.Dsc.PkgSlipIdx 0
                    quiz |> setSlipIndex qwIdx |> addSlip (pkg.GetSlip qwIdx)
                | None ->  quiz |> addSingleSlip None
            | None -> quiz |> addSingleSlip None
        | quiz -> quiz

    let private updateCurrentTour (f : QuizTour -> QuizTour) (quiz:Quiz) =
        match quiz.Dsc.Status with
        | Live ->
            let newList =
                match quiz.Tours with
                | qw :: tail -> (f qw) :: tail
                | _ -> quiz.Tours

            {quiz with Tours = newList}
        | _ ->
            quiz

    let setQuestionIdx qwIdx (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> tour.SetNextQwIndex qwIdx) |> Ok

    let update qwName seconds pkgQwIdx slip (quiz:Quiz) =
        {quiz with Dsc = {quiz.Dsc with PkgSlipIdx = pkgQwIdx}}
        |> updateCurrentTour (fun tour ->
                    {tour.NextQw() with
                        Name = qwName
                        Seconds = seconds
                        Slip = slip
                    }
        )

    let nextQuestion tourName seconds pkgQwIdx slip (quiz:Quiz) =
        {quiz with Dsc = {quiz.Dsc with PkgSlipIdx = pkgQwIdx}}
        |> updateCurrentTour (fun tour -> {tour.NextQw () with Name = tourName; Seconds = seconds; Slip = slip})
        |> Ok

    let startCountdown now (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> {tour.NextQw() with Status = Countdown; StartTime = Some now }) |> Ok

    let pauseCountdown (quiz:Quiz) =
        quiz |> updateCurrentTour (fun qw -> {qw with Status = Announcing; StartTime = None}) |> Ok

    let settle (quiz:Quiz) =
        quiz |> updateCurrentTour (fun qw -> {qw with Status = Settled}) |> Ok

    let next (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        match quiz.Dsc.PkgId with
        | Some pkgId ->
            let qwIdx = (defaultArg quiz.Dsc.PkgSlipIdx 0) + 1
            let qw =
                match pkgProvider pkgId with
                | Some pkg -> pkg.GetSlip qwIdx
                | None -> None
            quiz |> setSlipIndex qwIdx |> addSlip qw
        | None -> quiz |> addSingleSlip None

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

    let createNewAdmin teamId (teamName:string) (quiz:QuizDescriptor) status : Team =
        let dsc = {
            QuizId = quiz.QuizId
            TeamId = teamId
            Name = teamName.Trim()
            Status = status
            EntryToken = generateRandomToken()
            RegistrationDate = DateTime.UtcNow
            ActiveSessionId = 0
        }

        {Dsc = dsc; Answers = Map.empty; Version = 0}

    let createNew teamId (teamName:string) (quiz:QuizDescriptor) : Team =
        if quiz.WithPremoderation then New else Admitted
        |>createNewAdmin teamId teamName quiz

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