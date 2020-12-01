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
    Name : string
    IsProducer : bool
    Competitions : Map<int,int>    // quizId => teamId
    Quizzes : int list   // quizId
    Packages : int list   // packageId
    PackagesSharedWithMe : int list
    DefaultImg : string
    DefaultMixlr : int option
    Version : int
} with
    member x.AllPackages = x.Packages @ x.PackagesSharedWithMe

module Experts =
    let createNew id username name =
        {Id = id
         Username = username
         Name = name
         IsProducer = false
         Competitions = Map.empty
         Quizzes = []
         Packages = []
         PackagesSharedWithMe = []
         DefaultImg = ""
         DefaultMixlr = None
         Version = 0}

    let becomeProducer (expert:Expert) =
        {expert with IsProducer = true}

    let addQuiz quizId (expert:Expert)  =
        {expert with Quizzes = quizId :: expert.Quizzes}

    let addPackage packageId (expert:Expert)  =
        {expert with Packages = packageId :: expert.Packages} |> Ok

    let removePackage packageId (expert:Expert)  =
        {expert with Packages = expert.Packages |> List.filter (fun id -> id <> packageId)} |> Ok

    let removeQuiz quizId (expert:Expert)  =
        {expert with Quizzes = expert.Quizzes |> List.filter (fun id -> id <> quizId)} |> Ok

    let getComp quizId (expert:Expert) =
        match expert.Competitions.TryGetValue quizId with
        | true, teamId -> Some teamId
        | _ -> None

    let addComp quizId teamId (expert:Expert) =
        {expert with Competitions = expert.Competitions.Add(quizId, teamId)}

    let addSharedPackage packageId (expert:Expert) =
        {expert with PackagesSharedWithMe = (packageId :: expert.PackagesSharedWithMe) |> List.distinct} |> Ok

    let removeSharedPackage packageId (expert:Expert) =
        {expert with PackagesSharedWithMe = expert.PackagesSharedWithMe |> List.filter (fun id -> id <> packageId)} |> Ok

    let authorizePackageRead packageId (expert:Expert) =
        match expert.AllPackages |> List.contains packageId with
        | true -> Ok ()
        | false -> Error "You are not authorized to load the package"

    let authorizePackageWrite packageId (expert:Expert) =
        match expert.Packages |> List.contains packageId with
        | true -> Ok ()
        | false -> Error "You are not authorized to change the package"

type PackageDescriptor = {
    PackageId : int
    Producer : string
    Name : string
}

type Package = {
    Dsc : PackageDescriptor
    TransferToken : string
    SharedWith : string list
    Slips : Slip list
    Version : int
} with
    member x.GetSlip idx =
        if idx >= 0 && idx <x.Slips.Length then
            Some (x.Slips.Item(idx))
        else None

type Slip =
    | Single of SingleSlip
    | Multiple of name:string * SingleSlip list
with
    member x.Answers =
        match x with
        | Single slip -> [slip.Answer]
        | Multiple (_,slips) -> slips |> List.map (fun s -> s.Answer)
    member x.QuestionsCount =
        match x with
        | Single _ -> 1
        | Multiple (_,slips) -> slips.Length
    member x.QuestionsPartCount qwIdx =
        match x with
        | Single slip -> if qwIdx = 0 then slip.QuestionsCount else 1
        | Multiple (_,slips) -> slips |> List.tryItem qwIdx |> Option.map (fun slip -> slip.QuestionsCount) |> Option.defaultValue 0
    member x.SecondsDevider =
        match x with
        | Single slip -> slip.QuestionsCount
        | _ -> 1

type Question =
    | Solid of string
    | Split of string list

type ChoiceAnswer = {
    Text: string
    IsCorrect: bool
}
type Answer =
    | OpenAnswer of string
    | ChoiceAnswer of ChoiceAnswer list
    with
        member x.ToRawString () =
            match x with
            | OpenAnswer txt -> txt
            | ChoiceAnswer list ->
                String.Join("\n", list |> List.choose (fun ch -> if ch.IsCorrect then Some ch.Text else None))

type MediaType =
    | Picture
    | Audio
    | Video

type MediaDsc = {
    Key : string
    Type : MediaType
}

type SingleSlip = {
    Caption : string
    Question : Question
    QuestionMedia : MediaDsc option
    Answer : Answer
    AnswerMedia : MediaDsc option
    Comment : string
    Points : decimal
    JeopardyPoints : decimal option
    WithChoice : bool
    EndOfTour : bool
} with
    member x.QuestionsCount =
        match x.Question with
        | Solid _ -> 1
        | Split list -> list.Length
    member x.QuestionText idx =
        match x.Question with
        | Solid qw -> qw
        | Split list -> list |> List.tryItem idx |> Option.defaultValue ""

    static member InitEmpty qwCount =
        {
            Caption = ""
            Question =
                match qwCount with
                | 1 -> Solid ""
                | n -> List.init n (fun i -> "") |> Split
            QuestionMedia = None
            Answer= OpenAnswer ""
            AnswerMedia = None
            Comment=""
            Points = 1m
            JeopardyPoints = None
            WithChoice = false
            EndOfTour = false
        }

module Packages =

    let createNew producerId packageId : Package =
        {
            Dsc = {
                PackageId = packageId
                Producer = producerId
                Name = (sprintf "PKG #%i" packageId)
            }
            SharedWith = List.empty
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

    let shareWith (expertId:string) (package:Package) =
        {package with SharedWith = (expertId :: package.SharedWith) |> List.distinct} |> Ok

    let removeShareWith (expertId:string) (package:Package) =
        {package with SharedWith = package.SharedWith |> List.filter ((<>) expertId)} |> Ok

type QuizStatus =
    | Setup
    | Live
    | Finished

type QuizTourStatus =
    | Announcing
    | Countdown
    | Settled

type QuizTour = {
    Name : string
    Seconds : int
    Status : QuizTourStatus
    StartTime : DateTime option
    QwIdx : int
    QwPartIdx : int
    IsQuestionDisplayed : bool
    Slip : Slip
} with
    member x.NextQw () =
        {x with
            QwIdx = if x.QwIdx + 1 < x.Slip.QuestionsCount then x.QwIdx + 1 else x.Slip.QuestionsCount
            QwPartIdx = 0
        }

    member x.NextQwPart () =
        let nextIdx = x.QwPartIdx + 1
        let maxIdx = (x.Slip.QuestionsPartCount x.QwIdx) - 1
        {x with QwPartIdx = min nextIdx maxIdx}

    member x.SetNextQwIndex idx =
        {x with QwIdx = if idx < x.Slip.QuestionsCount then idx else x.Slip.QuestionsCount}

    member x.SetNextQwPartIndex qwIdx idx =
        {x with QwIdx = if idx < (x.Slip.QuestionsPartCount qwIdx) then idx else (x.Slip.QuestionsPartCount qwIdx)}

type QuizDescriptor = {
    QuizId : int
    Producer : string
    StartTime : DateTime option
    Name : string
    Status : QuizStatus
    WelcomeText : string
    FarewellText : string
    InfoText : string
    ImgKey : string
    WithPremoderation : bool
    ListenToken : string
    AdminToken : string
    RegToken : string
    PkgId : int option
    PkgSlipIdx : int option
    EventPage : string
    MixlrCode : int option
    StreamUrl : string option
}

type Quiz = {
    Dsc : QuizDescriptor
    Tours : QuizTour list
    Version : int
} with
    member this.CurrentTour =
        List.tryHead this.Tours
    member this.CurrentTourIndex =
        List.length this.Tours - 1
    member this.GetTour index =
        this.Tours |> List.tryItem index

module Quizzes =
    let createNew quizId producerId defImg defMixlr: Quiz=
        {
            Dsc = {
                QuizId = quizId
                Producer = producerId
                StartTime = None
                ImgKey = defImg
                Name = sprintf "QUIZ-%i" quizId
                Status = Setup
                WelcomeText = ""
                FarewellText = ""
                InfoText = ""
                WithPremoderation = false
                ListenToken = Common.generateRandomToken()
                AdminToken = Common.generateRandomToken()
                RegToken = Common.generateRandomToken()
                PkgId = None
                PkgSlipIdx = None
                EventPage = ""
                MixlrCode = defMixlr
                StreamUrl = None
            }
            Tours = []
            Version = 0
        }

    let authorize expertId (quiz:QuizDescriptor) =
        if quiz.Producer <> expertId then Error "Quiz is produced by someone else" else Ok quiz

    let getDescription (quiz:QuizDescriptor) =
        match quiz.Status with
            | Setup | Live -> quiz.WelcomeText
            | Finished -> quiz.FarewellText

    let setPackageId (packageId : int option) (quiz : Quiz) =
        if (quiz.Dsc.PkgId <> packageId) then {quiz with Dsc = {quiz.Dsc with PkgId = packageId; PkgSlipIdx = None}}
        else quiz

    let setSlipIndex slipIdx (quiz:Quiz) =
        match quiz.Dsc.PkgId with
        | Some _ -> {quiz with Dsc = {quiz.Dsc with PkgSlipIdx = Some slipIdx}}
        | None -> quiz

    let addEmptySlip (quiz:Quiz) =
        quiz|> addSlip (SingleSlip.InitEmpty 1 |> Single)

    let getNextTourName (slip:Slip) (quiz:Quiz) =
        let genNextName () =
            match quiz.CurrentTour with
            | Some tour ->
                let newName =
                    let m = Regex.Match (tour.Name, "([^\\d]*)(\\d+)")
                    if m.Success then ((m.Groups.Item 1).Value) + (System.Int32.Parse((m.Groups.Item 2).Value) + 1).ToString()
                    else tour.Name  + "1"
                newName
            | None -> "1"
        match slip with
        | Single slip -> if slip.Caption <> "" then slip.Caption else genNextName()
        | Multiple _ -> genNextName()

    let getNextTourSeconds (quiz:Quiz) =
        match quiz.CurrentTour with
        | Some tour ->
            match tour.Slip with
            | Single slip -> tour.Seconds * slip.QuestionsCount
            | _ -> tour.Seconds
        | None -> 60

    let addSlip (slip:Slip) (quiz:Quiz) =
        {quiz with
            Tours = {
                Name =  getNextTourName slip quiz
                Seconds = (quiz |> getNextTourSeconds) / (slip.SecondsDevider)
                Status = Announcing
                Slip = slip
                QwIdx = 0
                QwPartIdx = 0
                IsQuestionDisplayed = false
                StartTime = None }
                :: quiz.Tours}

    let addNextSlip qwIdx (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        quiz.Dsc.PkgId
        |> Option.bind pkgProvider
        |> Option.bind (fun pkg -> pkg.GetSlip qwIdx)
        |> Option.bind (fun slip -> quiz |> setSlipIndex qwIdx |> addSlip slip |> Some)
        |> Option.defaultValue (quiz |> addEmptySlip)

    let changeStatus status  (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        match {quiz with Dsc = {quiz.Dsc with Status = status}} with
        | quiz when status = Live && quiz.Tours.Length = 0 ->
            let qwIdx = quiz.Dsc.PkgSlipIdx |> Option.defaultValue 0
            quiz |> addNextSlip qwIdx pkgProvider
        | quiz -> quiz

    let private updateCurrentTour (f : QuizTour -> QuizTour) (quiz:Quiz) =
        match quiz.Dsc.Status with
        | Live -> {quiz with Tours = match quiz.Tours with  qw :: tail -> (f qw) :: tail | _ -> quiz.Tours}
        | _ -> quiz

    let setQuestionIdx qwIdx (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> tour.SetNextQwIndex qwIdx) |> Ok

    let setQuestionPartIdx qwIdx qwPartIdx (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> tour.SetNextQwPartIndex qwIdx qwPartIdx) |> Ok

    let update qwName seconds pkgQwIdx qwIdx qwPartIdx slip (quiz:Quiz) =
        {quiz with Dsc = {quiz.Dsc with PkgSlipIdx = pkgQwIdx}}
        |> updateCurrentTour (fun tour -> {tour with Name = qwName; QwIdx = qwIdx; QwPartIdx = qwPartIdx; Seconds = seconds; Slip = slip})

    let nextQuestion (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> tour.NextQw ()) |> Ok

    let nextQuestionPart (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> tour.NextQwPart ()) |> Ok

    let showQuestion (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> {tour with IsQuestionDisplayed = true}) |> Ok

    let startCountdown now (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> {tour with Status = Countdown; StartTime = Some now }) |> Ok

    let pauseCountdown (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> {tour with Status = Announcing; StartTime = None}) |> Ok

    let settle (quiz:Quiz) =
        quiz |> updateCurrentTour (fun tour -> {tour with Status = Settled}) |> Ok

    let next (pkgProvider : Provider<int,Package>) (quiz:Quiz) =
        let qwIdx = quiz.Dsc.PkgSlipIdx |> Option.defaultValue 0 |> (+) 1
        quiz |> addNextSlip qwIdx pkgProvider

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
    Jeopardy : bool
    Vote : bool option
} with
    member x.VotePoints = (match x.Vote with Some true -> 1 | Some false -> -1 | _ -> 0)

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

type QwKey  = {
    TourIdx : int
    QwIdx : int
}

type Team = {
    Dsc : TeamDescriptor
    Answers : Map<QwKey, TeamAnswer>
    Version : int
} with
    member x.GetAnswer qwKey =
        x.Answers |> Map.tryFind qwKey
    member x.SelectAnswers tourIdx =
        x.Answers |> Map.toList |> List.filter (fun (key,aw) -> key.TourIdx = tourIdx)

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
        | true when quiz.Status <> Setup && quiz.Status <> Live -> Some  "Registration is not allowed"
        | false when quiz.Status <> Setup -> Some  "Changing name is not allowed"
        | _ when teamsInQuiz |> List.exists (fun t -> t.Name.Equals(teamName, StringComparison.InvariantCultureIgnoreCase)) -> Some "Team with such name is alreay registered"
        | _ -> None

    let changeName newName (team:Team) =
        {team with Dsc = {team.Dsc with Name = newName}}

    let updateAnswer qwIdx (f : TeamAnswer -> TeamAnswer*bool) (team: Team) =
        match team.GetAnswer qwIdx with
        | Some aw ->
            let (aw, changed) = f aw
            {team with Answers = team.Answers |> Map.add qwIdx aw}, changed
        | None -> team, false

    let settleAnswer qwIdx (jury : string -> bool) points jpdPoints withChoice now (team: Team) =
        team |> updateAnswer qwIdx (fun aw ->
            if (aw.IsAutoResult || aw.Result.IsNone) then
                let result =
                    match jury aw.Text with
                    | true ->
                        match jpdPoints, withChoice with
                        | Some jpdPoints, false -> Some jpdPoints
                        | Some jpdPoints, true when aw.Jeopardy -> Some jpdPoints
                        | _ -> Some points
                    | false ->
                        match jpdPoints, withChoice with
                        | Some jpdPoints, false -> Some (-jpdPoints)
                        | Some jpdPoints, true when aw.Jeopardy -> Some (-jpdPoints)
                        | _ -> None
                match result with
                | Some _ -> {aw with Result = result; IsAutoResult = true; UpdateTime = Some now}, true
                | None -> aw, false
            else aw, false
        )

    let registerAnswer qwIndex awText jeopardy now (team:Team) =
        if String.IsNullOrWhiteSpace awText then Error "Answer is empty"
        else
            let awText = if awText.Length <= 256 then awText else awText.Substring(0, 256)

            match team.Answers.TryFind qwIndex with
            | None ->
                Ok {team with
                        Answers = team.Answers.Add (
                                    qwIndex,
                                    {Text = awText;
                                    Jeopardy = jeopardy;
                                    RecieveTime = now;
                                    Result = None;
                                    IsAutoResult = false;
                                    UpdateTime = Some now;
                                    Vote = None})}
            | Some aw -> Error <| "Answer is alredy registered: " + aw.Text

    let updateResult qwIdx res now (team:Team) =
        team |> updateAnswer qwIdx (fun aw ->
            {aw with Result = res; IsAutoResult = false; UpdateTime = Some now}, true
        )

type TeamDetail = {
    Result : decimal option
    Vote : int
}

type TourResult = {
    Points : decimal
}

type TeamResult = {
    TeamId : int
    TeamName : string
    Points : decimal
    Rating : int
    PlaceFrom : int
    PlaceTo : int
    Tours : TourResult list
    Details : Map<QwKey, TeamDetail>
}

type QuestionResult = {
    Key : QwKey
    Name : string
    EOT : bool
    Score : int
    Votes : int
    Rating : decimal
}

type Results = {
    Teams : TeamResult list
    Questions : QuestionResult list
}

module Results =
    let private questionName tour qwIdx =
        match tour.Slip with
        | Single _ -> tour.Name
        | _ -> sprintf "%s.%i" tour.Name (qwIdx + 1)

    let private activeTeamsCount (teams:Team list) =
        teams |> List.sumBy (fun team -> if team.Answers.Count > 0 then 1 else 0)

    let private correctAnswersAndVotes qwKey (teams:Team list) =
        teams
        |> List.fold (fun (score,votes) team ->
            match team.Answers.TryGetValue qwKey with
            | true, aw ->
                score + (aw.Result |> Option.map (fun r -> if r > 0m then 1 else 0) |> Option.defaultValue 0),
                votes + aw.VotePoints
            | _ -> (score,votes)) (0,0)

    let questionResults (quiz:Quiz) teamsCount teams =

        quiz.Tours
        |> List.rev
        |> List.mapi (fun tourIdx tour ->
            match tour.Slip with
            | Single slip ->
                let key = {TourIdx = tourIdx; QwIdx = 0}
                let (score,votes) = correctAnswersAndVotes key teams
                [{Key = key; Name = questionName tour 0; EOT = slip.EndOfTour;  Score = teamsCount - score; Votes = votes; Rating = 0m}]
            | Multiple (_, slips) ->
                let lastIdx = slips.Length - 1
                slips
                |> List.mapi (fun idx slip ->
                    let key = {TourIdx = tourIdx; QwIdx = idx}
                    let (score,votes) = correctAnswersAndVotes key teams
                    {Key = key; Name = questionName tour idx; EOT = idx = lastIdx;  Score = teamsCount - score; Votes = votes; Rating = 0m})
        )|> List.concat

    let teamDetails (team : Team) =
        team.Answers |> Map.map (fun _ aw -> {Result = aw.Result; Vote = aw.VotePoints})

    let teamToursResults (quiz:Quiz) (team : Team) =
        let mutable tourQuestions = 0
        let mutable tourPoints = 0m
        [   for (idx,tour) in quiz.Tours |> List.rev |> List.mapi (fun idx t -> idx,t) do
                match tour.Slip with
                | Single slip ->
                    tourQuestions <- tourQuestions + 1
                    match team.GetAnswer {TourIdx = idx; QwIdx = 0} with
                    | Some aw ->
                        tourPoints <- tourPoints + (aw.Result |> Option.defaultValue 0.0m)
                    | None -> ()
                    if slip.EndOfTour then
                        yield {Points = tourPoints}
                        tourPoints <- 0m
                        tourQuestions <- 0
                | Multiple _ ->
                    if tourQuestions > 0 then
                        yield {Points = tourPoints}
                        tourPoints <- 0m
                        tourQuestions <- 0
                    yield {
                        Points =
                            team.Answers
                            |> Map.filter (fun key _ -> key.TourIdx = idx)
                            |> Map.fold (fun s key aw -> s + (aw.Result |> Option.defaultValue 0m)) 0m }

            if tourQuestions > 0 then yield {Points = tourPoints}
        ]


    let teamRating (questionResults:QuestionResult list) (team : Team) =
        questionResults
        |> List.sumBy (fun qw ->
            team.GetAnswer qw.Key
            |> Option.map (fun aw ->
                aw.Result
                |> Option.map (fun res -> if res > 0m then qw.Score else 0)
                |> Option.defaultValue 0)
            |> Option.defaultValue 0)

    let teamResults quiz (teams:Team list) (questionResults:QuestionResult list) =
        let mutable currentPlace = 1
        [for ((points,rating),teams) in
            teams
            |> List.groupBy (fun t ->
                t.Points, (teamRating questionResults t))
            |> List.sortByDescending (fun (k, _) -> k) do
                let len = teams.Length

                for team in teams do
                    {TeamId = team.Dsc.TeamId;
                        TeamName = team.Dsc.Name;
                        Points = points;
                        Rating = rating;
                        PlaceFrom = currentPlace;
                        PlaceTo = currentPlace + len - 1;
                        Tours = teamToursResults quiz team;
                        Details = teamDetails team }

                currentPlace <- currentPlace + len
        ]

    let questionRating teamsCount (teamResults:TeamResult list) qwKey =
        if teamsCount > 0 then
            let teamsCount = decimal teamsCount
            teamResults
            |> List.fold (fun state tr ->
                match tr.Details.TryGetValue qwKey with
                | true, details ->
                    let s = decimal details.Vote * (teamsCount - decimal (tr.PlaceFrom + tr.PlaceTo) / 2m + 1m)
                    s + state
                | _ -> state
                ) 0m
            |> (*) (100m / ((1m + teamsCount) / 2m * teamsCount))
        else 0m

    let results (quiz:Quiz) (teams:Team list) : Results =
        let teams = teams |> List.filter (fun t -> t.Dsc.Status = Admitted)
        let teamsCount = activeTeamsCount teams

        let qwResults = questionResults quiz teamsCount teams
        let teamResults = teamResults quiz teams qwResults

        {
            Questions = qwResults |> List.map (fun qr -> {qr with Rating = questionRating teamsCount teamResults qr.Key})
            Teams = teamResults
        }