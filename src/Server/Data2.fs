module OpenQuiz.Data2

open System
open System.Threading
open Amazon.DynamoDBv2
open DynamoDb.Ok
open DynamoDb.Ok.Read
open DynamoDb.Ok.Write

open OpenQuiz.Common
open OpenQuiz.Env

module A = Attribute
module P = Parse
module R = AttrReader

module Errors =
    [<Literal>]
    let TransactionFailed = "Transaction Failed"

    [<Literal>]
    let GetError = "DynamoDB Get Error"

    [<Literal>]
    let PutError = "DynamoDB Put Error"

    [<Literal>]
    let QueryError = "DynamoDB Query Error"

    [<Literal>]
    let DeleteError = "DynamoDB Delete Error"

    [<Literal>]
    let ItemDoesNotExist = "ItemDoesNotExist"

let private logErrors (env: #ILog) list =
    env.Logger.Error("{@Proc} {@Details}", "DB", list)

let putOptimistic' env get put id logic =

    let rec tryUpdate attempt =
        async {
            match! get env id with
            | Ok entity ->
                match logic entity with
                | Ok updatedEntity ->
                    match! put env updatedEntity with
                    | Ok Success -> return Ok updatedEntity
                    | Ok Retry when attempt < 3 ->
                        logErrors env [ sprintf "Put Condition Failed for Key %A Attempt %i" id attempt ]
                        do! Async.Sleep(attempt * 100)
                        return! tryUpdate (attempt + 1)
                    | Ok Retry ->
                        logErrors env [ sprintf "Put Condition Failed for Key %A Attempt %i Give Up" id attempt ]
                        return Error Errors.TransactionFailed
                    | Error txt -> return Error txt
                | Error txt -> return Error txt
            | Error txt -> return Error txt
        }

    tryUpdate 1

let client = new AmazonDynamoDBClient()

let semaphore = new SemaphoreSlim(512, 512)

let fullTableName (env: #ICfg) name =
    sprintf "%s-%s" (env.Configurer.DynamoTablePrefix) name

let private checkItem' env tableName fields =
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res = doesItemExist client (fullTableName env tableName) fields |> Async.Catch

        semaphore.Release() |> ignore

        return
            match res with
            | Choice1Of2 res ->
                res
                |> Result.mapError (fun errors ->
                    logErrors env errors
                    Errors.GetError)
            | Choice2Of2 exn -> raise exn
    }

let private getItemProjection' env projection tableName reader fields =
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res =
            getItemProjection projection client (fullTableName env tableName) reader fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return
            match res with
            | Choice1Of2 res ->
                match res with
                | Ok entity -> Ok entity
                | Error [ ItemDoesNotExist ] -> Error Errors.ItemDoesNotExist
                | Error list ->
                    logErrors env list
                    Error Errors.GetError
            | Choice2Of2 exn -> raise exn
    }

let private getItem' env tableName reader =
    getItemProjection' env [] tableName reader

let private query' env tableName projection reader kce =
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res =
            queryProjection client projection (fullTableName env tableName) reader kce
            |> Async.Catch

        semaphore.Release() |> ignore

        return
            match res with
            | Choice1Of2 res ->
                match res with
                | Ok list -> Ok list
                | Error list ->
                    logErrors env list
                    Error Errors.QueryError
            | Choice2Of2 exn -> raise exn
    }

let private putItem' env version tableName fields =
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let ce =
            match version with
            | None -> None
            | Some v when v = 0 -> None
            | Some v ->
                Query.KeyConditionExpression(Query.NumberEquals("Version", decimal v), [])
                |> Some

        let! res =
            putItemWithCondition ce client (fullTableName env tableName) fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return
            match res with
            | Choice1Of2 res ->
                match res with
                | Ok() -> Ok Success
                | Error [ ConditionalCheckFailed ] -> Ok Retry
                | Error list ->
                    logErrors env list
                    Error Errors.PutError
            | Choice2Of2 exn -> raise exn
    }

let private deleteItem' env tableName fields =
    async {
        do! semaphore.WaitAsync() |> Async.AwaitTask
        let! res = deleteItem client (fullTableName env tableName) fields |> Async.Catch

        semaphore.Release() |> ignore

        return
            match res with
            | Choice1Of2 res ->
                res
                |> Result.mapError (fun errors ->
                    logErrors env errors
                    Errors.DeleteError)
            | Choice2Of2 exn -> raise exn
    }

let private toInt32Map (map: Map<string, Model.AttributeValue>) =
    map
    |> Map.toSeq
    |> Seq.map (fun (key, value) -> ((Int32.Parse key), (Int32.Parse value.N)))
    |> Map.ofSeq
    |> Ok

let private fromInt32Map (map: Map<int, int>) =
    map
    |> Map.toList
    |> List.map (fun (key, value) -> Attr(key.ToString(), ScalarInt32 value))

let private toInt32List (set: Set<string>) =
    set |> Set.toList |> List.map Int32.Parse |> Ok

let private optInt32 fieldName v =
    match v with
    | Some id -> [ Attr(fieldName, ScalarInt32 id) ]
    | _ -> []

type SysItem =
    { Id: string
      Map: Map<string, string>
      Version: int }

    member x.LastId =
        x.Map.TryFind "LastId" |> Option.map Int32.Parse |> Option.defaultValue 0

    static member NewItem key =
        { Id = key
          Map = Map.empty
          Version = 0 }

module SysItem =

    let private tableName = "System"

    module private Fields =
        let Id = "Id"
        let Map = "Map"
        let Version = "Version"

    let private key id = [ Attr(Fields.Id, ScalarString id) ]

    let private fullId itemKey = "d2-" + itemKey

    let private builder id map version : SysItem =
        { Id = id
          Map = map
          Version = version }

    let private reader =
        builder <!> (req Fields.Id A.string)
        <*> (req Fields.Map A.docMap >- (Map.map (fun _ -> A.string)))
        <*> (req Fields.Version A.number >-> P.int)

    let get env = key >> getItem' env tableName reader

    let put env (item: SysItem) =
        [ Attr(Fields.Id, ScalarString item.Id)
          Attr(Fields.Map, DocMap(item.Map |> Map.toList |> List.map (fun (k, v) -> Attr(k, ScalarString v))))
          Attr(Fields.Version, ScalarInt32(item.Version + 1)) ]
        |> putItem' env (Some item.Version) tableName

    let delete env itemKey =
        key (fullId itemKey) |> deleteItem' env tableName

    let private setLastId id (item: SysItem) =
        { item with
            Map = item.Map.Add("LastId", id.ToString()) }

    let private incLastId (item: SysItem) =
        { item with
            Map = item.Map.Add("LastId", (item.LastId + 1).ToString()) }

    let getNextId env itemKey (startIdProvider: unit -> int) =
        let itemId = fullId itemKey

        async {
            match! putOptimistic' env get put itemId (incLastId >> Ok) with
            | Ok item -> return Ok(item.LastId)
            | Error Errors.ItemDoesNotExist ->
                let item = SysItem.NewItem(itemId) |> setLastId (startIdProvider ()) |> incLastId
                let! x = put env item
                return x |> Result.map (fun _ -> item.LastId)
            | Error txt -> return Error txt
        }

module RefreshTokens =
    let private tableName = "Tokens"

    module private Fields =
        let Token = "Token"
        let Expired = "TTL"

    let private key value =
        [ Attr(Fields.Token, ScalarString value) ]

    let private builder value expired : Domain.RefreshToken =
        { Value = value
          Expired = fromEpoch expired }

    let private reader =
        builder <!> (req Fields.Token A.string)
        <*> (req Fields.Expired A.number >-> P.decimal)

    let get env = key >> getItem' env tableName reader

    let put env (token: Domain.RefreshToken) =
        [ Attr(Fields.Token, ScalarString token.Value)
          Attr(Fields.Expired, ScalarDecimal(token.Expired |> toEpoch)) ]
        |> putItem' env None tableName

    let replace env (oldToken: Domain.RefreshToken) (newToken: Domain.RefreshToken) =
        deleteItem' env tableName (key oldToken.Value)
        |> AsyncResult.next (put env newToken)

module Experts =
    let private tableName = "Experts"

    module private Fields =
        let Id = "Id"
        let Username = "Username"
        let Name = "Name"
        let IsProducer = "IsProducer"
        let Competitions = "Competitions"
        let Quizzes = "Quizzes"
        let Packages = "Packages"
        let PackagesSharedWithMe = "PackagesSWM"
        let DefaultImg = "DefaultImg"
        let DefaultMixlr = "DefaultMixlr"
        let Version = "Version"

    let private key sub = [ Attr(Fields.Id, ScalarString sub) ]

    let private builder
        id
        username
        name
        isProducer
        competitions
        quizzes
        packages
        packagesSharedWithMe
        defaultImg
        defaultMixlr
        version
        : Domain.Expert =
        { Id = id
          Username = username |> Option.defaultValue ""
          Name = name |> Option.defaultValue ""
          IsProducer = isProducer |> Option.defaultValue false
          Competitions = competitions |> Option.defaultValue Map.empty
          Quizzes = quizzes |> Option.defaultValue []
          Packages = packages |> Option.defaultValue []
          PackagesSharedWithMe = packagesSharedWithMe |> Option.defaultValue []
          DefaultImg = defaultImg |> Option.defaultValue ""
          DefaultMixlr = defaultMixlr |> Option.defaultValue None
          Version = version }

    let private reader =
        builder <!> (req Fields.Id A.string)
        <*> (opt Fields.Username A.string)
        <*> (opt Fields.Name A.string)
        <*> (opt Fields.IsProducer A.bool)
        <*> (opt Fields.Competitions A.docMap ?>-> toInt32Map)
        <*> (opt Fields.Quizzes A.setString ?>-> toInt32List)
        <*> (opt Fields.Packages A.setString ?>-> toInt32List)
        <*> (opt Fields.PackagesSharedWithMe A.setString ?>-> toInt32List)
        <*> (opt Fields.DefaultImg A.string)
        <*> (opt Fields.DefaultMixlr (A.nullOr A.number)
             ?>-> (fun id -> id |> Option.map Int32.Parse |> Ok))
        <*> (req Fields.Version A.number >-> P.int)

    let get env = key >> getItem' env tableName reader

    let provider env =
        get env >> Async.RunSynchronously >> Result.toOption

    let private put env (exp: Domain.Expert) =
        [ Attr(Fields.Id, ScalarString exp.Id)
          Attr(Fields.Username, ScalarString exp.Username)
          Attr(Fields.Name, ScalarString exp.Name)
          Attr(Fields.IsProducer, ScalarBool exp.IsProducer)
          Attr(Fields.Competitions, DocMap(fromInt32Map exp.Competitions))
          yield! BuildAttr.setString Fields.Quizzes (exp.Quizzes |> List.map string)
          yield! BuildAttr.setString Fields.Packages (exp.Packages |> List.map string)
          yield! BuildAttr.setString Fields.PackagesSharedWithMe (exp.PackagesSharedWithMe |> List.map string)
          Attr(Fields.DefaultImg, ScalarString exp.DefaultImg)
          yield! optInt32 Fields.DefaultMixlr exp.DefaultMixlr
          Attr(Fields.Version, ScalarInt32(exp.Version + 1)) ]
        |> putItem' env (Some exp.Version) tableName

    let update env id logic = putOptimistic' env get put id logic

    let updateOrCreate env id logic creator =

        async {
            do!
                async {
                    match! checkItem' env tableName (key id) with
                    | Ok false -> return! put env <| creator () |> Async.Ignore
                    | _ -> return ()
                }

            return! putOptimistic' env get put id logic
        }

module private Slips =

    module private Fields =
        let Kind = "Kind"
        let Questions = "Questions"
        let Text = "Text"
        let MediaKey = "ImgKey"
        let MediaType = "MediaType"
        let Answer = "Answer"
        let Answers = "Answers"
        let AnswerText = "Text"
        let AnswerIsCorrect = "IsCorrect"
        let Comment = "Comment"
        let AnswerMediaKey = "CommentImgKey"
        let AnswerMediaType = "AnswerMediaType"
        let Points = "Points"
        let JpdPoints = "JpdPoints"
        let Choiсe = "Choiсe"
        let Name = "Name"
        let Items = "Items"
        let Caption = "Caption"
        let Seconds = "Seconds"
        let EndOfTour = "EOT"

        [<Literal>]
        let KindMultiple = "Multiple"

        [<Literal>]
        let KindWWW = "WWW"

        [<Literal>]
        let KindSingle = "Single"

    let private mediaDscBuilder key mediaType : Domain.MediaDsc = { Key = key; Type = mediaType }

    let readMediaDsc keyField typeField =
        choice keyField A.string (function
            | Some key when key <> "" ->
                fun m ->
                    let mediaType =
                        m
                        |> Map.tryFind typeField
                        |> Option.map A.string
                        |> Option.bind P.enum<Domain.MediaType>
                        |> Option.defaultValue Domain.MediaType.Picture

                    Some(mediaDscBuilder key mediaType) |> Ok
            | _ -> (fun _ -> Ok None))

    let private questionsInSlipReader =
        function
        | Some _ ->
            AttrReader.run (req Fields.Questions A.docList @>- A.string)
            >> Result.map (function
                | [ qw ] -> Domain.Solid qw
                | list -> Domain.Split list)
        | None -> AttrReader.run (optDef Fields.Text "" A.string) >> Result.map Domain.Solid

    let private choiceAnswerBuilder text isCorrect : Domain.ChoiceAnswer = { Text = text; IsCorrect = isCorrect }

    let choiceAnswerReader =
        choiceAnswerBuilder <!> (optDef Fields.AnswerText "" A.string)
        <*> (optDef Fields.AnswerIsCorrect false A.bool)

    let private answersInSlipReader =
        function
        | Some _ ->
            AttrReader.run (req Fields.Answers A.docList @>-> (A.docMap, AttrReader.run choiceAnswerReader))
            >> Result.map Domain.ChoiceAnswer
        | None ->
            AttrReader.run (optDef Fields.Answer "" A.string)
            >> Result.map Domain.OpenAnswer

    let private singleSlipBuilder
        cap
        question
        questionMedia
        answer
        answerMedia
        comment
        points
        jeopardyPoints
        withChoice
        seconds
        eot
        : Domain.SingleSlip =
        { Caption = cap
          Question = question
          QuestionMedia = questionMedia
          Answer = answer
          AnswerMedia = answerMedia
          Comment = comment
          Points = points
          JeopardyPoints = jeopardyPoints
          WithChoice = withChoice
          Seconds = seconds
          EndOfTour = eot }

    let singleSlipReader =
        singleSlipBuilder <!> (optDef Fields.Caption "" A.string)
        <*> (choice Fields.Questions A.docList questionsInSlipReader)
        <*> (readMediaDsc Fields.MediaKey Fields.MediaType)
        <*> (choice Fields.Answers A.docList answersInSlipReader)
        <*> (readMediaDsc Fields.AnswerMediaKey Fields.AnswerMediaType)
        <*> (optDef Fields.Comment "" A.string)
        <*> (optDef Fields.Points "1" A.number >-> P.decimal)
        <*> (opt Fields.JpdPoints (A.nullOr A.number) ??>-> P.decimal)
        <*> (optDef Fields.Choiсe false A.bool)
        <*> (opt Fields.Seconds (A.nullOr A.number) ??>-> P.int)
        <*> (optDef Fields.EndOfTour false A.bool)

    let private multipleSlipBuilder name slips = Domain.Multiple(name, slips)

    let private multipleSlipReader =
        multipleSlipBuilder <!> (optDef Fields.Name "" A.string)
        <*> (req Fields.Items A.docList @>-> (A.docMap, AttrReader.run singleSlipReader))

    let private slipReader =
        function
        | Some Fields.KindMultiple -> AttrReader.run multipleSlipReader
        | Some Fields.KindWWW
        | Some Fields.KindSingle
        | _ -> (AttrReader.run singleSlipReader) >> Result.map Domain.Single

    let reader = AttrReader.run (choice Fields.Kind A.string slipReader)

    let fieldsOfSingleSlip (slip: Domain.SingleSlip) =
        [ if slip.Caption <> "" then
              yield! BuildAttr.string Fields.Caption slip.Caption
          Attr(Fields.Kind, ScalarString Fields.KindSingle)
          match slip.Question with
          | Domain.Solid qw -> yield! BuildAttr.string Fields.Text qw
          | Domain.Split list -> Attr(Fields.Questions, DocList(list |> List.map ScalarString))
          match slip.QuestionMedia with
          | Some media ->
              yield! BuildAttr.string Fields.MediaKey media.Key

              if media.Type <> Domain.MediaType.Picture then
                  Attr(Fields.MediaType, ScalarString(media.Type.ToString()))
          | None -> ()
          match slip.Answer with
          | Domain.OpenAnswer aw -> yield! BuildAttr.string Fields.Answer aw
          | Domain.ChoiceAnswer(list) ->
              Attr(
                  Fields.Answers,
                  DocList(
                      list
                      |> List.map (fun aw ->
                          DocMap
                              [ Attr(Fields.AnswerText, ScalarString aw.Text)
                                Attr(Fields.AnswerIsCorrect, ScalarBool aw.IsCorrect) ])
                  )
              )
          match slip.AnswerMedia with
          | Some media ->
              yield! BuildAttr.string Fields.AnswerMediaKey media.Key

              if media.Type <> Domain.MediaType.Picture then
                  Attr(Fields.AnswerMediaType, ScalarString(media.Type.ToString()))
          | None -> ()
          yield! BuildAttr.string Fields.Comment slip.Comment
          Attr(Fields.Points, ScalarDecimal slip.Points)
          yield! BuildAttr.optional Fields.JpdPoints ScalarDecimal slip.JeopardyPoints
          if slip.WithChoice then
              Attr(Fields.Choiсe, ScalarBool slip.WithChoice)
          yield! BuildAttr.optional Fields.Seconds ScalarInt32 slip.Seconds
          if slip.EndOfTour then
              Attr(Fields.EndOfTour, ScalarBool slip.EndOfTour) ]

    let fieldsOfMultipleSlip (name: string) (slips: Domain.SingleSlip list) =
        [ Attr(Fields.Kind, ScalarString Fields.KindMultiple)
          if name <> "" then
              Attr(Fields.Name, ScalarString name)
          Attr(Fields.Items, DocList(slips |> List.map (fieldsOfSingleSlip >> DocMap))) ]

    let fields (slip: Domain.Slip) =
        match slip with
        | Domain.Single s -> fieldsOfSingleSlip s
        | Domain.Multiple(name, slips) -> fieldsOfMultipleSlip name slips

module Packages =
    let private tableName = "Packages"

    module private Fields =
        let Id = "Id"
        let Producer = "Producer"
        let Name = "Name"
        let TransferToken = "TransferToken"
        let SharedWith = "SharedWith"
        let Questions = "Questions"
        let Version = "Version"

    let private key id = [ Attr(Fields.Id, ScalarInt32 id) ]

    let private dscFields = [ Fields.Id; Fields.Producer; Fields.Name ]

    let private dscBuilder id producer name : Domain.PackageDescriptor =
        { PackageId = id
          Producer = producer
          Name = name }

    let private dscReader =
        dscBuilder <!> (req Fields.Id A.number >-> P.int)
        <*> (req Fields.Producer A.string)
        <*> (req Fields.Name A.string)

    let getDescriptor env =
        key >> getItemProjection' env dscFields tableName dscReader

    let private builder dsc transferToken sharedWith slips version : Domain.Package =
        { Dsc = dsc
          TransferToken = transferToken
          SharedWith = sharedWith |> Set.toList
          Slips = slips
          Version = version }

    let private reader =
        builder <!> dscReader
        <*> (optDef Fields.TransferToken "" A.string)
        <*> (optDef Fields.SharedWith Set.empty A.setString)
        <*> (req Fields.Questions A.docList @>-> (A.docMap, Slips.reader))
        <*> (req Fields.Version A.number >-> P.int)

    let get env = key >> getItem' env tableName reader

    let provider env =
        get env >> Async.RunSynchronously >> Result.toOption

    let delete env = key >> deleteItem' env tableName

    let private put env (pkg: Domain.Package) =
        [ Attr(Fields.Id, ScalarInt32 pkg.Dsc.PackageId)
          Attr(Fields.Producer, ScalarString pkg.Dsc.Producer)
          Attr(Fields.Name, ScalarString pkg.Dsc.Name)
          Attr(Fields.TransferToken, ScalarString pkg.TransferToken)
          yield! BuildAttr.setString Fields.SharedWith (pkg.SharedWith |> List.map string)
          Attr(Fields.Questions, DocList(pkg.Slips |> List.map (Slips.fields >> DocMap)))
          Attr(Fields.Version, ScalarInt32(pkg.Version + 1)) ]
        |> putItem' env (Some pkg.Version) tableName

    let create env (creator: int -> Domain.Package) =
        SysItem.getNextId env "pkg" (fun () -> 0)
        |> AsyncResult.bind (creator >> AsyncResult.retn)
        |> AsyncResult.side (put env)

    let update env id logic = putOptimistic' env get put id logic

module Quizzes =
    let private tableName = "Quizzes"

    module private Fields =
        let Id = "Id"
        let Producer = "Producer"
        let Name = "Name"
        let StartTime = "StartTime"
        let Status = "Status"
        let WelcomeText = "WelcomeText"
        let FarewellText = "FarewellText"
        let RegText = "RegText"
        let InfoText = "InfoText"
        let ImgKey = "ImgKey"
        let WithPremoderation = "WithPremoderation"
        let AdminToken = "AdminToken"
        let RegToken = "RegToken"
        let ListenToken = "ListenToken"
        let ResultsToken = "ResultsToken"
        let PkgId = "PkgId"
        let PkgQwIdx = "PkgQwIdx"
        let EventPage = "EventPage"
        let MixlrCode = "MixlrCode"
        let Questions = "Questions"
        let TourName = "Name"
        let TourSeconds = "Seconds"
        let TourStatus = "Status"
        let TourQwIdx = "QwIdx"
        let TourQwPartIdx = "QwPartIdx"
        let TourIsMediaDisplayed = "IsMediaDisplayed"
        let TourIsQuestionDisplayed = "IsQuestionDisplayed"
        let TourStartTime = "StartTime"
        let TourSlip = "Slip"
        let Version = "Version"
        let StreamUrl = "StreamUrl"

    let private key id = [ Attr(Fields.Id, ScalarInt32 id) ]

    let private dscFields =
        [ Fields.Id
          Fields.Producer
          Fields.StartTime
          Fields.Name
          Fields.Status
          Fields.WelcomeText
          Fields.FarewellText
          Fields.RegText
          Fields.InfoText
          Fields.ImgKey
          Fields.WithPremoderation
          Fields.AdminToken
          Fields.RegToken
          Fields.ListenToken
          Fields.ResultsToken
          Fields.PkgId
          Fields.PkgQwIdx
          Fields.EventPage
          Fields.MixlrCode
          Fields.StreamUrl ]

    let private dscBuilder
        id
        producer
        startTime
        name
        status
        wlcmText
        frwlText
        regText
        infoText
        imgKey
        withPremoderation
        adminToken
        regToken
        listenToken
        resultsToken
        pkgId
        pkgQwIdx
        evtPage
        mixlrCode
        streamUrl
        : Domain.QuizDescriptor =
        { QuizId = id
          Producer = producer
          StartTime = startTime
          Name = name
          Status = status |> Option.defaultValue Domain.Setup
          WelcomeText = wlcmText
          FarewellText = frwlText
          RegText = regText
          InfoText = infoText
          ImgKey = imgKey
          WithPremoderation = withPremoderation
          AdminToken = adminToken
          RegToken = regToken
          ListenToken = listenToken
          ResultsToken = resultsToken
          PkgId = pkgId
          PkgSlipIdx = pkgQwIdx
          EventPage = evtPage
          MixlrCode = mixlrCode
          StreamUrl = streamUrl }

    let private dscReader =
        dscBuilder <!> (req Fields.Id A.number >-> P.int)
        <*> (req Fields.Producer A.string)
        <*> (opt Fields.StartTime (A.nullOr A.string) ??>-> P.dateTime)
        <*> (req Fields.Name A.string)
        <*> (req Fields.Status A.string >- P.enum<Domain.QuizStatus>)
        <*> (optDef Fields.WelcomeText "" A.string)
        <*> (optDef Fields.FarewellText "" A.string)
        <*> (optDef Fields.RegText "" A.string)
        <*> (optDef Fields.InfoText "" A.string)
        <*> (optDef Fields.ImgKey "" A.string)
        <*> (optDef Fields.WithPremoderation false A.bool)
        <*> (optDef Fields.AdminToken "" A.string)
        <*> (optDef Fields.RegToken "" A.string)
        <*> (optDef Fields.ListenToken "" A.string)
        <*> (optFirstDef [ Fields.ResultsToken; Fields.ListenToken ] "" A.string)
        <*> (opt Fields.PkgId (A.nullOr A.number) ??>-> P.int)
        <*> (opt Fields.PkgQwIdx (A.nullOr A.number) ??>-> P.int)
        <*> (optDef Fields.EventPage "" A.string)
        <*> (opt Fields.MixlrCode (A.nullOr A.number) ??>-> P.int)
        <*> (opt Fields.StreamUrl (A.nullOr A.string) ??>-> Ok)

    let getDescriptor env =
        key >> getItemProjection' env dscFields tableName dscReader

    let private tourBuilder
        name
        seconds
        status
        qwIdx
        qwPartIdx
        isMediaDisplayed
        isQuestionDisplayed
        startTime
        slip
        : Domain.QuizTour =
        { Name = name
          Seconds = seconds
          Status = status |> Option.defaultValue Domain.Announcing
          QwIdx = qwIdx
          QwPartIdx = qwPartIdx
          IsMediaDisplayed = isMediaDisplayed
          IsQuestionDisplayed = isQuestionDisplayed
          StartTime = startTime
          Slip = slip }

    let private slipChoice =
        function
        | Some map -> (fun _ -> Slips.reader map)
        | None -> (AttrReader.run Slips.singleSlipReader) >> Result.map Domain.Single

    let private tourReader =
        tourBuilder <!> (optDef Fields.TourName "" A.string)
        <*> (req Fields.TourSeconds A.number >-> P.int)
        <*> (req Fields.TourStatus A.string >- P.enum<Domain.QuizTourStatus>)
        <*> (optDef Fields.TourQwIdx "0" A.number >-> P.int)
        <*> (optDef Fields.TourQwPartIdx "0" A.number >-> P.int)
        <*> (optDef Fields.TourIsMediaDisplayed false A.bool)
        <*> (optDef Fields.TourIsQuestionDisplayed false A.bool)
        <*> (opt Fields.TourStartTime (A.nullOr A.string) ??>-> P.dateTime)
        <*> (choice Fields.TourSlip A.docMap slipChoice)

    let private builder dsc tours version : Domain.Quiz =
        { Dsc = dsc
          Tours = tours
          Version = version }

    let private reader =
        builder <!> dscReader
        <*> (req Fields.Questions A.docList @>-> (A.docMap, AttrReader.run tourReader))
        <*> (req Fields.Version A.number >-> P.int)

    let sysKey quizId = (sprintf "quiz-%i" quizId)

    let get env = key >> getItem' env tableName reader

    let private put env (item: Domain.Quiz) =
        [ Attr(Fields.Id, ScalarInt32 item.Dsc.QuizId)
          Attr(Fields.Producer, ScalarString item.Dsc.Producer)
          yield! BuildAttr.optional Fields.StartTime ScalarDate item.Dsc.StartTime
          Attr(Fields.Name, ScalarString item.Dsc.Name)
          Attr(Fields.Status, ScalarString(item.Dsc.Status.ToString()))
          yield! BuildAttr.string Fields.WelcomeText item.Dsc.WelcomeText
          yield! BuildAttr.string Fields.FarewellText item.Dsc.FarewellText
          yield! BuildAttr.string Fields.RegText item.Dsc.RegText
          yield! BuildAttr.string Fields.InfoText item.Dsc.InfoText
          yield! BuildAttr.string Fields.ImgKey item.Dsc.ImgKey
          Attr(Fields.WithPremoderation, ScalarBool item.Dsc.WithPremoderation)
          yield! BuildAttr.string Fields.AdminToken item.Dsc.AdminToken
          yield! BuildAttr.string Fields.RegToken item.Dsc.RegToken
          yield! BuildAttr.string Fields.ListenToken item.Dsc.ListenToken
          yield! BuildAttr.string Fields.ResultsToken item.Dsc.ResultsToken
          yield! BuildAttr.optional Fields.PkgId ScalarInt32 item.Dsc.PkgId
          yield! BuildAttr.optional Fields.PkgQwIdx ScalarInt32 item.Dsc.PkgSlipIdx
          yield! BuildAttr.string Fields.EventPage item.Dsc.EventPage
          yield! BuildAttr.optional Fields.MixlrCode ScalarInt32 item.Dsc.MixlrCode
          yield! BuildAttr.optional Fields.StreamUrl ScalarString item.Dsc.StreamUrl

          Attr(
              Fields.Questions,
              DocList
                  [ for tour in item.Tours do
                        yield
                            DocMap
                                [ yield! BuildAttr.string Fields.TourName tour.Name
                                  Attr(Fields.TourSeconds, ScalarInt32 tour.Seconds)
                                  Attr(Fields.TourStatus, ScalarString(tour.Status.ToString()))
                                  Attr(Fields.TourSlip, DocMap(Slips.fields tour.Slip))
                                  yield! BuildAttr.optional Fields.TourStartTime ScalarDate tour.StartTime
                                  Attr(Fields.TourQwIdx, ScalarInt32 tour.QwIdx)
                                  Attr(Fields.TourQwPartIdx, ScalarInt32 tour.QwPartIdx)
                                  if tour.IsQuestionDisplayed then
                                      Attr(Fields.TourIsQuestionDisplayed, ScalarBool tour.IsQuestionDisplayed)
                                  if tour.IsMediaDisplayed then
                                      Attr(Fields.TourIsMediaDisplayed, ScalarBool tour.IsMediaDisplayed) ] ]
          )

          Attr(Fields.Version, ScalarInt32(item.Version + 1)) ]
        |> putItem' env (Some item.Version) tableName

    let create env (creator: int -> Domain.Quiz) =
        SysItem.getNextId env "quizzes" (fun () -> 0)
        |> AsyncResult.bind (creator >> AsyncResult.retn)
        |> AsyncResult.side (put env)

    let mutable private _subscriptions: (Domain.Quiz -> Async<unit>) list = []

    let subscribe handler =
        _subscriptions <- handler :: _subscriptions

    let update env quizId logic =
        putOptimistic' env get put quizId logic
        |> AsyncResult.side (fun quiz ->
            _subscriptions
            |> List.map (fun handler -> handler quiz)
            |> Async.Sequential
            |> Async.Catch
            |> Async.map Ok)

    let delete env quizId =
        deleteItem' env tableName (key quizId)
        |> AsyncResult.next (SysItem.delete env (sysKey quizId))

module Teams =
    let private tableName = "Teams"

    module private Fields =
        let QuizId = "QuizId"
        let TeamId = "TeamId"
        let Name = "Name"
        let Status = "Status"
        let EntryToken = "EntryToken"
        let RegistrationDate = "RegistrationDate"
        let ActiveSessionId = "ActiveSessionId"
        let Answers = "Answers"
        let AnswerText = "Text"
        let AnswerJeopardy = "Jpd"
        let AnswerRecieveTime = "RecieveTime"
        let AnswerResult = "Result"
        let AnswerVote = "Vote"
        let AnswerIsAutoResult = "IsAutoResult"
        let AnswerUpdateTime = "UpdateTime"
        let Version = "Version"

    let private key quizId teamId =
        [ Attr(Fields.QuizId, ScalarInt32 quizId)
          Attr(Fields.TeamId, ScalarInt32 teamId) ]

    let private queryKeyConditions quizId =
        Query.KeyConditionExpression(Query.NumberEquals(Fields.QuizId, decimal quizId), [])

    let private queryRangeConditions from to' quizId =
        Query.KeyConditionExpression(
            Query.NumberEquals(Fields.QuizId, decimal quizId),
            [ (Query.And, Query.KeyConditionExpression(Query.NumberBetwixt(Fields.TeamId, from, to'), [])) ]
        )

    let private dscFields =
        [ Fields.QuizId
          Fields.TeamId
          Fields.Name
          Fields.Status
          Fields.EntryToken
          Fields.RegistrationDate
          Fields.ActiveSessionId ]

    let private dscBuilder quizId teamId name status entryToken regDate activeSessionId : Domain.TeamDescriptor =
        { QuizId = quizId
          TeamId = teamId
          Name = name
          Status = status |> Option.defaultValue Domain.TeamStatus.New
          EntryToken = entryToken
          RegistrationDate = regDate
          ActiveSessionId = activeSessionId }

    let private dscReader =
        dscBuilder <!> (req Fields.QuizId A.number >-> P.int)
        <*> (req Fields.TeamId A.number >-> P.int)
        <*> (optDef Fields.Name "" A.string)
        <*> (req Fields.Status A.string >- P.enum<Domain.TeamStatus>)
        <*> (optDef Fields.EntryToken "" A.string)
        <*> (req Fields.RegistrationDate A.string >-> P.dateTime)
        <*> (req Fields.ActiveSessionId A.number >-> P.int)

    let getDescriptor env quizId teamId =
        key quizId teamId |> getItemProjection' env dscFields tableName dscReader

    let getIds env (quizId: int) =
        let kce = queryKeyConditions quizId
        let reader = (req Fields.TeamId A.number >-> P.int)
        query' env tableName [ Fields.TeamId ] reader kce

    let getDescriptors env (quizId: int) =
        let kce = queryKeyConditions quizId
        query' env tableName dscFields dscReader kce

    let private teamAnswerBuilder text jpd recieveTime result vote isAutoResult updateTime : Domain.TeamAnswer =
        { Text = text
          Jeopardy = jpd
          RecieveTime = recieveTime
          Vote = vote
          Result = result
          IsAutoResult = isAutoResult
          UpdateTime = updateTime }

    let private teamAnswerReader =
        teamAnswerBuilder <!> (optDef Fields.AnswerText "" A.string)
        <*> (optDef Fields.AnswerJeopardy false A.bool)
        <*> (req Fields.AnswerRecieveTime A.string >-> P.dateTime)
        <*> (opt Fields.AnswerResult (A.nullOr A.number) ??>-> P.decimal)
        <*> (opt Fields.AnswerVote A.bool)
        <*> (optDef Fields.AnswerIsAutoResult false A.bool)
        <*> (opt Fields.AnswerUpdateTime (A.nullOr A.string) ??>-> P.dateTime)

    let private teamBuilder dsc answers version : Domain.Team =
        { Dsc = dsc
          Answers = answers
          Version = version }

    let private qwKeyOfString (str: string) =
        match str.StartsWith "qw" with
        | true ->
            let indexes = (str.Substring(2)).Split '.'

            { TourIdx = indexes |> Array.tryItem 0 |> Option.bind tryParseInt32 |> Option.defaultValue 0
              QwIdx = indexes |> Array.tryItem 1 |> Option.bind tryParseInt32 |> Option.defaultValue 0 }
            : Domain.QwKey
        | false ->
            { TourIdx = str |> tryParseInt32 |> Option.defaultValue 0
              QwIdx = 0 }
        |> Ok

    let private stringOfQwKey (key: Domain.QwKey) : string = sprintf "qw%i.%i" key.TourIdx key.QwIdx

    let private reader =
        teamBuilder <!> dscReader
        <*> (map Fields.Answers qwKeyOfString (A.docMap >> (AttrReader.run teamAnswerReader)))
        <*> (req Fields.Version A.number >-> P.int)

    let get env (k: Domain.TeamKey) =
        getItem' env tableName reader (key k.QuizId k.TeamId)

    let getAllInQuiz env (quizId: int) =
        let kce = queryKeyConditions quizId
        query' env tableName [] reader kce

    let getRangeInQuiz env (from: int) (to': int) (quizId: int) =
        let kce = queryRangeConditions (decimal from) (decimal to') quizId
        query' env tableName [] reader kce

    let private put env (item: Domain.Team) =
        [ Attr(Fields.QuizId, ScalarInt32 item.Dsc.QuizId)
          Attr(Fields.TeamId, ScalarInt32 item.Dsc.TeamId)
          Attr(Fields.Name, ScalarString item.Dsc.Name)
          Attr(Fields.Status, ScalarString(item.Dsc.Status.ToString()))
          Attr(Fields.RegistrationDate, ScalarDate item.Dsc.RegistrationDate)
          Attr(Fields.EntryToken, ScalarString item.Dsc.EntryToken)
          Attr(Fields.ActiveSessionId, ScalarInt32 item.Dsc.ActiveSessionId)
          Attr(
              Fields.Answers,
              DocMap
                  [ for pair in item.Answers do
                        Attr(
                            (stringOfQwKey pair.Key),
                            DocMap[Attr(Fields.AnswerText, ScalarString pair.Value.Text)
                                   Attr(Fields.AnswerJeopardy, ScalarBool pair.Value.Jeopardy)
                                   Attr(Fields.AnswerRecieveTime, ScalarDate pair.Value.RecieveTime)

                                   match pair.Value.Result with
                                   | Some res -> Attr(Fields.AnswerResult, ScalarDecimal res)
                                   | None -> Attr(Fields.AnswerResult, ScalarNull)

                                   match pair.Value.Vote with
                                   | Some v -> Attr(Fields.AnswerVote, ScalarBool v)
                                   | None -> ()

                                   Attr(Fields.AnswerIsAutoResult, ScalarBool pair.Value.IsAutoResult)

                                   match pair.Value.UpdateTime with
                                   | Some v -> Attr(Fields.AnswerUpdateTime, ScalarDate v)
                                   | None -> Attr(Fields.AnswerUpdateTime, ScalarNull)]
                        ) ]
          )
          Attr(Fields.Version, ScalarInt32(item.Version + 1)) ]
        |> putItem' env (Some item.Version) tableName

    let create env quizId creator =
        let lastIdProvider () =
            match getIds env quizId |> Async.RunSynchronously with
            | Ok(head :: tail) -> List.max (head :: tail)
            | _ -> 0

        SysItem.getNextId env (Quizzes.sysKey quizId) lastIdProvider
        |> AsyncResult.bind (creator >> AsyncResult.fromResult)
        |> AsyncResult.side (put env)

    let check env quizId teamId =
        key quizId teamId |> checkItem' env tableName

    let update env id logic = putOptimistic' env get put id logic

    let delete env quizId teamId =
        key quizId teamId |> deleteItem' env tableName