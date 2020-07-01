module Data2

open System
open System.Threading
open Amazon.DynamoDBv2
open DynamoDb.Ok
open DynamoDb.Ok.Read
open DynamoDb.Ok.Write
open Serilog

open Common

module A = Attribute
module P = Parse
module R = AttrReader

type PutResult<'item> = Success | Retry

module Errors =
    let [<Literal>] TransactionFailed = "Transaction Failed"
    let [<Literal>] GetError = "DynamoDB Get Error"
    let [<Literal>] PutError = "DynamoDB Put Error"
    let [<Literal>] QueryError = "DynamoDB Query Error"
    let [<Literal>] DeleteError = "DynamoDB Delete Error"
    let [<Literal>] ItemDoesNotExist = "ItemDoesNotExist"

let private logErrors list =
    Log.Logger.Error ("{@Proc} {@Details}", "DB", list)

let private putOptimistic' get put id logic  =

    let rec tryUpdate attempt =
        async{
            match! get id with
            | Ok entity ->
                match logic entity with
                | Ok updatedEntity ->
                    match! put updatedEntity with
                    | Ok Success -> return Ok updatedEntity
                    | Ok Retry when attempt < 3 ->
                        logErrors [sprintf "Put Condition Failed for Key %A Attempt %i" id attempt]
                        do! Async.Sleep (attempt * 100)
                        return! tryUpdate (attempt + 1)
                    | Ok Retry ->
                        logErrors [sprintf "Put Condition Failed for Key %A Attempt %i Give Up" id attempt]
                        return Error Errors.TransactionFailed
                    | Error txt -> return Error txt
                | Error txt -> return Error txt
            | Error txt -> return Error txt
        }

    tryUpdate 1

let client = new AmazonDynamoDBClient()

let semaphore = new SemaphoreSlim(512, 512);

let fullTableName name = sprintf "OQ-%s" name

let private checkItem' tableName fields =
    async{
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res =
            doesItemExist client (fullTableName tableName) fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return match res with
               | Choice1Of2 res -> res |> Result.mapError (fun errors -> logErrors errors; Errors.GetError)
               | Choice2Of2 exn -> raise exn
    }

let private getItemProjection' projection tableName reader fields  =
    async{
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res =
            getItemProjection projection client (fullTableName tableName) reader fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return match res with
               | Choice1Of2 res ->
                    match res with
                    | Ok entity -> Ok entity
                    | Error [ItemDoesNotExist] -> Error Errors.ItemDoesNotExist
                    | Error list ->
                        logErrors list
                        Error Errors.GetError
               | Choice2Of2 exn -> raise exn
    }

let private getItem' tableName reader = getItemProjection' [] tableName reader

let private query' tableName projection reader kce =
    async{
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let! res =
            queryProjection client projection (fullTableName tableName) reader kce
            |> Async.Catch

        semaphore.Release() |> ignore

        return match res with
               | Choice1Of2 res ->
                    match res with
                    | Ok list -> Ok list
                    | Error list ->
                        logErrors list
                        Error Errors.QueryError
               | Choice2Of2 exn -> raise exn
    }

let private putItem' version tableName fields  =
    async{
        do! semaphore.WaitAsync() |> Async.AwaitTask

        let ce =
            match version with
            | None -> None
            | Some v when v = 0 -> None
            | Some v -> Query.KeyConditionExpression (Query.NumberEquals ("Version", decimal v), []) |> Some

        let! res =
            putItemWithCondition ce client (fullTableName tableName) fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return match res with
               | Choice1Of2 res ->
                    match res with
                    | Ok () -> Ok Success
                    | Error [ConditionalCheckFailed] -> Ok Retry
                    | Error list ->
                        logErrors list
                        Error Errors.PutError
               | Choice2Of2 exn -> raise exn
    }

let private deleteItem' tableName fields =
    async{
        do! semaphore.WaitAsync() |> Async.AwaitTask
        let! res =
            deleteItem client (fullTableName tableName) fields
            |> Async.Catch

        semaphore.Release() |> ignore

        return match res with
               | Choice1Of2 res -> res |> Result.mapError (fun errors -> logErrors errors; Errors.DeleteError)
               | Choice2Of2 exn -> raise exn
    }

let private toInt32Map (map:Map<string,Model.AttributeValue>) =
    map
    |> Map.toSeq
    |> Seq.map (fun (key,value) -> ((Int32.Parse key), (Int32.Parse value.N)))
    |> Map.ofSeq
    |> Ok

let private fromInt32Map (map:Map<int,int>) =
    map
    |> Map.toList
    |> List.map (fun (key,value) -> Attr (key.ToString(), ScalarInt32 value))

let private toInt32List (set:Set<string>) =
    set
    |> Set.toList
    |> List.map Int32.Parse
    |> Ok

let private optInt32 fieldName v =
    match v with
    | Some id -> [Attr (fieldName, ScalarInt32 id)]
    | _ -> []

type SysItem = {
    Id : string; Map : Map<string,string>; Version : int
} with
    member x.LastId = x.Map.TryFind "LastId" |> Option.map Int32.Parse |> Option.defaultValue -100
    static member NewItem key = {Id = key; Map = Map.empty; Version = 0}

module SysItem =

    let private tableName = "System"

    module private Fields =
        let Id = "Id"
        let Map = "Map"
        let Version = "Version"

    let private key id = [ Attr (Fields.Id, ScalarString id) ]

    let private fullId itemKey = "d2-" + itemKey

    let private builder id map version : SysItem =
        {Id = id; Map = map; Version = version}

    let private reader =
        builder
        <!> (req Fields.Id A.string)
        <*> (req Fields.Map A.docMap >- (Map.map (fun _ -> A.string)))
        <*> (req Fields.Version A.number >-> P.int)

    let get = key >> getItem' tableName reader

    let put (item:SysItem) =
        [Attr (Fields.Id, ScalarString item.Id)
         Attr (Fields.Map, DocMap (item.Map |> Map.toList |> List.map (fun (k,v) -> Attr (k, ScalarString v))))
         Attr (Fields.Version, ScalarInt32 (item.Version + 1))]
        |> putItem' (Some item.Version) tableName

    let delete itemKey =
        key (fullId itemKey)
        |> deleteItem' tableName

    let private setLastId id (item:SysItem) =
        {item with Map = item.Map.Add("LastId", id.ToString())}

    let private incLastId (item:SysItem) =
        {item with Map = item.Map.Add("LastId", (item.LastId + 1).ToString())}

    let getNextId itemKey (startIdProvider: unit -> int) =
        let itemId = fullId itemKey
        async{
            match! putOptimistic' get put itemId (incLastId >> Ok) with
            | Ok item -> return Ok (item.LastId)
            | Error Errors.ItemDoesNotExist ->
                let item =
                    SysItem.NewItem(itemId)
                    |> setLastId (startIdProvider())
                    |> incLastId
                let! x = put item
                return x |> Result.map (fun _ -> item.LastId)
            | Error txt -> return Error txt
        }

module RefreshTokens =
    let private tableName = "Tokens"

    module private Fields =
        let Token = "Token"
        let Expired = "TTL"

    let private key value = [ Attr (Fields.Token, ScalarString value) ]

    let private builder value expired : Domain.RefreshToken=
        {Value = value; Expired = fromEpoch expired}

    let private reader =
        builder
        <!> (req Fields.Token A.string)
        <*> (req Fields.Expired A.number >-> P.decimal)

    let get = key >> getItem' tableName reader

    let put (token:Domain.RefreshToken) =
        [Attr (Fields.Token, ScalarString token.Value)
         Attr (Fields.Expired, ScalarDecimal (token.Expired |> toEpoch))]
        |> putItem' None tableName

    let replace (oldToken:Domain.RefreshToken) (newToken:Domain.RefreshToken) =
        deleteItem' tableName (key oldToken.Value)
        |> AsyncResult.next (put newToken)

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

    let private key sub = [ Attr (Fields.Id, ScalarString sub) ]

    let private builder id username name isProducer competitions quizzes packages packagesSharedWithMe
                            defaultImg defaultMixlr version: Domain.Expert =
        {Id = id
         Username = username |> Option.defaultValue ""
         Name = name |> Option.defaultValue ""
         IsProducer = isProducer |> Option.defaultValue false
         Competitions = competitions|> Option.defaultValue Map.empty
         Quizzes = quizzes |> Option.defaultValue []
         Packages = packages |> Option.defaultValue []
         PackagesSharedWithMe = packagesSharedWithMe |> Option.defaultValue []
         DefaultImg = defaultImg |> Option.defaultValue ""
         DefaultMixlr = defaultMixlr |> Option.defaultValue None
         Version = version}

    let private reader =
        builder
        <!> (req Fields.Id A.string)
        <*> (opt Fields.Username A.string)
        <*> (opt Fields.Name A.string)
        <*> (opt Fields.IsProducer A.bool)
        <*> (opt Fields.Competitions A.docMap ?>-> toInt32Map)
        <*> (opt Fields.Quizzes A.setString ?>-> toInt32List)
        <*> (opt Fields.Packages A.setString ?>-> toInt32List)
        <*> (opt Fields.PackagesSharedWithMe A.setString ?>-> toInt32List)
        <*> (opt Fields.DefaultImg A.string)
        <*> (opt Fields.DefaultMixlr (A.nullOr A.number) ?>-> (fun id -> id |> Option.map Int32.Parse |> Ok))
        <*> (req Fields.Version A.number >-> P.int)

    let get = key >> getItem' tableName reader

    let provider = get >> Async.RunSynchronously >> Result.toOption

    let private put (exp : Domain.Expert) =
        [
            Attr (Fields.Id, ScalarString exp.Id)
            Attr (Fields.Username, ScalarString exp.Username)
            Attr (Fields.Name, ScalarString exp.Name)
            Attr (Fields.IsProducer, ScalarBool exp.IsProducer)
            Attr (Fields.Competitions, DocMap (fromInt32Map exp.Competitions))
            yield! BuildAttr.setString Fields.Quizzes (exp.Quizzes |> List.map string)
            yield! BuildAttr.setString Fields.Packages (exp.Packages |> List.map string)
            yield! BuildAttr.setString Fields.PackagesSharedWithMe (exp.PackagesSharedWithMe |> List.map string)
            Attr (Fields.DefaultImg, ScalarString exp.DefaultImg)
            yield! optInt32 Fields.DefaultMixlr exp.DefaultMixlr
            Attr (Fields.Version, ScalarInt32 (exp.Version + 1))
        ]
        |> putItem' (Some exp.Version) tableName

    let update = putOptimistic' get put

    let updateOrCreate id logic creator =

        async{
            do!
                async{
                    match! checkItem' tableName (key id) with
                    | Ok false -> return! put <| creator() |> Async.Ignore
                    | _ -> return ()
                }

            return! putOptimistic' get put id logic
        }

module private Slips =

    module private Fields =
        let Kind = "Kind"
        let Questions = "Questions"
        let Text = "Text"
        let ImgKey = "ImgKey"
        let Answer = "Answer"
        let Answers = "Answers"
        let AnswerText = "Text"
        let AnswerIsCorrect = "IsCorrect"
        let Comment = "Comment"
        let CommentImgKey = "CommentImgKey"
        let Points = "Points"
        let JpdPoints = "JpdPoints"
        let Choiсe = "Choiсe"
        let Name = "Name"
        let Items = "Items"
        let [<Literal>] KindMultiple = "Multiple"
        let [<Literal>] KindWWW = "WWW"
        let [<Literal>] KindSingle = "Single"

    let private questionsInSlipReader =
        function
        | Some _ ->
            AttrReader.run (req Fields.Questions A.docList @>- A.string)
            >> Result.map (function [qw] -> Domain.Solid qw | list -> Domain.Split list)
        | None ->
            AttrReader.run (optDef Fields.Text "" A.string)
            >> Result.map Domain.Solid

    let private choiceAnswerBuilder text isCorrect : Domain.ChoiceAnswer =
        {Text = text; IsCorrect = isCorrect}

    let choiceAnswerReader =
        choiceAnswerBuilder
        <!> (optDef Fields.AnswerText "" A.string)
        <*> (optDef Fields.AnswerIsCorrect false A.bool)

    let private answersInSlipReader =
        function
        | Some _ ->
            AttrReader.run (req Fields.Answers A.docList @>-> (A.docMap, AttrReader.run choiceAnswerReader))
            >> Result.map Domain.ChoiceAnswer
        | None ->
            AttrReader.run (optDef Fields.Answer "" A.string)
            >> Result.map Domain.OpenAnswer

    let private singleSlipBuilder question imgKey answer comment commentImgKey points jeopardyPoints withChoice : Domain.SingleSlip =
        {Question = question; ImgKey = imgKey; Answer = answer; Comment = comment; CommentImgKey = commentImgKey;
            Points = points; JeopardyPoints = jeopardyPoints; WithChoice = withChoice}

    let singleSlipReader =
        singleSlipBuilder
        <!> (choice Fields.Questions A.docList questionsInSlipReader)
        <*> (optDef Fields.ImgKey "" A.string)
        <*> (choice Fields.Answers A.docList answersInSlipReader)
        <*> (optDef Fields.Comment "" A.string)
        <*> (optDef Fields.CommentImgKey "" A.string)
        <*> (optDef Fields.Points "1" A.number >-> P.decimal)
        <*> (opt Fields.JpdPoints (A.nullOr A.number) ??>-> P.decimal)
        <*> (optDef Fields.Choiсe false A.bool)

    let private multipleSlipBuilder name slips =
        Domain.Multiple (name, slips)

    let private multipleSlipReader =
        multipleSlipBuilder
        <!> (optDef Fields.Name  "" A.string)
        <*> (req Fields.Items A.docList @>-> (A.docMap, AttrReader.run singleSlipReader))

    let private slipReader =
        function
        | Some Fields.KindMultiple ->  AttrReader.run multipleSlipReader
        | Some Fields.KindWWW | Some Fields.KindSingle | _ -> (AttrReader.run singleSlipReader) >> Result.map Domain.Single

    let reader = AttrReader.run (choice Fields.Kind A.string slipReader)

    let fieldsOfSingleSlip (slip : Domain.SingleSlip) =
        [
            Attr (Fields.Kind, ScalarString Fields.KindSingle)
            match slip.Question with
            | Domain.Solid qw -> yield! BuildAttr.string Fields.Text qw
            | Domain.Split list -> Attr (Fields.Questions, DocList (list |> List.map ScalarString))
            yield! BuildAttr.string Fields.ImgKey slip.ImgKey
            match slip.Answer with
            | Domain.OpenAnswer aw -> yield! BuildAttr.string Fields.Answer aw
            | Domain.ChoiceAnswer (list) ->
                Attr (Fields.Answers, DocList (list
                    |> List.map (fun aw ->
                        DocMap [Attr (Fields.AnswerText, ScalarString aw.Text)
                                Attr (Fields.AnswerIsCorrect, ScalarBool aw.IsCorrect)])))
            yield! BuildAttr.string Fields.Comment slip.Comment
            yield! BuildAttr.string Fields.CommentImgKey slip.CommentImgKey
            Attr (Fields.Points, ScalarDecimal slip.Points)
            yield! BuildAttr.optional Fields.JpdPoints ScalarDecimal slip.JeopardyPoints
            if slip.WithChoice then Attr (Fields.Choiсe, ScalarBool slip.WithChoice)
        ]

    let fieldsOfMultipleSlip (name:string) (slips : Domain.SingleSlip list) =
        [Attr (Fields.Kind, ScalarString Fields.KindMultiple)
         if name <> "" then Attr (Fields.Name, ScalarString name)
         Attr (Fields.Items, DocList (slips |> List.map (fieldsOfSingleSlip >> DocMap)))]

    let fields (slip : Domain.Slip) =
        match slip with
        | Domain.Single s -> fieldsOfSingleSlip s
        | Domain.Multiple (name, slips) -> fieldsOfMultipleSlip name slips

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

    let private key id = [ Attr (Fields.Id, ScalarInt32 id) ]

    let private dscFields = [Fields.Id; Fields.Producer; Fields.Name]

    let private dscBuilder id producer name : Domain.PackageDescriptor =
        {PackageId = id; Producer = producer; Name = name}

    let private dscReader =
        dscBuilder
        <!> (req Fields.Id A.number >-> P.int)
        <*> (req Fields.Producer A.string)
        <*> (req Fields.Name A.string)

    let getDescriptor = key >> getItemProjection' dscFields tableName dscReader

    let private builder dsc transferToken sharedWith slips version : Domain.Package =
        {Dsc = dsc; TransferToken = transferToken; SharedWith = sharedWith |> Set.toList
         Slips = slips; Version = version}

    let private reader =
        builder
        <!> dscReader
        <*> (optDef Fields.TransferToken "" A.string)
        <*> (optDef Fields.SharedWith Set.empty A.setString)
        <*> (req Fields.Questions A.docList @>-> (A.docMap, Slips.reader))
        <*> (req Fields.Version A.number >-> P.int)

    let get = key >> getItem' tableName reader

    let provider = get >> Async.RunSynchronously >> Result.toOption

    let delete = key >> deleteItem' tableName

    let private put (pkg : Domain.Package) =
        [
            Attr (Fields.Id, ScalarInt32 pkg.Dsc.PackageId)
            Attr (Fields.Producer, ScalarString pkg.Dsc.Producer)
            Attr (Fields.Name, ScalarString pkg.Dsc.Name)
            Attr (Fields.TransferToken, ScalarString pkg.TransferToken)
            yield! BuildAttr.setString Fields.SharedWith (pkg.SharedWith |> List.map string)
            Attr (Fields.Questions, DocList (pkg.Slips |> List.map (Slips.fields >> DocMap)))
            Attr (Fields.Version, ScalarInt32 (pkg.Version + 1))
        ]
        |> putItem' (Some pkg.Version) tableName

    let create (creator : int -> Domain.Package) =
        SysItem.getNextId "pkg" (fun () -> -100)
        |> AsyncResult.bind (creator >> AsyncResult.retn)
        |> AsyncResult.side put

    let update = putOptimistic' get put

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
        let ImgKey = "ImgKey"
        let WithPremoderation = "WithPremoderation"
        let AdminToken = "AdminToken"
        let RegToken = "RegToken"
        let ListenToken = "ListenToken"
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
        let TourStartTime = "StartTime"
        let TourSlip = "Slip"
        let Version = "Version"
        let StreamUrl = "StreamUrl"

    let private key id = [ Attr (Fields.Id, ScalarInt32 id) ]

    let private dscFields = [
        Fields.Id; Fields.Producer; Fields.StartTime; Fields.Name; Fields.Status; Fields.WelcomeText; Fields.FarewellText;
        Fields.ImgKey;  Fields.WithPremoderation; Fields.AdminToken; Fields.RegToken; Fields.ListenToken;
        Fields.PkgId; Fields.PkgQwIdx; Fields.EventPage; Fields.MixlrCode; Fields.StreamUrl]

    let private dscBuilder id producer startTime name status wlcmText frwlText
        imgKey withPremoderation adminToken regToken listenToken pkgId pkgQwIdx
        evtPage mixlrCode streamUrl: Domain.QuizDescriptor =
        {QuizId = id; Producer = producer; StartTime = startTime; Name = name; Status = status |> Option.defaultValue Domain.Setup;
         WelcomeText = wlcmText; FarewellText = frwlText; ImgKey = imgKey; WithPremoderation = withPremoderation;
         AdminToken = adminToken; RegToken = regToken; ListenToken = listenToken;
         PkgId = pkgId; PkgSlipIdx = pkgQwIdx; EventPage = evtPage; MixlrCode = mixlrCode; StreamUrl = streamUrl}

    let private dscReader =
        dscBuilder
        <!> (req Fields.Id A.number >-> P.int)
        <*> (req Fields.Producer A.string)
        <*> (opt Fields.StartTime (A.nullOr A.string) ??>-> P.dateTime)
        <*> (req Fields.Name A.string)
        <*> (req Fields.Status A.string >- P.enum<Domain.QuizStatus>)
        <*> (optDef Fields.WelcomeText "" A.string)
        <*> (optDef Fields.FarewellText "" A.string)
        <*> (optDef Fields.ImgKey "" A.string)
        <*> (optDef Fields.WithPremoderation false A.bool)
        <*> (optDef Fields.AdminToken "" A.string)
        <*> (optDef Fields.RegToken "" A.string)
        <*> (optDef Fields.ListenToken "" A.string)
        <*> (opt Fields.PkgId (A.nullOr A.number) ??>-> P.int)
        <*> (opt Fields.PkgQwIdx (A.nullOr A.number) ??>-> P.int)
        <*> (optDef Fields.EventPage "" A.string)
        <*> (opt Fields.MixlrCode (A.nullOr A.number) ??>-> P.int)
        <*> (opt Fields.StreamUrl (A.nullOr A.string) ??>-> Ok)

    let getDescriptor = key >> getItemProjection' dscFields tableName dscReader

    let private tourBuilder name seconds status qwIdx qwPartIdx startTime slip : Domain.QuizTour =
        {Name = name; Seconds = seconds; Status = status |> Option.defaultValue Domain.Announcing;
            QwIdx = qwIdx; QwPartIdx = qwPartIdx; StartTime = startTime; Slip = slip}

    let private slipChoice =
        function
        | Some map -> (fun _ -> Slips.reader map)
        | None -> (AttrReader.run Slips.singleSlipReader) >> Result.map Domain.Single

    let private tourReader =
        tourBuilder
        <!> (optDef Fields.TourName "" A.string)
        <*> (req Fields.TourSeconds A.number >-> P.int)
        <*> (req Fields.TourStatus A.string >- P.enum<Domain.QuizTourStatus>)
        <*> (optDef Fields.TourQwIdx "0" A.number >-> P.int)
        <*> (optDef Fields.TourQwPartIdx "0" A.number >-> P.int)
        <*> (opt Fields.TourStartTime (A.nullOr A.string) ??>-> P.dateTime)
        <*> (choice Fields.TourSlip A.docMap slipChoice)

    let private builder dsc tours version : Domain.Quiz =
        {Dsc = dsc; Tours = tours; Version = version}

    let private reader =
        builder
        <!> dscReader
        <*> (req Fields.Questions A.docList @>-> (A.docMap, AttrReader.run tourReader))
        <*> (req Fields.Version A.number >-> P.int)

    let sysKey quizId = (sprintf "quiz-%i" quizId)

    let get = key >> getItem' tableName reader

    let private put (item : Domain.Quiz) =
        [
            Attr (Fields.Id, ScalarInt32 item.Dsc.QuizId)
            Attr (Fields.Producer, ScalarString item.Dsc.Producer)
            yield! BuildAttr.optional Fields.StartTime ScalarDate item.Dsc.StartTime
            Attr (Fields.Name, ScalarString item.Dsc.Name)
            Attr (Fields.Status, ScalarString (item.Dsc.Status.ToString()))
            yield! BuildAttr.string Fields.WelcomeText item.Dsc.WelcomeText
            yield! BuildAttr.string Fields.FarewellText item.Dsc.FarewellText
            yield! BuildAttr.string Fields.ImgKey item.Dsc.ImgKey
            Attr (Fields.WithPremoderation, ScalarBool item.Dsc.WithPremoderation)
            yield! BuildAttr.string Fields.AdminToken item.Dsc.AdminToken
            yield! BuildAttr.string Fields.RegToken item.Dsc.RegToken
            yield! BuildAttr.string Fields.ListenToken item.Dsc.ListenToken
            yield! BuildAttr.optional Fields.PkgId ScalarInt32 item.Dsc.PkgId
            yield! BuildAttr.optional Fields.PkgQwIdx ScalarInt32 item.Dsc.PkgSlipIdx
            yield! BuildAttr.string Fields.EventPage item.Dsc.EventPage
            yield! BuildAttr.optional Fields.MixlrCode ScalarInt32 item.Dsc.MixlrCode
            yield! BuildAttr.optional Fields.StreamUrl ScalarString item.Dsc.StreamUrl

            Attr (Fields.Questions, DocList [
                for tour in item.Tours do
                    yield DocMap [
                        yield! BuildAttr.string Fields.TourName tour.Name
                        Attr (Fields.TourSeconds, ScalarInt32 tour.Seconds)
                        Attr (Fields.TourStatus, ScalarString (tour.Status.ToString()))
                        Attr (Fields.TourSlip, DocMap (Slips.fields tour.Slip))
                        yield! BuildAttr.optional Fields.TourStartTime ScalarDate tour.StartTime
                        Attr (Fields.TourQwIdx, ScalarInt32 tour.QwIdx)
                        Attr (Fields.TourQwPartIdx, ScalarInt32 tour.QwPartIdx)
                    ]
            ])

            Attr (Fields.Version, ScalarInt32 (item.Version + 1))
        ]
        |> putItem' (Some item.Version) tableName

    let create (creator : int -> Domain.Quiz) =
        SysItem.getNextId "quizzes" (fun () -> -100)
        |> AsyncResult.bind (creator >> AsyncResult.retn)
        |> AsyncResult.side put

    let mutable private _subscriptions : (Domain.Quiz -> Async<unit>) list = []

    let subscribe handler =
        _subscriptions <- handler :: _subscriptions

    let update quizId logic =
        putOptimistic' get put quizId logic
        |> AsyncResult.side (fun quiz ->
            _subscriptions
            |> List.map (fun handler -> handler quiz)
            |> Async.Sequential
            |> Async.Catch
            |> Async.map Ok
        )

    let delete quizId =
        deleteItem' tableName (key quizId)
        |> AsyncResult.next (SysItem.delete (sysKey quizId))

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
        let AnswerIsAutoResult = "IsAutoResult"
        let AnswerUpdateTime = "UpdateTime"
        let Version = "Version"

    let private key quizId teamId =
       [ Attr (Fields.QuizId, ScalarInt32 quizId)
         Attr (Fields.TeamId, ScalarInt32 teamId) ]

    let private queryKeyConditions quizId =
        Query.KeyConditionExpression (Query.NumberEquals (Fields.QuizId, decimal quizId), [])

    let private dscFields =
        [Fields.QuizId; Fields.TeamId; Fields.Name; Fields.Status; Fields.EntryToken; Fields.RegistrationDate; Fields.ActiveSessionId]

    let private dscBuilder quizId teamId name status entryToken regDate activeSessionId : Domain.TeamDescriptor =
       {QuizId = quizId
        TeamId = teamId
        Name = name
        Status = status |> Option.defaultValue Domain.TeamStatus.New
        EntryToken = entryToken
        RegistrationDate = regDate
        ActiveSessionId = activeSessionId}

    let private dscReader =
        dscBuilder
        <!> (req Fields.QuizId A.number >-> P.int)
        <*> (req Fields.TeamId A.number >-> P.int)
        <*> (optDef Fields.Name "" A.string)
        <*> (req Fields.Status A.string >- P.enum<Domain.TeamStatus>)
        <*> (optDef Fields.EntryToken "" A.string)
        <*> (req Fields.RegistrationDate A.string >-> P.dateTime)
        <*> (req Fields.ActiveSessionId A.number >-> P.int)

    let getDescriptor quizId teamId  =
        key quizId teamId
        |> getItemProjection' dscFields tableName dscReader

    let getIds (quizId: int)  =
        let kce = queryKeyConditions quizId
        let reader = (req Fields.TeamId A.number >-> P.int)
        query' tableName [Fields.TeamId] reader kce

    let getDescriptors (quizId: int)  =
        let kce = queryKeyConditions quizId
        query' tableName dscFields dscReader kce

    let private teamAnswerBuilder text jpd recieveTime result isAutoResult updateTime : Domain.TeamAnswer =
        {Text = text; Jeopardy = jpd; RecieveTime = recieveTime;
            Result = result; IsAutoResult = isAutoResult; UpdateTime = updateTime}

    let private teamAnswerReader =
        teamAnswerBuilder
        <!> (optDef Fields.AnswerText "" A.string)
        <*> (optDef Fields.AnswerJeopardy false A.bool)
        <*> (req Fields.AnswerRecieveTime A.string >-> P.dateTime)
        <*> (opt Fields.AnswerResult  (A.nullOr A.number) ??>-> P.decimal)
        <*> (optDef Fields.AnswerIsAutoResult false A.bool)
        <*> (opt Fields.AnswerUpdateTime  (A.nullOr A.string) ??>-> P.dateTime)

    let private teamBuilder dsc answers version : Domain.Team =
        {Dsc = dsc; Answers = answers; Version = version}

    let private qwKeyOfString (str:string) =
        match str.StartsWith "qw" with
        | true ->
            let indexes = (str.Substring(2)).Split '.'
            {
                TourIdx = indexes |> Array.tryItem 0 |> Option.bind tryParseInt32 |> Option.defaultValue 0
                QwIdx = indexes |> Array.tryItem 1 |> Option.bind tryParseInt32 |> Option.defaultValue 0
            } : Domain.QwKey
        | false ->
            {
                TourIdx = str |> tryParseInt32 |> Option.defaultValue 0
                QwIdx = 0
            }
        |> Ok

    let private stringOfQwKey (key:Domain.QwKey) : string =
        sprintf "qw%i.%i" key.TourIdx key.QwIdx

    let private reader =
        teamBuilder
        <!> dscReader
        <*> (map Fields.Answers qwKeyOfString (A.docMap >> (AttrReader.run teamAnswerReader)))
        <*> (req Fields.Version A.number >-> P.int)

    let get (k:Domain.TeamKey) =
        getItem' tableName reader (key k.QuizId k.TeamId)

    let getAllInQuiz (quizId: int)  =
        let kce = queryKeyConditions quizId
        query' tableName [] reader kce

    let private put (item : Domain.Team) =
        [
            Attr (Fields.QuizId, ScalarInt32 item.Dsc.QuizId)
            Attr (Fields.TeamId, ScalarInt32 item.Dsc.TeamId)
            Attr (Fields.Name, ScalarString item.Dsc.Name)
            Attr (Fields.Status, ScalarString (item.Dsc.Status.ToString()))
            Attr (Fields.RegistrationDate, ScalarDate item.Dsc.RegistrationDate)
            Attr (Fields.EntryToken, ScalarString item.Dsc.EntryToken)
            Attr (Fields.ActiveSessionId, ScalarInt32 item.Dsc.ActiveSessionId)
            Attr (Fields.Answers, DocMap [
                for pair in item.Answers do
                    Attr((stringOfQwKey pair.Key), DocMap[
                        Attr (Fields.AnswerText, ScalarString pair.Value.Text)
                        Attr (Fields.AnswerJeopardy, ScalarBool pair.Value.Jeopardy)
                        Attr (Fields.AnswerRecieveTime, ScalarDate pair.Value.RecieveTime)
                        match pair.Value.Result with
                        | Some res -> Attr (Fields.AnswerResult, ScalarDecimal res)
                        | None -> Attr (Fields.AnswerResult, ScalarNull)
                        Attr (Fields.AnswerIsAutoResult, ScalarBool pair.Value.IsAutoResult)
                        match pair.Value.UpdateTime with
                        | Some v -> Attr (Fields.AnswerUpdateTime, ScalarDate v)
                        | None -> Attr (Fields.AnswerUpdateTime, ScalarNull)
                    ])
            ])
            Attr (Fields.Version, ScalarInt32 (item.Version + 1))
        ]
        |> putItem' (Some item.Version) tableName

    let create quizId creator =
        let lastIdProvider () =
            match getIds quizId |> Async.RunSynchronously with
            | Ok (head::tail) -> List.max (head::tail)
            | _ -> 0

        SysItem.getNextId (Quizzes.sysKey quizId) lastIdProvider
        |> AsyncResult.bind (creator >> AsyncResult.fromResult)
        |> AsyncResult.side put

    let check quizId teamId =
        key quizId teamId
        |> checkItem' tableName

    let update = putOptimistic' get put

    let delete quizId teamId  =
        key quizId teamId
        |> deleteItem' tableName