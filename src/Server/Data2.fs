module rec Data2

open System
open Amazon.DynamoDBv2
open DynamoDb.Ok
open DynamoDb.Ok.Read
open DynamoDb.Ok.Write
open Serilog

open Common

module A = Attribute
module P = Parse
module R = AttrReader

type PutResult = Success | Retry

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
                        return Error "Transaction Failed"
                    | Error txt -> return Error txt
                | Error txt -> return Error txt
            | Error txt -> return Error txt
        }

    tryUpdate 1

let client = new AmazonDynamoDBClient()

let fullTableName name = sprintf "OQ-%s" name

let private checkItem' tableName fields =
    async{
        let! res = doesItemExist client (fullTableName tableName) fields
        return res |> Result.mapError (fun errors -> logErrors errors; "DynamoDB Get Error")
    }

let private getItemProjection' projection tableName reader fields  =
    async{
        match! getItemProjection projection client (fullTableName tableName) reader fields  with
        | Ok entity -> return Ok entity
        | Error [ItemDoesNotExist] -> return Error "ItemDoesNotExist"
        | Error list ->
            logErrors list
            return Error "DynamoDB Get Error"
    }

let private getItem' tableName reader = getItemProjection' [] tableName reader

let private putItem' version tableName fields  =
    async{
        let ce =
            match version with
            | None -> None
            | Some v when v = 0 -> None
            | Some v -> Query.KeyConditionExpression (Query.NumberEquals ("Version", decimal v), []) |> Some

        match! putItemWithCondition ce client (fullTableName tableName) fields with
        | Ok () -> return Ok Success
        | Error [ConditionalCheckFailed] -> return Ok Retry
        | Error list ->
            logErrors list
            return Error "DynamoDB Put Error"
    }

let private deleteItem' tableName fields =
    async{
        let! res = deleteItem client (fullTableName tableName) fields
        return res |> Result.mapError (fun errors -> logErrors errors; "DynamoDB Delete Error")
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

let private toStringList (set:Set<string>) =
    set
    |> Set.toList
    |> Ok

let private optInt32 fieldName v =
    match v with
    | Some id -> [Attr (fieldName, ScalarInt32 id)]
    | _ -> []

let private setString fieldName v =
    match v with
    | head::tail -> [Attr (fieldName, SetString (NonEmptyList (head, tail)))]
    | _ -> []

module RefreshTokens =
    let private tableName = "Tokens"

    let toEpoch dt =
        (dt - DateTime.UnixEpoch).TotalSeconds
        |> Convert.ToInt64
        |> Convert.ToDecimal

    let fromEpoch (epoch:decimal) =
        epoch
        |> Convert.ToDouble
        |> DateTime.UnixEpoch.AddSeconds

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
        async{
            let! _ = deleteItem' tableName (key oldToken.Value)
            return! put newToken
        }

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
            yield! setString Fields.Quizzes (exp.Quizzes |> List.map string)
            yield! setString Fields.Packages (exp.Packages |> List.map string)
            yield! setString Fields.PackagesSharedWithMe (exp.PackagesSharedWithMe |> List.map string)
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

    let private singleSlipBuilder question imgKey answer comment commentImgKey points jeopardyPoints withChoice : Domain.SingleAwSlip =
        {Question = question; ImgKey = imgKey; Answer = answer; Comment = comment; CommentImgKey = commentImgKey;
            Points = points; JeopardyPoints = jeopardyPoints; WithChoice = withChoice}

    let private singleSlipReader =
        singleSlipBuilder
        <!> (choice Fields.Questions A.docList questionsInSlipReader)
        <*> (optDef Fields.ImgKey "" A.string)
        <*> (optDef Fields.Answer "" A.string)
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

    let reader = AttrReader.run (choice Fields.Kind A.string Slips.slipReader)

    let fieldsOfSingleSlip (slip : Domain.SingleAwSlip) =
        [
            Attr (Fields.Kind, ScalarString Fields.KindSingle)
            match slip.Question with
            | Domain.Solid qw -> if qw <> "" then Attr (Fields.Text, ScalarString qw)
            | Domain.Split list -> Attr (Fields.Questions, DocList (list |> List.map ScalarString))
            if slip.ImgKey <> "" then Attr (Fields.ImgKey, ScalarString slip.ImgKey)
            if slip.Answer <> "" then Attr (Fields.Answer, ScalarString slip.Answer)
            if slip.Comment <> "" then Attr (Fields.Comment, ScalarString slip.Comment)
            if slip.CommentImgKey <> "" then Attr (Fields.CommentImgKey, ScalarString slip.CommentImgKey)
            Attr (Fields.Points, ScalarDecimal slip.Points)
            match slip.JeopardyPoints with
            | Some points -> Attr (Fields.JpdPoints, ScalarDecimal points)
            | None -> ()
            if slip.WithChoice then Attr (Fields.Choiсe, ScalarBool slip.WithChoice)
        ]

    let fieldsOfMultipleSlip (name:string) (slips : Domain.SingleAwSlip list) =
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
        {Dsc = dsc
         TransferToken = transferToken
         SharedWith = sharedWith |> Set.toList
         Slips = slips
         Version = version}

    let private reader =
        builder
        <!> dscReader
        <*> (optDef Fields.TransferToken "" A.string)
        <*> (optDef Fields.SharedWith Set.empty A.setString)
        <*> (req Fields.Questions A.docList @>-> (A.docMap, Slips.reader))
        <*> (req Fields.Version A.number >-> P.int)

    let get = key >> getItem' tableName reader

    let delete = key >> deleteItem' tableName

    let private put (pkg : Domain.Package) =
        [
            Attr (Fields.Id, ScalarInt32 -100)
            Attr (Fields.Producer, ScalarString pkg.Dsc.Producer)
            Attr (Fields.Name, ScalarString pkg.Dsc.Name)
            Attr (Fields.TransferToken, ScalarString pkg.TransferToken)
            yield! setString Fields.SharedWith (pkg.SharedWith |> List.map string)
            Attr (Fields.Questions, DocList (pkg.Slips |> List.map (Slips.fields >> DocMap)))
            Attr (Fields.Version, ScalarInt32 (pkg.Version + 1))
        ]
        |> putItem' None tableName

    let update = putOptimistic' get put
