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

let private putOptimistic' id logic get put =

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

let private getItem' tableName reader fields =
    async{
        match! getItem client (fullTableName tableName) reader fields with
        | Ok entity -> return Ok entity
        | Error [ItemDoesNotExist] -> return Error "ItemDoesNotExist"
        | Error list ->
            logErrors list
            return Error "DynamoDB Get Error"
    }

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

    let private key value =
        [ Attr (Fields.Token, ScalarString value) ]

    let private builder value expired : Domain.RefreshToken=
        {Value = value; Expired = fromEpoch expired}

    let private reader =
        builder
        <!> (req Fields.Token A.string)
        <*> (req Fields.Expired A.number >-> P.decimal)

    let get tokenValue =
        getItem' tableName reader (key tokenValue)

    let put (token:Domain.RefreshToken) =
        let fields = [
            Attr (Fields.Token, ScalarString token.Value)
            Attr (Fields.Expired, ScalarDecimal (token.Expired |> toEpoch))
        ]
        putItem' None tableName fields

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

    let private key sub =
        [ Attr (Fields.Id, ScalarString sub) ]

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

    let get id =
        getItem' tableName reader (key id)

    let provider id =
        get id
        |> Async.RunSynchronously
        |> Result.toOption

    let put (exp : Domain.Expert) =
        let fields = [
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

        putItem' (Some exp.Version) tableName fields

    let update id logic =
        putOptimistic' id logic get put

    let updateOrCreate id logic (creator:unit->Domain.Expert) =
        async{
            do!
                async{
                    match! checkItem' tableName (key id) with
                    | Ok false -> return! put <| creator() |> Async.Ignore
                    | _ -> return ()
                }

            return! putOptimistic' id logic get put
        }


