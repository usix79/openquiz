namespace DynamoDb.Ok

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open Amazon.DynamoDBv2
open Amazon.DynamoDBv2.Model


type Attr = Attr of name: string * value: AttrValue

and AttrValue =
    | ScalarString of String
    | ScalarInt32 of Int32
    | ScalarDecimal of Decimal
    | ScalarBinary of String
    | ScalarBool of Boolean
    | ScalarGuid of Guid
    | ScalarDate of DateTime
    | ScalarNull
    | SetString of NonEmptyList<String>
    | SetDecimal of NonEmptyList<Decimal>
    | SetInt32 of NonEmptyList<Int32>
    | SetBinary of NonEmptyList<String>
    | DocList of AttrValue list
    | DocMap of Attr list

and NonEmptyList<'a when 'a: comparison> = NonEmptyList of head: 'a * tail: 'a list

type private A = AttributeValue

type PutResult<'item> = Success | Retry

module private AttrMapping =

    let ISO8601DateFormat = "yyyy-MM-dd\\THH:mm:ss.fff\\Z"
    let toSet (NonEmptyList (head, tail)) = Set.ofList tail |> Set.add head

    let rec mapAttrValue =
        function
        | ScalarString s -> A(S = s)
        | ScalarGuid g -> A(S = string g)
        | ScalarDate d -> A(S = d.ToUniversalTime().ToString(ISO8601DateFormat, Globalization.CultureInfo.InvariantCulture))
        | ScalarInt32 i -> A(N = string i)
        | ScalarDecimal d -> A(N = string d)
        | ScalarBinary b -> A(B = toGzipMemoryStream b)
        | ScalarBool b -> A(BOOL = b)
        | ScalarNull -> A(NULL = true)
        | SetString ss -> A(SS = ResizeArray(toSet ss))
        | SetDecimal sd -> A(NS = ResizeArray(Seq.map string (toSet sd)))
        | SetInt32 si -> A(NS = ResizeArray(Seq.map string (toSet si)))
        | SetBinary bs -> A(BS = ResizeArray(Seq.map toGzipMemoryStream (toSet bs)))
        | DocList l -> A(L = ResizeArray(List.map mapAttrValue l), IsLSet = true)
        | DocMap m -> A(M = mapAttrsToDictionary m, IsMSet = true)

    and mapAttr (Attr (name, value)) = name, mapAttrValue value

    and mapAttrsToDictionary =
        List.map mapAttr >> dict >> Dictionary<string, A>

    and toGzipMemoryStream (s: string) =
        let output = new MemoryStream()

        use zipStream =
            new GZipStream(output, CompressionMode.Compress, true)

        use writer = new StreamWriter(zipStream)
        writer.Write s
        output

module Async =

    let retn x = async { return x }

    let map f m =
        async {
            let! x = m
            return f x
        }

    let bind f m =
        async {
            let! x = m
            return! f x
        }

module AsyncResult =

    let ret x = async { return x }

    let retn x = async { return Ok x }

    let map f m =
        async {
            let! r = m
            match r with
            | Ok a -> return Ok(f a)
            | Error e -> return Error e
        }

    let bind f m =
        async {
            let! r = m
            match r with
            | Ok a -> return! f a
            | Error e -> return Error e
        }

type DynamoDbError =
    | ParseError of attributeName: string
    | MissingAttributeError of attributeName: string
    | OperationError of exn
    | UnprocessedItemsError
    | ItemDoesNotExist
    | ConditionalCheckFailed

module DynamoDbError =

    let private toString =
        function
        | ParseError e
        | MissingAttributeError e -> e
        | OperationError e -> string e
        | UnprocessedItemsError -> "Unprocessed items in batch operation"
        | ItemDoesNotExist -> "Item does not exist"
        | ConditionalCheckFailed -> "Conditional check failed"

    let flatten =
        List.fold (fun acc e -> sprintf "%s\n%s" acc (toString e)) String.Empty

    let handleAsyncError =
        function
        | Choice1Of2 x -> Ok x
        | Choice2Of2 (e: exn) when (e :? AggregateException) ->
            match e.InnerException with
            | ex when  (ex :? ConditionalCheckFailedException) -> Error [ ConditionalCheckFailed ]
            | _ -> Error [ OperationError e.InnerException ]
        | Choice2Of2 (e: exn) -> Error [ OperationError e ]


module Query =

    type KeyCondition =
        | StringBeginsWith of key: String * value: String
        | StringEquals of key: String * value: String
        | NumberEquals of key: String * value: Decimal
        | NumberLessThan of key: String * value: Decimal
        | NumberLessThanOrEqualTo of key: String * value: Decimal
        | NumberGreaterThan of key: String * value: Decimal
        | NumberGreaterThanOrEqualTo of key: String * value: Decimal
        | NumberBetwixt of key: String * value: Decimal * Decimal

    and KeyConditionExpression = KeyConditionExpression of KeyCondition * (BoolOperator * KeyConditionExpression) list

    and BoolOperator =
        | And
        | Or

    let private boolOperatorToExpression =
        function
        | And -> "AND"
        | Or -> "OR"

    let private getAttributeName attributes value =
        let getAlphabetLetter =
            (+) 64 >> char >> string >> fun s -> s.ToLower()

        let attributeName =
            List.length attributes
            + 1
            |> getAlphabetLetter
            |> sprintf ":%s"

        attributeName, (attributeName, value) :: attributes

    let private getAttribute attributes key value format =
        let attributeName, attributes = getAttributeName attributes value
        sprintf format key attributeName, attributes

    let private keyConditionToString attributes =
        function
        | StringBeginsWith (key, value) -> getAttribute attributes key (ScalarString value) "begins_with(%s, %s)"
        | StringEquals (key, value) -> getAttribute attributes key (ScalarString value) "%s = %s"
        | NumberEquals (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s = %s"
        | NumberLessThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s < %s"
        | NumberLessThanOrEqualTo (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s <= %s"
        | NumberGreaterThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s > %s"
        | NumberGreaterThanOrEqualTo (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s >= %s"
        | NumberBetwixt (key, start, end_) ->
            let startAttributeName, attributes =
                getAttributeName attributes (ScalarDecimal start)

            let endAttributeName, attributes =
                getAttributeName attributes (ScalarDecimal end_)

            sprintf "%s between %s and %s" key startAttributeName endAttributeName, attributes

    let rec buildKeyConditionExpression (KeyConditionExpression (kc, additionalConditions)) attributes =
        let init = keyConditionToString attributes kc

        let folder (acc, attributes) (operator, kce) =
            let exp, attributes =
                buildKeyConditionExpression kce attributes

            let bool = boolOperatorToExpression operator
            sprintf "%s %s (%s)" acc bool exp, attributes

        List.fold folder init additionalConditions

    let buildProjection (projectionFields:string list) =
        let attrs =
            projectionFields
            |> List.mapi (fun idx name -> (sprintf "#prj%i" idx, name ))

        System.String.Join (",", attrs |> List.map fst), Dictionary<string,string>(attrs |> Map.ofList)


module Write =

    let private putItem' ce (client: AmazonDynamoDBClient) tableName fields  =

        let req = PutItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)

        match ce with
        | Some ce ->
            let expression, attrs = Query.buildKeyConditionExpression ce []

            let buildAttributes =
                List.map (fun (attr, value) -> attr, AttrMapping.mapAttrValue value)
                >> dict
                >> Dictionary<string, A>

            req.ConditionExpression <- expression
            req.ExpressionAttributeValues <- buildAttributes attrs
        | None -> ()

        req
        |> client.PutItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map ignore)

    let putItemWithCondition = putItem'

    let putItem (client: AmazonDynamoDBClient) = putItem' None client

    let putItems (client: AmazonDynamoDBClient) tableName =

        let (|Success|Retry|GiveUp|) =
            function
            | true, true -> Retry
            | true, false -> GiveUp
            | false, _ -> Success

        let rec write attempt items: Async<Result<Unit, DynamoDbError list>> =
            BatchWriteItemRequest(RequestItems = items)
            |> client.BatchWriteItemAsync
            |> Async.AwaitTask
            |> Async.Catch
            |> Async.map DynamoDbError.handleAsyncError
            |> AsyncResult.bind (fun r ->
                match r.UnprocessedItems.Count > 0, attempt < 3 with
                | Success -> AsyncResult.retn ()
                | GiveUp -> Async.retn (Error [ UnprocessedItemsError ])
                | Retry ->
                    Async.Sleep (attempt*100)
                    |> Async.map Ok
                    |> AsyncResult.bind (fun _ -> write (attempt+1) r.UnprocessedItems))

        List.map
            (AttrMapping.mapAttrsToDictionary
             >> fun a -> PutRequest(Item = a)
             >> fun r -> WriteRequest(PutRequest = r))
        >> fun reqs -> [ tableName, ResizeArray reqs ]
        >> dict
        >> Dictionary<string, ResizeArray<WriteRequest>>
        >> write 0

    let deleteItem (client: AmazonDynamoDBClient) tableName fields =
        DeleteItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.DeleteItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map ignore)

    module BuildAttr =

        let optional name attr =
            function
            | Some s -> [ Attr(name, attr s) ]
            | None -> []

        let setString name =
            function
            | h :: t -> [ Attr(name, NonEmptyList(h, t) |> SetString) ]
            | _ -> []

        let setDecimal name =
            function
            | h :: t -> [ Attr(name, NonEmptyList(h, t) |> SetDecimal) ]
            | _ -> []

        let string name s =
            if String.IsNullOrEmpty s then [] else [ Attr(name, ScalarString s) ]

        let docList name =
            function
            | h :: t -> [ Attr(name, DocList(h :: t)) ]
            | _ -> []

    module BuildAttrValue =

        let optionToNull attr =
            function
            | Some s -> attr s
            | None -> ScalarNull


module Read =

    type AttrReader<'a> = AttrReader of (Map<string, A> -> 'a)

    module AttrReader =

        let run (AttrReader f) a = f a

        let retn a = AttrReader(fun _ -> a)

        let bind f ra =
            AttrReader(fun m -> run ra m |> f |> fun rb -> run rb m)

        let map f r = AttrReader(run r >> f)

    // let apply f r =
    //   AttrReader (fun a -> run r a |> run f a)


    module private AttrReaderResult =

        let retn a = Ok a |> AttrReader.retn

        let map f =
            Result.map f
            |> fun f r -> AttrReader(AttrReader.run r >> f)

        let bind f r =
            AttrReader(fun b ->
                AttrReader.run r b
                |> Result.bind (fun a -> AttrReader.run (f a) b))

        // let apply f r =
        //   AttrReader <| fun a ->
        //     let fa = AttrReader.run f a
        //     let fb = AttrReader.run r a
        //     match fa, fb with
        //     | Ok a, Ok b -> Ok (a b)
        //     | Error e, _ -> Error e
        //     | _, Error e -> Error e

        let apply f readerResultA =
            let newReader m =
                let resultF = AttrReader.run f m
                let resultA = AttrReader.run readerResultA m

                let resultB =
                    match resultF, resultA with
                    | Ok f, Ok a -> Ok(f a)
                    | Error e1, Error e2 -> Error(e1 @ e2)
                    | Error e1, _ -> Error e1
                    | _, Error e2 -> Error e2

                resultB

            AttrReader newReader

        let mapError e f =
            AttrReader(AttrReader.run f >> Result.mapError e)


    type AttrReaderResultBuilder() =
        member __.Return(x) = AttrReaderResult.retn x
        member __.ReturnFrom(m: AttrReader<Result<'a, 'b>>) = m
        member __.Bind(f, r) = AttrReaderResult.bind r f
        member __.Zero() = __.Return()

    let attrReaderResult = AttrReaderResultBuilder()


    let private toMap d = Seq.map (|KeyValue|) d |> Map.ofSeq

    let private traverseResult f list =
        let folder head tail =
            f head
            |> Result.bind (fun h -> tail |> Result.bind (fun t -> Ok(h :: t)))

        List.foldBack folder list (Ok [])

    let private ifSome f =
        function
        | Some x -> f x |> Result.map Some
        | None -> Ok None

    let private ifSome2 f =
        function
        | Some (Some x) -> f x |> Result.map Some
        | _ -> Ok None

    let private required key =
        function
        | Some x -> Ok x
        | None -> Error [ MissingAttributeError(sprintf "could not find attr %s" key) ]

    let req key typ =
        AttrReader(Map.tryFind key >> required key >> Result.map typ)

    let opt key typ =
        AttrReader(Map.tryFind key >> Option.map typ >> Ok)

    let optDef key defValue typ =
        AttrReader(Map.tryFind key >> Option.map typ >> Option.defaultValue defValue >> Ok)

    let choice key typ f =
        fun m ->
            let ch = m |> Map.tryFind key |> Option.map typ
            f ch m
        |> AttrReader

    let map key keyReader valueReader =
        fun m ->
            m
            |> Map.tryFind key
            |> required key
            |> Result.map (fun (a:A) -> a.M |> Seq.map (|KeyValue|) |> List.ofSeq )
            |> Result.bind (fun list ->
                list
                |> traverseResult (
                    fun (k,v) ->
                        match (keyReader k), (valueReader v) with
                        | (Ok k), (Ok v) -> Ok (k, v)
                        | _ -> Error [ParseError key]
                ))
            |> Result.map Map.ofList
        |> AttrReader

    let private toInt32Map (map:Map<string,Model.AttributeValue>) =
        map
        |> Map.toSeq
        |> Seq.map (fun (key,value) -> ((Int32.Parse key), (Int32.Parse value.N)))
        |> Map.ofSeq
        |> Ok


    let (<!>) = AttrReaderResult.map
    let (<*>) = AttrReaderResult.apply
    let (>>=) r f = AttrReaderResult.bind f r

    /// pass ARR into map
    let (>-) r f = AttrReaderResult.map f r

    /// pass ARR into Result returing f (e.g. Parse.*)
    let (>->) r f = AttrReader.map (Result.bind f) r

    /// pass ARR option into map
    let (?>-) r f = r >- Option.map f

    /// pass ARR option into Result returing f (e.g. Parse.*)
    let (?>->) r f = r >-> ifSome f

    let (??>->) r f = r >-> ifSome2 f

    /// pass ARR list into map
    let (@>-) r typ = r >- List.map typ

    /// pass ARR list into Result returing f (e.g. Parse.*)
    let (@>->) r (typ, f) = r >-> (List.map typ >> traverseResult f)

    /// pass ARR option list into map
    let (?@>-) r f = r >- Option.map (List.map f)

    /// pass ARR option list into Result returing f (e.g. Parse.*)
    let (?@>->) r (typ, f) =
        r >-> ifSome (List.map typ >> traverseResult f)

    /// pass ARR list option into map
    let (@?>-) r f = r >- List.map (Option.map f)

    /// pass ARR list option into Result returing f (e.g. Parse.*)
    let (@?>->) r (typ, f) =
        r
        >-> (List.map (Option.map typ)
             >> traverseResult (ifSome f))

    let getItemProjection (projection:string list) (client: AmazonDynamoDBClient) tableName reader fields =
        let req = GetItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        if not (projection.IsEmpty) then
            let (expression, names) = Query.buildProjection projection
            req.ProjectionExpression <- expression
            req.ExpressionAttributeNames <- names

        req
        |> client.GetItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map (fun r -> toMap r.Item)
             >> Result.bind (fun m -> if Map.isEmpty m then Error [ItemDoesNotExist] else Ok m)
             >> Result.bind (AttrReader.run reader))

    let getItem (client: AmazonDynamoDBClient) tableName reader fields = getItemProjection []

    let doesItemExist (client: AmazonDynamoDBClient) tableName fields =
        GetItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.GetItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map (fun r -> toMap r.Item |> Map.isEmpty |> not))

    let private queryScanIndexDirection forward (client: AmazonDynamoDBClient) (projection:string list)  tableName reader kce =
        let expression, attrs = Query.buildKeyConditionExpression kce []

        let buildAttributes =
            List.map (fun (attr, value) -> attr, AttrMapping.mapAttrValue value)
            >> dict
            >> Dictionary<string, A>

        let request startKey =
            let req = QueryRequest
                        (tableName,
                         KeyConditionExpression = expression,
                         ExpressionAttributeValues = buildAttributes attrs,
                         ScanIndexForward = forward,
                         ExclusiveStartKey = startKey)

            if not (projection.IsEmpty) then
                let (expression, names) = Query.buildProjection projection
                req.ProjectionExpression <- expression
                req.ExpressionAttributeNames <- names

            req

        let rec query req =
            req
            |> client.QueryAsync
            |> Async.AwaitTask
            |> Async.Catch
            |> Async.map
                (DynamoDbError.handleAsyncError
                 >> Result.map (fun r -> Seq.map toMap r.Items |> List.ofSeq, r.LastEvaluatedKey))
            |> AsyncResult.bind (function
                | items, key when key.Count = 0 -> AsyncResult.retn items
                | items, key ->
                    async {
                        match! query (request key) with
                        | Ok restItems -> return Ok (items @ restItems)
                        | Error list -> return Error list
                    }
                )

        request null
        |> query
        |> AsyncResult.bind (fun list ->
            list
            |> traverseResult (AttrReader.run reader)
            |> AsyncResult.ret
        )

    let query client = queryScanIndexDirection true client []

    let queryProjection client = queryScanIndexDirection true client

    let queryReverse client = queryScanIndexDirection false client []

    let queryProjectionReverse client = queryScanIndexDirection false client


    module Attribute =

        let string (a: A) = a.S

        let bool (a: A) = a.BOOL

        let number (a: A) = a.N

        let docMap (a: A) = toMap a.M

        let docList (a: A) = List.ofSeq a.L

        let setString (a: A) = Set.ofSeq a.SS

        let isNull (a: A) = a.NULL

        let nullOr f (a: A) = if a.NULL then None else Some(f a)


    module Parse =

        let private fromByRef e =
            function
            | true, x -> Ok x
            | _ -> Error [ ParseError e ]

        let guid (s: String) =
            Guid.TryParse s
            |> fromByRef (sprintf "could not parse %s as guid" s)

        let dateTime (s: String) =
            DateTime.TryParseExact(s, AttrMapping.ISO8601DateFormat, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.AssumeUniversal)
            |> fromByRef (sprintf "could not parse %s as date" s)

        let decimal (s: String) =
            Decimal.TryParse s
            |> fromByRef (sprintf "could not parse %s as decimal" s)

        let int (s: String) =
            Int32.TryParse s
            |> fromByRef (sprintf "could not parse %s as integer" s)

        let enum<'a> (s: String) =
            match Reflection.FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
            |[|case|] -> Some(Reflection.FSharpValue.MakeUnion(case,[||]) :?> 'a)
            |_ -> None

/// todo:
/// parameterize error type (not hardcoded string)
/// update
/// page results
/// filter expressions
/// global/local secondary indexes
//√ key condition expression with 'or' logic


module Example =

    open Read

    module A = Attribute
    module P = Parse
    module R = Read.AttrReader

    type Inny = { X: String; Y: Decimal }

    let buildInny x y = { X = x; Y = y }

    let innyReader =
        buildInny
        <!> (req "x" A.string)
        <*> (req "y" A.string >-> P.decimal)

    type Outty =
        { F: String
          G: DateTime
          I: Inny
          H: Inny list
          J: Decimal list
          K: String list
          L: DateTime list option }

    let buildOutty f g i h j k l =
        { F = f
          G = g
          I = i
          H = h
          J = j
          K = k
          L = l }


    let outtyReader =
        buildOutty
        <!> (req "f" A.string)
        <*> (req "g" A.string >-> P.dateTime)
        <*> (req "i" A.docMap >-> R.run innyReader)
        <*> (req "h" A.docList
             @>-> (A.docMap, R.run innyReader))
        <*> (req "j" A.docList @>-> (A.number, P.decimal))
        <*> (req "k" A.docList @>- A.string)
        <*> (opt "l" A.docList ?@>-> (A.string, P.dateTime))
