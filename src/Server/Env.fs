module Env

open DynamoDb.Ok
open DynamoDb.Ok.Read

[<Interface>] type ILog = abstract Logger: Serilog.ILogger

[<Interface>]
type IConfigurer =
    abstract DynamoTablePrefix: string

[<Interface>] type ICfg = abstract Configurer: IConfigurer

[<Interface>]
type IDatabase =
    abstract GetItem: tableName:string * reader:AttrReader<Result<'b,list<DynamoDbError>>> -> (Attr list -> Async<Result<'b,string>>)
    abstract PutItem: tableName:string * version:option<int> -> (list<Attr>  -> Async<Result<PutResult<'b>,string>>)
    abstract DelItem: tableName:string -> (list<Attr>  -> Async<Result<unit,string>>)
    abstract CheckItem: tableName:string -> (list<Attr> -> Async<Result<bool,string>>)
    abstract GetProjection: tableName:string * reader:AttrReader<Result<'b,list<DynamoDbError>>> * projection:list<string> -> (list<Attr> -> Async<Result<'b,string>>)
    abstract PutOptimistic: id:'a * get:('a -> Async<Result<'b,string>>) *  put:('c -> Async<Result<PutResult<'d>,string>>) * logic:('b -> Result<'c,string>) -> Async<Result<'c,string>>
    abstract Query: tableName:string * projection:list<string> * reader:AttrReader<Result<'b,list<DynamoDbError>>> -> (Query.KeyConditionExpression -> Async<Result<list<'b>,string>>)

[<Interface>] type IDb = abstract Database: IDatabase

// [<Struct>]
// type AppEnv (logger:Serilog.ILogger, database:IDatabase, configurer:IConfigurer) =
//     interface ILog with member _.Logger = logger
//     interface ICfg with member _.Configurer = configurer
//     interface IDb with member _.Database = database

type IDbEnv =
    inherit ILog
    inherit ICfg

type IAppEnv =
    inherit ILog
    inherit ICfg
    inherit IDb