module Env

open DynamoDb.Ok
open DynamoDb.Ok.Read
open Common

[<Interface>] type ILog = abstract Logger: Serilog.ILogger

[<Interface>]
type IConfigurer =
    abstract DynamoTablePrefix: string

[<Interface>] type ICfg = abstract Configurer: IConfigurer

[<Interface>]
type IPublisher =
    abstract Publish: PublisherCommand -> unit

type IPublisherEnv =
    inherit ILog
    inherit ICfg

type IAppEnv =
    inherit ILog
    inherit ICfg
    inherit IPublisher