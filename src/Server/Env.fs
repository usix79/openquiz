module Env

open Common
open Shared

[<Interface>] type ILog = abstract Logger: Serilog.ILogger

[<Interface>]
type IConfigurer =
    abstract DynamoTablePrefix: string
    abstract JwtSecret : string
    abstract UserPoolClientId : string
    abstract LoginUrl : string
    abstract AppUrl : string
    abstract BucketName : string
    abstract BucketUrl : string
    abstract AppSyncCfg : AppSyncConfig

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