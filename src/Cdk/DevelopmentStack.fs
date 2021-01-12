namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.DynamoDB

module Helpers =

    let createTable' stack env tableName partitionKeyName partitionKeyType sortKey ttl =
        Table(stack, tableName,
                TableProps(
                    TableName = sprintf "OpenQuiz-%A-%s" env tableName,
                    PartitionKey = Attribute (Name = partitionKeyName, Type = partitionKeyType),
                    SortKey = (match sortKey with Some (name,type') -> Attribute (Name = name, Type = type') | None -> null),
                    BillingMode = BillingMode.PAY_PER_REQUEST,
                    TimeToLiveAttribute = match ttl with Some name -> name | _ -> null
                )) |> ignore

    let createTable stack env tableName partitionKeyName partitionKeyType =
        createTable' stack env tableName partitionKeyName partitionKeyType None None

    let createTableWithTTL stack env tableName partitionKeyName partitionKeyType ttl =
        createTable' stack env tableName partitionKeyName partitionKeyType None (Some ttl)

    let createTableWithSortKey stack env tableName partitionKeyName partitionKeyType sortKeyName sortKeyType =
        createTable' stack env tableName partitionKeyName partitionKeyType (Some (sortKeyName,sortKeyType)) None


open Helpers

type DevelopmentStack(env, scope, id, props) as this =
    inherit Stack(scope, (sprintf "%s-%A" id env), props)

    do createTable this env "System" "Id" AttributeType.STRING
    do createTable this env "Experts" "Id" AttributeType.STRING
    do createTable this env "Packages" "Id" AttributeType.NUMBER
    do createTable this env "Quizzes" "Id" AttributeType.NUMBER
    do createTableWithSortKey this env "Teams" "QuizId" AttributeType.NUMBER "TeamId" AttributeType.NUMBER
    do createTableWithTTL this env "Tokens" "Token" AttributeType.STRING "TTL"