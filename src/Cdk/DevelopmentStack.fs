namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.S3

type DevelopmentStack(scope:Construct, id, props, globalId) as this =
    inherit Stack(scope, id, props)
    let env = "Development"

    do Assets.createDynamoDBTables this env
    let mediaBucket = Assets.createMediaBucket this env
    do Helpers.createParameter this env "BucketUrl" mediaBucket.BucketWebsiteUrl

    do Assets.createUserPool this env globalId "http://localhost:8080/app/" |> ignore
    do Assets.createAppsyncApi this env
