namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.S3
open Amazon.CDK.AWS.S3.Deployment

type ProductionStack(scope:Construct, id, props, globalId) as this =
    inherit Stack(scope, id, props)
    let env = "Production"

    do Assets.createDynamoDBTables this env
    do Assets.createMediaBucket this env |> ignore

    let bucket =
        Bucket(this, "StaticBucket",
            BucketProps(
                WebsiteIndexDocument = "index.html",
                WebsiteErrorDocument = "error.html",
                PublicReadAccess = true))

    let bucketDeployment =
        BucketDeployment(this, "DeploymentOfStatic",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/openquiz-static.zip") |]
            ))