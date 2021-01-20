namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.S3

type ProductionStack(scope:Construct, id, props, globalId) as this =
    inherit Stack(scope, id, props)
    let env = "Production"

    do Assets.createDynamoDBTables this env
