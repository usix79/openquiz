namespace OpenQuiz.Cdk

open System

open Amazon.CDK
open Amazon.CDK.AWS.S3
open Amazon.CDK.AWS.S3.Deployment
open Amazon.CDK.AWS.S3.Assets
open Amazon.CDK.AWS.EC2
open Amazon.CDK.AWS.ElasticBeanstalk

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
                PublicReadAccess = true ))

    let bucketDeployment =
        BucketDeployment(this, "DeploymentOfStatic",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/openquiz-static.zip") |] ))

    let vpc =
        Vpc(this, "OpenQuizVpc",
            VpcProps(
                NatGateways = 0.
            ))

    let apiBundle = Asset(this, "ApiBundle", AssetProps(Path = "./bundle/openquiz-api.zip"))

    let apiApp =
        CfnApplication(this, "ApiApp",
            CfnApplicationProps(
                ApplicationName = sprintf "OpenQuiz-%s" env ))

    let subnets = vpc.SelectSubnets(SubnetSelection(SubnetType = SubnetType.PUBLIC)).SubnetIds |> String.concat ","

    let optionSettingProperties = [|
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:elasticbeanstalk:environment", OptionName = "EnvironmentType", Value = "SingleInstance")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:autoscaling:launchconfiguration", OptionName = "InstanceType", Value = "t2.micro")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:autoscaling:launchconfiguration", OptionName = "IamInstanceProfile", Value = "aws-elasticbeanstalk-ec2-role")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "VPCId", Value = vpc.VpcId)
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "AssociatePublicIpAddress", Value = "true")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "Subnets", Value = subnets)
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "ELBScheme", Value = "public")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "ELBSubnets", Value = subnets)
        |]

    let appVersion =
        CfnApplicationVersion(this, "ApiAppVersion",
            CfnApplicationVersionProps(
                ApplicationName = apiApp.ApplicationName,
                SourceBundle = CfnApplicationVersion.SourceBundleProperty(S3Bucket = apiBundle.S3BucketName, S3Key = apiBundle.S3ObjectKey)
            ))

    let appEnv =
        CfnEnvironment(this, "ApiAppEnv",
            CfnEnvironmentProps(
                EnvironmentName = apiApp.ApplicationName + "-Single",
                ApplicationName = apiApp.ApplicationName,
                SolutionStackName = "64bit Amazon Linux 2 v2.1.1 running .NET Core",
                OptionSettings =  optionSettingProperties,
                VersionLabel = appVersion.Ref
            ))

    do appVersion.AddDependsOn(apiApp)