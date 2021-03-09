namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.S3
open Amazon.CDK.AWS.S3.Deployment
open Amazon.CDK.AWS.S3.Assets
open Amazon.CDK.AWS.EC2
open Amazon.CDK.AWS.ElasticBeanstalk
open Amazon.CDK.AWS.CloudFront
open Amazon.CDK.AWS.IAM

type ProductionStack(scope:Construct, id, props, globalId) as this =
    inherit Stack(scope, id, props)
    let env = "Production"

    do Assets.createDynamoDBTables this env

    let bucket = Assets.createBucket this env

    do BucketDeployment(this, "DeploymentOfHtml",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/client/", AssetOptions(Exclude = [|"*.*"; "!*.html";|])) |],
                CacheControl = [|CacheControl.FromString("max-age=0,no-cache,no-store,must-revalidate")|],
                Prune = false
            )) |> ignore
    do BucketDeployment(this, "DeploymentOfRest",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/client/", AssetOptions(Exclude = [|"*.html"|])) |],
                CacheControl = [|CacheControl.FromString("max-age=31536000,public,immutable")|],
                Prune = false
            )) |> ignore

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


    let apiAppRoleName = "open-quiz-api-handler"
    let apiAppRole =
        Role(this, "ApiAppRole",
            RoleProps(
                RoleName = apiAppRoleName,
                AssumedBy = ServicePrincipal("ec2")))

    let instanceProfile =
        CfnInstanceProfile(this, "ApiInstanceProfile",
            CfnInstanceProfileProps(
                InstanceProfileName = apiAppRoleName,
                Roles = [|apiAppRole.RoleName|]
            ))

    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AmazonS3FullAccess"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("CloudWatchAgentServerPolicy"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AWSAppSyncInvokeFullAccess"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AmazonDynamoDBFullAccess"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("CloudWatchLogsFullAccess"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AmazonSSMFullAccess"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AmazonEC2ContainerRegistryReadOnly"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AWSElasticBeanstalkWebTier"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AWSElasticBeanstalkMulticontainerDocker"))
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName("AWSElasticBeanstalkWorkerTier"))

    let subnets = vpc.SelectSubnets(SubnetSelection(SubnetType = SubnetType.PUBLIC)).SubnetIds |> String.concat ","

    let optionSettingProperties = [|
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:elasticbeanstalk:environment", OptionName = "EnvironmentType", Value = "SingleInstance")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:autoscaling:launchconfiguration", OptionName = "InstanceType", Value = "t2.micro")
        CfnEnvironment.OptionSettingProperty(Namespace = "aws:autoscaling:launchconfiguration", OptionName = "IamInstanceProfile", Value = instanceProfile.InstanceProfileName)
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

    let cnamePrefix = sprintf "openquiz-%s-%s" (env.ToLower()) globalId
    let appEnv =
        CfnEnvironment(this, "ApiAppEnv",
            CfnEnvironmentProps(
                EnvironmentName = apiApp.ApplicationName + "-Single",
                ApplicationName = apiApp.ApplicationName,
                SolutionStackName = "64bit Amazon Linux 2 v2.1.3 running .NET Core",
                OptionSettings =  optionSettingProperties,
                CnamePrefix = cnamePrefix,
                VersionLabel = appVersion.Ref
            ))

    do appVersion.AddDependsOn(apiApp)

    let apiEnvCname = sprintf "%s.%s.elasticbeanstalk.com" cnamePrefix this.Region

    let distribution =
        CloudFrontWebDistribution(this, "OpenQuizWebDistribution",
            CloudFrontWebDistributionProps(
                OriginConfigs = [|
                    SourceConfiguration(
                        S3OriginSource = S3OriginConfig(S3BucketSource = bucket),
                        Behaviors = [|
                            Behavior(PathPattern = "/static/*", AllowedMethods = CloudFrontAllowedMethods.GET_HEAD_OPTIONS, DefaultTtl = Duration.Seconds(0.) )
                            Behavior(PathPattern = "/media/*", AllowedMethods = CloudFrontAllowedMethods.GET_HEAD_OPTIONS)
                            Behavior(IsDefaultBehavior = true )
                            |])
                    SourceConfiguration(
                        CustomOriginSource = CustomOriginConfig(DomainName = apiEnvCname, OriginProtocolPolicy = OriginProtocolPolicy.HTTP_ONLY),
                        Behaviors = [|Behavior(PathPattern = "/api/*", AllowedMethods = CloudFrontAllowedMethods.ALL, DefaultTtl = Duration.Seconds(0.) )|])
                |]
            )
        )

    let cloudFrontUrl = "https://" + distribution.DistributionDomainName
    do Helpers.createParameter this env "BucketUrl" cloudFrontUrl

    do CfnOutput(this, "OpenQuizUrl", CfnOutputProps (Value = cloudFrontUrl, Description = "Alias your domain name with the Url")) |> ignore

    let userPool = Assets.createUserPool this env globalId (cloudFrontUrl + "/app/index.html")
    do Assets.createAppsyncApi this env
