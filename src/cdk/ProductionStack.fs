namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.S3.Deployment
open Amazon.CDK.AWS.S3.Assets
open Amazon.CDK.AWS.EC2
open Amazon.CDK.AWS.ElasticBeanstalk
open Amazon.CDK.AWS.CloudFront
open Amazon.CDK.AWS.CloudFront.Origins
open Amazon.CDK.AWS.IAM
open Constructs

type ProductionStack(scope: Construct, id, props, env, globalId) as this =
    inherit Stack(scope, id, props)
    do Assets.createDynamoDBTables this env

    let bucket = Assets.createBucket this env

    do
        BucketDeployment(
            this,
            "DeploymentOfHtml",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/client/", AssetOptions(Exclude = [| "*.*"; "!*.html" |])) |],
                CacheControl = [| CacheControl.FromString "max-age=0,no-cache,no-store,must-revalidate" |],
                Prune = false
            )
        )
        |> ignore

    do
        BucketDeployment(
            this,
            "DeploymentOfRest",
            BucketDeploymentProps(
                DestinationBucket = bucket,
                Sources = [| Source.Asset("./bundle/client/", AssetOptions(Exclude = [| "*.html" |])) |],
                CacheControl = [| CacheControl.FromString "max-age=31536000,public,immutable" |],
                Prune = false
            )
        )
        |> ignore

    let vpc = Vpc(this, "OpenQuizVpc", VpcProps(NatGateways = 0., MaxAzs = 2.))

    let apiBundle =
        Asset(this, "ApiBundle", AssetProps(Path = "./bundle/openquiz-api.zip"))

    let apiApp =
        CfnApplication(this, "ApiApp", CfnApplicationProps(ApplicationName = sprintf "OpenQuiz-%s" env))

    let apiAppRoleName = sprintf "open-quiz-api-handler-%s" globalId

    let apiAppRole =
        Role(this, "ApiAppRole", RoleProps(RoleName = apiAppRoleName, AssumedBy = ServicePrincipal "ec2"))

    let instanceProfile =
        CfnInstanceProfile(
            this,
            "ApiInstanceProfile",
            CfnInstanceProfileProps(InstanceProfileName = apiAppRoleName, Roles = [| apiAppRole.RoleName |])
        )

    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AmazonS3FullAccess")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "CloudWatchAgentServerPolicy")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AWSAppSyncInvokeFullAccess")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AmazonDynamoDBFullAccess")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "CloudWatchLogsFullAccess")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AmazonSSMFullAccess")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AmazonEC2ContainerRegistryReadOnly")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AWSElasticBeanstalkWebTier")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AWSElasticBeanstalkMulticontainerDocker")
    do apiAppRole.AddManagedPolicy(ManagedPolicy.FromAwsManagedPolicyName "AWSElasticBeanstalkWorkerTier")

    let subnets =
        vpc.SelectSubnets(SubnetSelection(SubnetType = SubnetType.PUBLIC)).SubnetIds
        |> String.concat ","

    let optionSettingProperties =
        [| CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:elasticbeanstalk:environment",
               OptionName = "EnvironmentType",
               Value = "SingleInstance"
           )
           CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:autoscaling:launchconfiguration",
               OptionName = "InstanceType",
               Value = "t2.micro"
           )
           CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:autoscaling:launchconfiguration",
               OptionName = "IamInstanceProfile",
               Value = instanceProfile.InstanceProfileName
           )
           CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "VPCId", Value = vpc.VpcId)
           CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:ec2:vpc",
               OptionName = "AssociatePublicIpAddress",
               Value = "true"
           )
           CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "Subnets", Value = subnets)
           CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "ELBScheme", Value = "public")
           CfnEnvironment.OptionSettingProperty(Namespace = "aws:ec2:vpc", OptionName = "ELBSubnets", Value = subnets)
           // Application environment variables exposed to the app process
           CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:elasticbeanstalk:application:environment",
               OptionName = "ASPNETCORE_ENVIRONMENT",
               Value = env
           )
           // Optional: also set DOTNET_ENVIRONMENT for Generic Host parity
           CfnEnvironment.OptionSettingProperty(
               Namespace = "aws:elasticbeanstalk:application:environment",
               OptionName = "DOTNET_ENVIRONMENT",
               Value = env
           ) |]

    let appVersion =
        CfnApplicationVersion(
            this,
            "ApiAppVersion",
            CfnApplicationVersionProps(
                ApplicationName = apiApp.ApplicationName,
                SourceBundle =
                    CfnApplicationVersion.SourceBundleProperty(
                        S3Bucket = apiBundle.S3BucketName,
                        S3Key = apiBundle.S3ObjectKey
                    )
            )
        )

    let version = "2025v4"
    let cnamePrefix = sprintf "openquiz-%s-%s-v%s" (env.ToLower()) globalId version

    let appEnv =
        CfnEnvironment(
            this,
            "ApiAppEnv",
            CfnEnvironmentProps(
                EnvironmentName = apiApp.ApplicationName + "-SingleV" + version,
                ApplicationName = apiApp.ApplicationName,
                SolutionStackName = "64bit Amazon Linux 2023 v3.5.3 running .NET 8",
                OptionSettings = optionSettingProperties,
                CnamePrefix = cnamePrefix,
                VersionLabel = appVersion.Ref
            )
        )

    do appVersion.AddDependency(apiApp)

    let apiEnvCname = sprintf "%s.%s.elasticbeanstalk.com" cnamePrefix this.Region

    let s3BundleOrigin =
        S3BucketOrigin.WithBucketDefaults(bucket, OriginProps(OriginId = "s3-bundle"))

    let s3MediaOrigin =
        S3BucketOrigin.WithBucketDefaults(bucket, OriginProps(OriginId = "s3-media"))

    let apiOrigin =
        S3BucketOrigin.WithBucketDefaults(bucket, OriginProps(OriginId = "api-origin"))
    // HttpOrigin(
    //     apiEnvCname,
    //     HttpOriginProps(OriginId = "api-origin", ProtocolPolicy = OriginProtocolPolicy.HTTP_ONLY)
    // )

    let distribution =
        Distribution(
            this,
            "OpenQuizWebDistribution",
            DistributionProps(
                DefaultBehavior =
                    BehaviorOptions(
                        Origin = s3BundleOrigin,
                        ViewerProtocolPolicy = ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
                        AllowedMethods = AllowedMethods.ALLOW_GET_HEAD_OPTIONS
                    ),
                AdditionalBehaviors =
                    dict
                        [ "/static/*",
                          BehaviorOptions(
                              Origin = s3MediaOrigin,
                              ViewerProtocolPolicy = ViewerProtocolPolicy.ALLOW_ALL,
                              AllowedMethods = AllowedMethods.ALLOW_GET_HEAD_OPTIONS,
                              CachePolicy = CachePolicy.CACHING_DISABLED
                          )
                          "/media/*",
                          BehaviorOptions(
                              Origin = s3MediaOrigin,
                              ViewerProtocolPolicy = ViewerProtocolPolicy.ALLOW_ALL,
                              AllowedMethods = AllowedMethods.ALLOW_GET_HEAD_OPTIONS,
                              CachePolicy = CachePolicy.ELEMENTAL_MEDIA_PACKAGE
                          )
                          "/api/*",
                          BehaviorOptions(
                              // BUG: should be apiOrigin but getting error: "Non-allowlisted account trying to use IPAddressType feature"
                              // Replace manually in the AWS console
                              Origin = apiOrigin,
                              ViewerProtocolPolicy = ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
                              AllowedMethods = AllowedMethods.ALLOW_ALL,
                              CachePolicy = CachePolicy.CACHING_DISABLED
                          ) ]
            )
        )

    let cloudFrontUrl = "https://" + distribution.DomainName
    do Helpers.createParameter this env "BucketUrl" cloudFrontUrl

    do
        CfnOutput(
            this,
            "OpenQuizUrl",
            CfnOutputProps(Value = cloudFrontUrl, Description = "Alias your domain name with the Url")
        )
        |> ignore

    do
        CfnOutput(
            this,
            "ApiUrl",
            CfnOutputProps(Value = apiEnvCname, Description = "API Environment CNAME for manual replacement")
        )
        |> ignore

    let userPool =
        Assets.createUserPool this env globalId (cloudFrontUrl + "/app/index.html")

    do Assets.createAppsyncApi this env