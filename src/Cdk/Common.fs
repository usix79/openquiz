namespace OpenQuiz.Cdk

open Amazon.CDK
open Amazon.CDK.AWS.DynamoDB
open Amazon.CDK.AWS.S3
open Amazon.CDK.AWS.SSM
open Amazon.CDK.AWS.Cognito
open Amazon.CDK.AWS.AppSync
open Amazon.CDK.AWS.IAM

module Helpers =

    let createTable' stack env tableName partitionKeyName partitionKeyType sortKey ttl =
        Table(
            stack,
            tableName,
            TableProps(
                TableName = sprintf "OpenQuiz-%s-%s" env tableName,
                PartitionKey = Attribute(Name = partitionKeyName, Type = partitionKeyType),
                SortKey =
                    (match sortKey with
                     | Some(name, type') -> Attribute(Name = name, Type = type')
                     | None -> null),
                BillingMode = BillingMode.PAY_PER_REQUEST,
                TimeToLiveAttribute =
                    match ttl with
                    | Some name -> name
                    | _ -> null
            )
        )
        |> ignore

    let createTable stack env tableName partitionKeyName partitionKeyType =
        createTable' stack env tableName partitionKeyName partitionKeyType None None

    let createTableWithTTL stack env tableName partitionKeyName partitionKeyType ttl =
        createTable' stack env tableName partitionKeyName partitionKeyType None (Some ttl)

    let createTableWithSortKey stack env tableName partitionKeyName partitionKeyType sortKeyName sortKeyType =
        createTable' stack env tableName partitionKeyName partitionKeyType (Some(sortKeyName, sortKeyType)) None

    let createParameter stack env name value' =
        let fullName = sprintf "/OpenQuiz/%s/%s" env name
        let props = StringParameterProps(ParameterName = fullName, StringValue = value')

        StringParameter(stack, fullName, props) |> ignore

open Helpers

module Assets =

    let createDynamoDBTables stack env =
        createTable stack env "System" "Id" AttributeType.STRING
        createTable stack env "Experts" "Id" AttributeType.STRING
        createTable stack env "Packages" "Id" AttributeType.NUMBER
        createTable stack env "Quizzes" "Id" AttributeType.NUMBER
        createTableWithSortKey stack env "Teams" "QuizId" AttributeType.NUMBER "TeamId" AttributeType.NUMBER
        createTableWithTTL stack env "Tokens" "Token" AttributeType.STRING "TTL"

    let createBucket stack env =
        let bucketProps =
            BucketProps(
                WebsiteIndexDocument = "index.html",
                WebsiteErrorDocument = "error.html",
                PublicReadAccess = true,
                BlockPublicAccess =
                    BlockPublicAccess(
                        BlockPublicAccessOptions(
                            BlockPublicAcls = false,
                            IgnorePublicAcls = false,
                            BlockPublicPolicy = false,
                            RestrictPublicBuckets = false
                        )
                    ),
                Cors =
                    [| CorsRule(
                           AllowedMethods = [| HttpMethods.GET; HttpMethods.PUT |],
                           AllowedOrigins = [| "*" |],
                           AllowedHeaders = [| "*" |]
                       ) |]
            )

        let bucket = Bucket(stack, "Bucket", bucketProps)

        let publicAccessPolicy = PolicyStatement()
        publicAccessPolicy.Effect <- Effect.ALLOW
        publicAccessPolicy.AddActions "s3:GetObject"
        publicAccessPolicy.AddResources(sprintf "arn:aws:s3:::%s/*" bucket.BucketName)
        publicAccessPolicy.AddPrincipals(AnyPrincipal()) |> ignore

        bucket.AddToResourcePolicy(publicAccessPolicy) |> ignore

        createParameter stack env "BucketName" bucket.BucketName
        bucket

    let createUserPool stack env globalId appUrl =
        let userPool =
            UserPool(
                stack,
                "UserPool",
                UserPoolProps(
                    UserPoolName = sprintf "OpenQuiz-%s" env,
                    SignInCaseSensitive = false,
                    AutoVerify = AutoVerifiedAttrs(Email = true),
                    SignInAliases = SignInAliases(Username = true, Email = true),
                    StandardAttributes = StandardAttributes(Email = StandardAttribute(Required = true)),
                    PasswordPolicy =
                        PasswordPolicy(
                            MinLength = 6.,
                            RequireDigits = false,
                            RequireLowercase = false,
                            RequireSymbols = false,
                            RequireUppercase = false
                        ),
                    SelfSignUpEnabled = true
                )
            )

        let userPoolDomain =
            userPool.AddDomain(
                "UserPoolDomain",
                UserPoolDomainOptions(
                    CognitoDomain =
                        CognitoDomainOptions(DomainPrefix = sprintf "openquiz-%s-%s" (env.ToLower()) globalId)
                )
            )

        let appClientProps =
            UserPoolClientProps(
                UserPool = userPool,
                AuthFlows = AuthFlow(UserPassword = true),
                OAuth =
                    OAuthSettings(
                        Flows = OAuthFlows(AuthorizationCodeGrant = true, ImplicitCodeGrant = true),
                        Scopes = [| OAuthScope.EMAIL; OAuthScope.OPENID; OAuthScope.PROFILE |],
                        CallbackUrls = [| appUrl |]
                    ),
                SupportedIdentityProviders = [| UserPoolClientIdentityProvider.COGNITO |],
                UserPoolClientName = sprintf "OpenQuiz-%s-Client" env
            )

        let appClient = UserPoolClient(stack, "OpenQuizUserPoolClient", appClientProps)
        do createParameter stack env "AppUrl" appUrl
        do createParameter stack env "LoginUrl" <| userPoolDomain.BaseUrl()
        do createParameter stack env "UserPoolClientId" appClient.UserPoolClientId

        userPool

    let createAppsyncApi stack env =
        let api =
            GraphqlApi(
                stack,
                "Api",
                GraphqlApiProps(
                    Name = sprintf "OpenQuiz-%s-API" env,
                    Schema = SchemaFile.FromAsset("./aws/schema.graphql"),
                    AuthorizationConfig =
                        AuthorizationConfig(
                            DefaultAuthorization = AuthorizationMode(AuthorizationType = AuthorizationType.IAM),
                            AdditionalAuthorizationModes =
                                [| AuthorizationMode(
                                       AuthorizationType = AuthorizationType.API_KEY,
                                       ApiKeyConfig =
                                           ApiKeyConfig(
                                               Name = "MainApiKey",
                                               Expires = Expiration.AtDate(System.DateTime.UtcNow.AddDays(365.))
                                           )
                                   ) |]
                        )
                )
            )

        let ds = NoneDataSource(stack, "DummyDataSource", NoneDataSourceProps(Api = api))

        do
            ds.CreateResolver(
                "quizMessageResolver",
                BaseResolverProps(
                    TypeName = "Mutation",
                    FieldName = "quizMessage",
                    RequestMappingTemplate =
                        MappingTemplate.FromString(
                            """
    #**
    Resolvers with None data sources can locally publish events that fire
    subscriptions or otherwise transform data without hitting a backend data source.
    The value of 'payload' is forwarded to $ctx.result in the response mapping template.
    *#
    {
        "version": "2017-02-28",
        "payload": {
            "quizId": $util.toJson($context.arguments.quizId),
            "token": $util.toJson($context.arguments.token),
            "body" : $util.toJson($context.arguments.body),
            "version": $util.toJson($context.arguments.version),
        }
    }"""
                        ),
                    ResponseMappingTemplate = MappingTemplate.FromString("$util.toJson($context.result)")
                )
            )
            |> ignore

        do createParameter stack env "AppsyncEndpoint" api.GraphqlUrl
        do createParameter stack env "AppsyncRegion" api.Env.Region
        do createParameter stack env "AppsyncApiKey" api.ApiKey