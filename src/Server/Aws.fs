module OpenQuiz.Aws

open System
open System.Collections.Generic
open System.Net.Http
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.IO
open Amazon.S3;
open Amazon.S3.Model;

open OpenQuiz.Shared
open OpenQuiz.Common
open OpenQuiz.Env

let private httpClient = new HttpClient()

let getUserToken env code =

    async {
        let uri = sprintf "%s/oauth2/token" (env:>ICfg).Configurer.LoginUrl

        let values = new Dictionary<string, string>()
        values.Add("grant_type", "authorization_code")
        values.Add("client_id", env.Configurer.UserPoolClientId)
        values.Add("code", code)
        values.Add("redirect_uri", env.Configurer.AppUrl)
        let content = new FormUrlEncodedContent(values);

        let! resp = httpClient.PostAsync(uri, content) |> Async.AwaitTask
        let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

        match resp.StatusCode with
        | Net.HttpStatusCode.OK ->
            let json = JObject.Parse respStr

            let res = {|
                IdToken = json.GetValue("id_token").ToString()
                AccessToken = json.GetValue("access_token").ToString()
                RefreshToken = json.GetValue("refresh_token").ToString()
            |}

            return Ok res
        | _ ->
            (env:>ILog).Logger.Error ("{@Op} {@Status} {@Resp}", "Aws.getUserToken", resp.StatusCode,  respStr)
            return Error (resp.StatusCode.ToString())
    }

let getUserInfo env accessToken =

    async {
        let uri = sprintf "%s/oauth2/userInfo"  (env:>ICfg).Configurer.LoginUrl
        let httpReq = new HttpRequestMessage(HttpMethod.Get, uri)
        httpReq.Headers.Add ("Authorization", sprintf "Bearer %s" accessToken)

        let! resp = httpClient.SendAsync(httpReq) |> Async.AwaitTask
        let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

        match resp.StatusCode with
        | Net.HttpStatusCode.OK ->
            let json = JObject.Parse respStr

            let username = json.GetValue("username").ToString()

            let picture =
                match json.GetValue("picture") with
                | null -> None
                | jtoken ->
                    let picture = jtoken.ToString();
                    match username with
                    | _ when not (String.IsNullOrEmpty picture) && username.Contains ("facebook") ->
                        let picJson = JObject.Parse picture
                        picJson.["data"].["url"].ToString()
                    | _ -> picture
                    |> Some

            let name =
                match json.GetValue("name") with
                | null -> username
                | jtoken -> jtoken.ToString()

            let res = {|
                Sub = json.GetValue("sub").ToString()
                Username = username
                Name = name
                Picture = picture
            |}

            return Ok res
        | _ ->
            (env:>ILog).Logger.Error ("{@Op} {@Status} {@Resp}", "Aws.getUserToken", resp.StatusCode,  respStr)
            return Error (resp.StatusCode.ToString())

    }

let private publishQuery = sprintf """{ "query": "mutation quizMessage {quizMessage(quizId: %i, token: \"%s\", body: \"%s\", version: %i){quizId, token, body, version} }"}"""

let publishQuizMessage env quizId token version evt =
    let appSyncCfg = (env:>ICfg).Configurer.AppSyncCfg
    printfn "PUBLISH TO %A" appSyncCfg
    async {
        try
            let! creds = Amazon.Runtime.FallbackCredentialsFactory.GetCredentials().GetCredentialsAsync() |> Async.AwaitTask

            let signer = new Aws4RequestSigner.AWS4RequestSigner(creds.AccessKey, creds.SecretKey)

            let body =
                JsonConvert.SerializeObject(evt,fableConverter)
                |> Text.UTF8Encoding.UTF8.GetBytes
                |> Convert.ToBase64String

            let data = publishQuery quizId token body version
            printfn "QUERY: %s" data

            let content = new StringContent(data, Text.Encoding.UTF8, "application/graphql")

            let origReq = new HttpRequestMessage(HttpMethod.Post, appSyncCfg.Endpoint, Content = content)

            let! signedReq = signer.Sign(origReq, "appsync", appSyncCfg.Region) |> Async.AwaitTask

            if creds.UseToken then
                signedReq.Headers.Add("X-Amz-Security-Token", creds.Token)

            let! resp = httpClient.SendAsync(signedReq) |> Async.AwaitTask
            let! respStr = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            match resp.StatusCode with
            | Net.HttpStatusCode.OK -> return ()
            | _ -> return (env:>ILog).Logger.Error ("{@Op} {@Status} {@Resp}", "Aws.appsync", resp.StatusCode,  respStr)
        with
        | ex -> return (env:>ILog).Logger.Error ("{@Op} {@Exception}", "Aws.appsync", ex)
    }

let getResultsKey quizId token =
    sprintf "static/%d-%s/results.json" quizId token

let getSignedUrl bucketName (cat:MediaCategory) =
    use client = new AmazonS3Client(AmazonS3Config(UseAccelerateEndpoint = false)) // acceletation is disabled until an accelerate endpoint is added throught cdk
    let key = sprintf "%s/%.0f-%s" cat.Prefix (toEpoch DateTime.UtcNow) (generateRandomToken())
    key,
    client.GetPreSignedURL(
        GetPreSignedUrlRequest(
            BucketName = bucketName,
            Key = Infra.s3KeyForMedia key,
            Verb = HttpVerb.PUT,
            Expires = System.DateTime.UtcNow.AddDays(1.0)))

let uploadFile env (key:string) (fileType:string) (fileBody : byte[]) : Async<Result<unit, string>> =

    use client = new AmazonS3Client()

    let req =
        PutObjectRequest (
            BucketName = (env:>ICfg).Configurer.BucketName,
            Key = key,
            InputStream = new MemoryStream(fileBody),
            ContentType = fileType)

    client.PutObjectAsync(req)
    |> Async.AwaitTask
    |> Async.Catch
    |> Async.map (
        function
        | Choice1Of2 resp ->
            if resp.HttpStatusCode = System.Net.HttpStatusCode.OK then Ok ()
            else Error <| resp.HttpStatusCode.ToString()
        | Choice2Of2 ex ->
            (env:>ILog).Logger.Error ("{@Proc} {@Exception}", "BUCKET", ex)
            Error ex.Message)

let deleteFile env (key:string) : Async<Result<unit, string>> =

    use client = new AmazonS3Client()

    let req = DeleteObjectRequest (BucketName = (env:>ICfg).Configurer.BucketName, Key = key)

    client.DeleteObjectAsync(req)
    |> Async.AwaitTask
    |> Async.Catch
    |> Async.map (
        function
        | Choice1Of2 resp ->
            if resp.HttpStatusCode = System.Net.HttpStatusCode.OK then Ok ()
            else Error <| resp.HttpStatusCode.ToString()
        | Choice2Of2 ex ->
            (env :> ILog).Logger.Error ("{@Proc} {@Exception}", "BUCKET", ex)
            Error ex.Message)