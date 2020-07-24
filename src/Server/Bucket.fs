module Bucket

open System.IO
open Amazon.S3;
open Amazon.S3.Model;
open Serilog

open Shared
open Common

let getResultsKey quizId token =
    sprintf "static/%d-%s/results.json" quizId token

let getSignedUrl bucketName (cat:MediaCategory) =
    use client = new AmazonS3Client(AmazonS3Config(UseAccelerateEndpoint = true))
    let key = sprintf "%s/%.0f-%s" cat.Prefix (toEpoch System.DateTime.UtcNow) (Common.generateRandomToken())
    key,
    client.GetPreSignedURL(
        GetPreSignedUrlRequest(
            BucketName = bucketName,
            Key = Infra.s3KeyForMedia key,
            Verb = HttpVerb.PUT,
            Expires = System.DateTime.UtcNow.AddDays(1.0)))

let uploadFile bucketName (key:string) (fileType:string) (fileBody : byte[]) : Async<Result<unit, string>> =

    use client = new AmazonS3Client()

    let req =
        PutObjectRequest (
            BucketName = bucketName,
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
            Log.Logger.Error ("{@Proc} {@Exception}", "BUCKET", ex)
            Error ex.Message)

let deleteFile bucketName (key:string) : Async<Result<unit, string>> =

    use client = new AmazonS3Client()

    let req = DeleteObjectRequest (BucketName = bucketName, Key = key)

    client.DeleteObjectAsync(req)
    |> Async.AwaitTask
    |> Async.Catch
    |> Async.map (
        function
        | Choice1Of2 resp ->
            if resp.HttpStatusCode = System.Net.HttpStatusCode.OK then Ok ()
            else Error <| resp.HttpStatusCode.ToString()
        | Choice2Of2 ex ->
            Log.Logger.Error ("{@Proc} {@Exception}", "BUCKET", ex)
            Error ex.Message)