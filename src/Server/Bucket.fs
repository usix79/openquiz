module Bucket

open FSharp.Control.Tasks.ContextInsensitive
open System.IO
open Amazon.S3;
open Amazon.S3.Model;

open Shared

let prefix = "pub/"

let uploadFile bucketName (cat:ImgCategory) (fileType:string) (fileBody : byte[]) : Result<{|BucketKey:string|}, string> =
    use client = new AmazonS3Client()
    let key = cat.Prefix + "/" + Common.generateRandomToken().Replace("/", "_").Replace("+", "-")
    use stream = new MemoryStream(fileBody)

    let req =
        PutObjectRequest (
            BucketName = bucketName,
            Key = prefix + key,
            InputStream = stream,
            ContentType = fileType
        )
    try
        let resp = client.PutObjectAsync(req).Result

        if (resp.HttpStatusCode = System.Net.HttpStatusCode.OK) then
            Ok {|BucketKey = key|}
        else
            Error <| resp.HttpStatusCode.ToString()
    with
    | ex ->
        printfn "EXCEPTION %A" ex
        Error ex.Message

let downloadFile buketName key =
    task {
        use client = new AmazonS3Client()
        let req =
            GetObjectRequest(
                BucketName = buketName,
                Key = prefix + key
            )
        let! resp = client.GetObjectAsync req

        return {|ContentType = resp.Headers.["Content-Type"]; Body = resp.ResponseStream|}
    }