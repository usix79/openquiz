module Bucket

open Amazon.S3;
open Amazon.S3.Model;

open Shared
open Common

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