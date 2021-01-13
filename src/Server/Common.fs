module Common

open System
open System.Security.Cryptography

open Fable.Remoting.Server
open Fable.Remoting.Json

open Shared

let NYI = Error "Not Yet Implemented"

type ARES<'Value> = Async<Result<'Value, string>>

let executedResponse f req =
    async{
        let! v = f req.Token req.Arg
        return {Status = Executed; Value = v; ST = DateTime.UtcNow}
    }

let generateRandomToken'() =
    let randomNumber =  Array.zeroCreate 32
    use rng = RandomNumberGenerator.Create()
    rng.GetBytes(randomNumber)
    System.Convert.ToBase64String(randomNumber)

let generateRandomToken() =
    generateRandomToken'()
        .Replace("/", "_")
        .Replace("+", "-")

let tryParseInt32 (str:string) =
    match System.Int32.TryParse(str) with
    | true, n -> Some n
    | _ -> None

let toEpoch dt =
    (dt - DateTime.UnixEpoch).TotalSeconds
    |> Convert.ToInt64
    |> Convert.ToDecimal

let fromEpoch (epoch:decimal) =
    epoch
    |> Convert.ToDouble
    |> DateTime.UnixEpoch.AddSeconds

let trimEnd n suffix (str:string) =
    if str.Length < n then str
    else
       str.Substring(0, n) + suffix

let trimMiddle n suffix (str:string) =
    if str.Length < n then str
    else
       str.Substring(0, n / 2) + suffix + str.Substring(str.Length - (n / 2), n / 2)

let fableConverter = FableJsonConverter()

module Result =
    let toOption = function
    | Ok entity -> Some entity
    | Error _ -> None

    let fromOption error = function
    | Some entity -> Ok entity
    | None -> Error error

module Async =

    let retn x = async { return x }

    let map f m =
        async {
            let! x = m
            return f x
        }

    let bind f m =
        async {
            let! x = m
            return! f x
        }

    let ParallelThrottle throttle workflows =
            Async.Parallel(workflows, throttle)

module AsyncResult =

    let fromResult x = async { return x }

    let retn x = async { return Ok x }

    let map f m =
        async {
            match! m with
            | Ok a -> return Ok(f a)
            | Error e -> return Error e
        }

    let mapError f m =
        async {
            match! m with
            | Ok a -> return Ok a
            | Error e -> return Error (f e)
        }

    let ifError f m =
        async {
            match! m with
            | Ok a -> return Ok a
            | Error e -> return Ok (f e)
        }

    let bind f m =
        async {
            match! m with
            | Ok a -> return! f a
            | Error e -> return Error e
        }

    let side f m =
        async {
            match! m with
            | Ok a ->
                match! f a with
                | Ok _ -> return Ok a
                | Error e -> return Error e
            | Error e -> return Error e
        }

    let sideIf p f m =
        if p then side f m else m

    let sideRes f m =
        async {
            match! m with
            | Ok a ->
                match f a with
                | Ok _ -> return Ok a
                | Error e -> return Error e
            | Error e -> return Error e
        }

    let next mn m =
        bind (fun _ -> mn) m

type PublisherCommand =
    | PublishResults of quizId : int


let ofOption error = function Some s -> Ok s | None -> Error error

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()

module Diag =
    let status () =
        printfn "THREADS:%i WI(%i %i) MEMORY:%i Heap:%i GCC(%i %i %i) AllockRage:%i  MI(%i %i %i %i %i) Locks:%i"
            Threading.ThreadPool.ThreadCount
            Threading.ThreadPool.PendingWorkItemCount
            Threading.ThreadPool.CompletedWorkItemCount
            (Environment.WorkingSet / 1000000L)
            (GC.GetTotalMemory(false) / 1000000L)
            (GC.CollectionCount(0))
            (GC.CollectionCount(1))
            (GC.CollectionCount(2))
            (GC.GetTotalAllocatedBytes(false))
            (GC.GetGCMemoryInfo().FragmentedBytes)
            (GC.GetGCMemoryInfo().HeapSizeBytes)
            (GC.GetGCMemoryInfo().HighMemoryLoadThresholdBytes)
            (GC.GetGCMemoryInfo().MemoryLoadBytes)
            (GC.GetGCMemoryInfo().TotalAvailableMemoryBytes)
            (Threading.Monitor.LockContentionCount)

    let run command =
        try
            use proc = new Diagnostics.Process()
            proc.StartInfo.FileName <- "/bin/sh"
            proc.StartInfo.Arguments <- "-c \" " + command + " \""
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardOutput <- true
            proc.StartInfo.RedirectStandardError <- true
            proc.Start() |> ignore

            let output =
                sprintf "%s\n%s"
                    (proc.StandardOutput.ReadToEnd())
                    (proc.StandardError.ReadToEnd())

            proc.WaitForExit();

            output
        with
        | ex -> ex.ToString()
