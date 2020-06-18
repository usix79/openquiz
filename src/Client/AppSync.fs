// fsharplint:disable
module AppSync
    open Fable.Core
    open Fable.SimpleJson

    type QuizMessage =
        //abstract quizId: int with get, set
        //abstract token: string with get, set
        abstract body: string with get, set
        //abstract version: int with get, set

    type SubError =
        abstract message: string with get, set

    type IAppSync =
        abstract configure: endpoint:string*region:string*apikey:string -> unit
        abstract subscribe: quizId:int*token:string*onSuccess:(QuizMessage->unit)*onError:(SubError array -> unit) -> int
        abstract unsubscribe: int -> unit

    [<ImportAll("./appsync.js")>]
    let private appSync: IAppSync = jsNative

    let configure endpoint region apikey =
        appSync.configure (endpoint, region, apikey)

    let mutable subscriptionId : int option = None

    let subscribe quizId token onSuccess onError=
        let sid =
            appSync.subscribe(quizId, token,
                (fun msg ->
                    let body = msg.body |> System.Convert.FromBase64String |> System.Text.Encoding.UTF8.GetString
                    let evt = Json.parseAs<Shared.QuizChangedEvent> body
                    onSuccess evt),
                (fun errors -> errors |> Array.map (fun err -> err.message) |> onError )
            )
        subscriptionId <- Some sid

    let unsubscribe () =
        match subscriptionId with
        | Some sid ->
            appSync.unsubscribe sid
            subscriptionId <- None
        | None -> ()
