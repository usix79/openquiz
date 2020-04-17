// fsharplint:disable
module EventSource

open Fable.Core
open Browser.Types

[<RequireQualifiedAccess>]
type ReadyState =
    | CONNECTING = 0
    | OPEN = 1
    | CLOSED = 2

[<AllowNullLiteral>]
type IEventSource =
    abstract url: string
    abstract readyState: ReadyState
    abstract withCredentials: bool
    abstract onopen: (MessageEvent -> Unit) with get, set
    abstract onmessage: (MessageEvent -> Unit) with get, set
    abstract onerror: (Event -> Unit) with get, set
    abstract addEventListener: ``type``: string * listener: (MessageEvent -> Unit) -> unit
    abstract dispatchEvent: evt: Event -> bool
    abstract removeEventListener: ``type``: string * ?listener: (MessageEvent -> Unit) -> unit
    abstract close: unit -> unit

[<AllowNullLiteral>]
type EventSourceInitDict =
    abstract withCredentials: bool option with get, set
    abstract headers: obj option with get, set
    abstract proxy: string option with get, set
    abstract https: obj option with get, set
    abstract rejectUnauthorized: bool option with get, set

[<AllowNullLiteral>]
type EventSource=
    [<Emit "new EventSource($1...)">]
    abstract Create: url: string * ?eventSourceInitDict: EventSourceInitDict -> IEventSource

// TODO: this feels like an hack but alas not soure how else I can do it 😎😎
// NOTE: createEmpty<EventSource.EventSource>.Create does the same but
[<Emit("")>]
let  eventSource: EventSource = jsNative
