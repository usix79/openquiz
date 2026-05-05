[<RequireQualifiedAccess>]
module ReactPlayer

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fable.Core

[<Emit("$2[$0] = $1")>]
let private setProp (propName: string) (propValue: obj) (any: obj) : unit = jsNative

let playerEx isVideo (isPlaying: bool) (src: string) =
    let props = obj ()

    // react-player v3 uses `src` prop (legacy `url` prop removed)
    setProp "src" src props
    setProp "controls" true props
    setProp "playing" isPlaying props

    if isVideo then
        setProp "height" "100%" props
    else
        setProp "height" "64px" props

    setProp "width" "100%" props

    ofImport "default" "react-player" props []

let player = playerEx true false