module OpenQuiz.App

open Elmish
open Elmish.React
open Elmish.Navigation

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Root.init Root.update Root.view
#if DEBUG
|> Program.withConsoleTrace
#endif
// React 18+ already batches state updates by default; using the synchronous variant simplifies integration
|> Program.withReactSynchronous "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run