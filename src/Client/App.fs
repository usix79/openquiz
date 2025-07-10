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
|> Program.withReactBatched "elmish-app"
// Temporarily disable debugger to fix socket connection error
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run