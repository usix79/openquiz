open System
open Amazon.CDK
open OpenQuiz.Cdk

[<EntryPoint>]
let main _ =
    let app = App(null)
    let globalId = app.Node.TryGetContext("globalId") :?> string
    DevelopmentStack(app, "OpenQuiz-Development", StackProps(), globalId) |> ignore
    ProductionStack(app, "OpenQuiz-Production", StackProps(), globalId) |> ignore
    app.Synth() |> ignore
    0
