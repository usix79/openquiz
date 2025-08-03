open System
open Amazon.CDK
open Constructs
open OpenQuiz.Cdk

[<EntryPoint>]
let main _ =
    let app = App(null)
    let globalId = app.Node.TryGetContext "globalId" :?> string
    DevelopmentStack(app, "OpenQuiz-Development", StackProps(), globalId) |> ignore

    ProductionStack(app, "OpenQuiz-Stage", StackProps(), "Stage", globalId)
    |> ignore

    ProductionStack(app, "OpenQuiz-Production", StackProps(), "Production", globalId)
    |> ignore

    Tags.Of(app).Add("Product", "OpenQuiz")
    app.Synth() |> ignore
    0