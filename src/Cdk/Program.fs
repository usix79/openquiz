open Amazon.CDK
open OpenQuiz.Cdk

[<EntryPoint>]
let main _ =
    let app = App(null)
    DevelopmentStack(Development, app, "OpenQuiz", StackProps()) |> ignore
    app.Synth() |> ignore
    0