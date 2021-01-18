open System
open Amazon.CDK
open OpenQuiz.Cdk

[<EntryPoint>]
let main _ =
    let app = App(null)
    let props =
        StackProps(
            Env = Environment(
                Account = Environment.GetEnvironmentVariable("CDK_DEFAULT_ACCOUNT"),
                Region = Environment.GetEnvironmentVariable("CDK_DEFAULT_REGION"))
    )
    DevelopmentStack(Development, app, "OpenQuiz", props) |> ignore
    app.Synth() |> ignore
    0